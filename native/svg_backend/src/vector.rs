//! Native geometry and selected CID text over the raster adapter's usvg tree.
//! Authorization and supported input semantics belong to the Elixir validators.
use pdf_writer::types::{
    ActionType, AnnotationType, CidFontType, FontFlags, FunctionShadingType, MaskType, PaintType,
    SystemInfo, TilingType, UnicodeCmap,
};
use pdf_writer::{Content, Finish, Name, Pdf, Rect, Ref, Str, TextStr};
use std::collections::HashMap;
use usvg::tiny_skia_path::{Path, PathSegment};

#[derive(Default)]
struct Resources {
    forms: Vec<(String, Ref)>,
    states: Vec<(String, Ref)>,
    shadings: Vec<(String, Ref)>,
    fonts: Vec<(String, Ref)>,
    patterns: Vec<(String, Ref)>,
}

impl Resources {
    fn write(&self, mut resources: pdf_writer::writers::Resources<'_>) {
        let mut objects = resources.x_objects();
        for (name, id) in &self.forms {
            objects.pair(Name(name.as_bytes()), *id);
        }
        objects.finish();
        let mut states = resources.ext_g_states();
        for (name, id) in &self.states {
            states.pair(Name(name.as_bytes()), *id);
        }
        states.finish();
        let mut shadings = resources.shadings();
        for (name, id) in &self.shadings {
            shadings.pair(Name(name.as_bytes()), *id);
        }
        shadings.finish();
        let mut patterns = resources.patterns();
        for (name, id) in &self.patterns {
            patterns.pair(Name(name.as_bytes()), *id);
        }
        patterns.finish();
        if !self.fonts.is_empty() {
            let mut fonts = resources.fonts();
            for (name, id) in &self.fonts {
                fonts.pair(Name(name.as_bytes()), *id);
            }
            fonts.finish();
        }
    }
}

pub struct HtmlSurface {
    pub width: f32,
    pub height: f32,
    pub clusters: Vec<(String, [f32; 4])>,
}

struct Writer<'a> {
    pdf: Pdf,
    next: i32,
    object_ceiling: u64,
    database: &'a usvg::fontdb::Database,
    transform: usvg::Transform,
    // Pattern matrices are relative to the current PDF stream's initial CTM.
    // Transparency forms establish a new origin while SVG geometry keeps its
    // full scene transform for clips, links and selected-glyph positions.
    pattern_transform: usvg::Transform,
    luminance: bool,
    effect_scale: (f32, f32),
    slot_probe: bool,
    html_surfaces: HashMap<String, HtmlSurface>,
    slots: std::collections::HashSet<String>,
    clip: Option<[f32; 4]>,
    fonts: HashMap<usvg::fontdb::ID, (String, Ref, u16)>,
    mappings: HashMap<usvg::fontdb::ID, std::collections::BTreeMap<u16, String>>,
    clusters: HashMap<usize, Vec<crate::vector_validator::Cluster<'a>>>,
}

impl Writer<'_> {
    fn reference(&mut self) -> Result<Ref, String> {
        if self.next as u64 > self.object_ceiling {
            return Err("max_pdf_objects".into());
        }
        let id = Ref::new(self.next);
        self.next += 1;
        Ok(id)
    }

    fn opacity(
        &mut self,
        opacity: f32,
        content: &mut Content,
        resources: &mut Resources,
    ) -> Result<(), String> {
        if opacity != 1.0 {
            let id = self.reference()?;
            self.pdf.ext_graphics(id).non_stroking_alpha(opacity);
            let name = format!("A{}", id.get());
            content.set_parameters(Name(name.as_bytes()));
            resources.states.push((name, id));
        }
        Ok(())
    }

    fn group(
        &mut self,
        group: &usvg::Group,
        content: &mut Content,
        resources: &mut Resources,
    ) -> Result<(), String> {
        if group.opacity().get() == 0.0 {
            return Ok(());
        }
        content.save_state();
        let t = group.transform();
        let previous = self.transform;
        let previous_pattern = self.pattern_transform;
        let previous_clip = self.clip;
        self.transform = previous.pre_concat(t);
        self.pattern_transform = previous_pattern.pre_concat(t);
        if let Some(clip) = group.clip_path() {
            if let Some(bounds) = clip_bounds(clip, self.transform) {
                self.clip = Some(match previous_clip {
                    Some(parent) => [
                        parent[0].max(bounds[0]),
                        parent[1].max(bounds[1]),
                        parent[2].min(bounds[2]),
                        parent[3].min(bounds[3]),
                    ],
                    None => bounds,
                });
            }
        }
        content.transform([t.sx, t.ky, t.kx, t.sy, t.tx, t.ty]);
        let complex_clip = group.clip_path().filter(|clip| !simple_clip(clip));
        let mut clipped_body = Content::new();
        let mut clipped_resources = Resources::default();
        let (paint, paint_resources) = if complex_clip.is_some() {
            (&mut clipped_body, &mut clipped_resources)
        } else {
            (&mut *content, &mut *resources)
        };
        if let Some(clip) = group.clip_path().filter(|clip| simple_clip(clip)) {
            // A zero-area subpath preserves genuinely empty/hidden clipping.
            paint.rect(0.0, 0.0, 0.0, 0.0);
            let rule = draw_clip(clip.root(), clip.transform(), paint)?;
            match rule {
                usvg::FillRule::NonZero => paint.clip_nonzero(),
                usvg::FillRule::EvenOdd => paint.clip_even_odd(),
            };
            paint.end_path();
        }
        if !group.filters().is_empty() && !self.slot_probe {
            self.filtered_group(group, paint, paint_resources)?;
        } else if group.opacity().get() != 1.0 || group.mask().is_some() {
            let mut body = Content::new();
            let mut child_resources = Resources::default();
            self.pattern_transform = usvg::Transform::identity();
            self.children(group, &mut body, &mut child_resources)?;
            self.pattern_transform = previous_pattern.pre_concat(t);
            let id = self.reference()?;
            let stream = body.finish();
            let bbox = group.stroke_bounding_box();
            let mut form = self.pdf.form_xobject(id, &stream);
            form.bbox(Rect::new(
                bbox.left(),
                bbox.top(),
                bbox.right(),
                bbox.bottom(),
            ));
            form.group().transparency().isolated(true);
            child_resources.write(form.resources());
            form.finish();
            let name = format!("X{}", id.get());
            if let Some(mask) = group.mask() {
                self.mask(mask, paint, paint_resources)?;
            }
            self.opacity(group.opacity().get(), paint, paint_resources)?;
            paint.x_object(Name(name.as_bytes()));
            paint_resources.forms.push((name, id));
        } else {
            self.children(group, paint, paint_resources)?;
        }
        if let Some(clip) = complex_clip {
            let bbox = group.layer_bounding_box().to_rect();
            self.clip_mask(clip, bbox, content, resources)?;
            let id = self.geometry_form(clipped_body, clipped_resources, bbox)?;
            let name = format!("C{}", id.get());
            content.x_object(Name(name.as_bytes()));
            resources.forms.push((name, id));
        }
        self.transform = previous;
        self.pattern_transform = previous_pattern;
        self.clip = previous_clip;
        content.restore_state();
        Ok(())
    }

    // Every child is painted independently. PDF alpha composition computes
    // their union, preserving each child's fill rule and path direction.
    // Linked and child clip masks apply to separate Forms, so intersections
    // cannot overwrite ancestor opacity/masks or cancel overlapping paths.
    fn clip_mask(
        &mut self,
        clip: &usvg::ClipPath,
        bbox: usvg::Rect,
        content: &mut Content,
        resources: &mut Resources,
    ) -> Result<(), String> {
        let mut geometry = Content::new();
        let mut geometry_resources = Resources::default();
        let t = clip.transform();
        geometry.transform([t.sx, t.ky, t.kx, t.sy, t.tx, t.ty]);
        self.clip_children(clip.root(), &mut geometry, &mut geometry_resources)?;
        let mut id = self.geometry_form(geometry, geometry_resources, bbox)?;
        if let Some(parent) = clip.clip_path() {
            let mut body = Content::new();
            let mut child_resources = Resources::default();
            self.clip_mask(parent, bbox, &mut body, &mut child_resources)?;
            let name = format!("C{}", id.get());
            body.x_object(Name(name.as_bytes()));
            child_resources.forms.push((name, id));
            id = self.geometry_form(body, child_resources, bbox)?;
        }
        let state = self.reference()?;
        self.pdf
            .ext_graphics(state)
            .soft_mask()
            .subtype(MaskType::Alpha)
            .group(id);
        let name = format!("C{}", state.get());
        content.set_parameters(Name(name.as_bytes()));
        resources.states.push((name, state));
        Ok(())
    }

    fn geometry_form(
        &mut self,
        body: Content,
        resources: Resources,
        bbox: usvg::Rect,
    ) -> Result<Ref, String> {
        let id = self.reference()?;
        let stream = body.finish();
        let mut form = self.pdf.form_xobject(id, &stream);
        form.bbox(Rect::new(
            bbox.left(),
            bbox.top(),
            bbox.right(),
            bbox.bottom(),
        ));
        form.group()
            .transparency()
            .isolated(true)
            .color_space()
            .device_rgb();
        resources.write(form.resources());
        form.finish();
        Ok(id)
    }

    fn clip_children(
        &mut self,
        group: &usvg::Group,
        content: &mut Content,
        resources: &mut Resources,
    ) -> Result<(), String> {
        for node in group.children() {
            match node {
                usvg::Node::Group(child) => {
                    content.save_state();
                    let t = child.transform();
                    content.transform([t.sx, t.ky, t.kx, t.sy, t.tx, t.ty]);
                    if let Some(clip) = child.clip_path() {
                        let bbox = child.stroke_bounding_box();
                        let mut body = Content::new();
                        let mut child_resources = Resources::default();
                        self.clip_children(child, &mut body, &mut child_resources)?;
                        let id = self.geometry_form(body, child_resources, bbox)?;
                        self.clip_mask(clip, bbox, content, resources)?;
                        let name = format!("C{}", id.get());
                        content.x_object(Name(name.as_bytes()));
                        resources.forms.push((name, id));
                    } else {
                        self.clip_children(child, content, resources)?;
                    }
                    content.restore_state();
                }
                usvg::Node::Text(text) => {
                    self.clip_children(text.flattened(), content, resources)?
                }
                usvg::Node::Path(path) if path.is_visible() => {
                    content.set_fill_rgb(1.0, 1.0, 1.0);
                    draw_path(path.data(), content);
                    match path.fill().map(|fill| fill.rule()).unwrap_or_default() {
                        usvg::FillRule::NonZero => content.fill_nonzero(),
                        usvg::FillRule::EvenOdd => content.fill_even_odd(),
                    };
                }
                usvg::Node::Path(_) => {}
                _ => return Err("internal resolved SVG clip contains non-geometry artwork".into()),
            }
        }
        Ok(())
    }

    fn filtered_group(
        &mut self,
        group: &usvg::Group,
        content: &mut Content,
        resources: &mut Resources,
    ) -> Result<(), String> {
        let bbox = group.layer_bounding_box();
        let (sx, sy) = crate::effects_validator::filter_scale(group, self.effect_scale);
        let width = (bbox.width() * sx).ceil() as u32;
        let height = (bbox.height() * sy).ceil() as u32;
        let mut pixmap =
            tiny_skia::Pixmap::new(width, height).ok_or("SVG effect canvas allocation failed")?;
        // render_node offsets the absolute layer bbox itself and applies the
        // node's local transform. Cancel only those two bookkeeping transforms
        // so the true group renders once in its original local coordinates.
        let absolute = group.abs_layer_bounding_box();
        let transform = usvg::Transform::from_scale(sx, sy)
            .pre_translate(-bbox.x(), -bbox.y())
            .pre_concat(
                group
                    .transform()
                    .invert()
                    .ok_or("SVG effect transform is singular")?,
            )
            .pre_translate(absolute.x(), absolute.y());
        resvg::render_node(
            &usvg::Node::Group(Box::new(group.clone())),
            transform,
            &mut pixmap.as_mut(),
        )
        .ok_or("SVG effect group has no renderable region")?;
        if !pixmap.pixels().iter().any(|pixel| pixel.alpha() != 0) {
            return Ok(());
        }
        let name = self.bitmap(&pixmap, resources)?;
        content.save_state();
        content.transform([
            width as f32 / sx,
            0.0,
            0.0,
            -(height as f32 / sy),
            bbox.x(),
            bbox.y() + height as f32 / sy,
        ]);
        let logical = self.effect_text(group, usvg::Transform::identity(), bbox)?;
        if !logical.is_empty() {
            content
                .begin_marked_content_with_properties(Name(b"Span"))
                .properties()
                .actual_text(TextStr(&logical));
        }
        content.x_object(Name(name.as_bytes()));
        if !logical.is_empty() {
            content.end_marked_content();
        }
        content.restore_state();
        Ok(())
    }

    fn bitmap(
        &mut self,
        pixmap: &tiny_skia::Pixmap,
        resources: &mut Resources,
    ) -> Result<String, String> {
        let width = pixmap.width();
        let height = pixmap.height();
        let mut rgb = Vec::with_capacity(width as usize * height as usize * 3);
        let mut alpha = Vec::with_capacity(width as usize * height as usize);
        for pixel in pixmap.pixels() {
            let color = pixel.demultiply();
            if self.luminance {
                let gray = (color.red() as f32 * 0.2126
                    + color.green() as f32 * 0.7152
                    + color.blue() as f32 * 0.0722)
                    .round() as u8;
                rgb.extend_from_slice(&[gray; 3]);
            } else {
                rgb.extend_from_slice(&[color.red(), color.green(), color.blue()]);
            }
            alpha.push(color.alpha());
        }
        let mask = self.reference()?;
        let mut image = self.pdf.image_xobject(mask, &alpha);
        image
            .width(width as i32)
            .height(height as i32)
            .bits_per_component(8);
        image.color_space().device_gray();
        image.finish();
        let id = self.reference()?;
        let mut image = self.pdf.image_xobject(id, &rgb);
        image
            .width(width as i32)
            .height(height as i32)
            .bits_per_component(8)
            .s_mask(mask);
        image.color_space().device_rgb();
        image.finish();
        let name = format!("E{}", id.get());
        resources.forms.push((name.clone(), id));
        Ok(name)
    }

    fn image(
        &mut self,
        image: &usvg::Image,
        content: &mut Content,
        resources: &mut Resources,
    ) -> Result<(), String> {
        if let usvg::ImageKind::SVG(tree) = image.kind() {
            return self.group(tree.root(), content, resources);
        }
        let size = image.size();
        let mut pixmap = tiny_skia::Pixmap::new(size.width() as u32, size.height() as u32)
            .ok_or("authorized SVG image allocation failed")?;
        let bbox = image.abs_bounding_box();
        // render_node removes the absolute bbox offset. The image's geometry
        // is already present in its surrounding usvg group, so decode at its
        // genuine intrinsic pixel size and cancel only that bookkeeping offset.
        resvg::render_node(
            &usvg::Node::Image(Box::new(image.clone())),
            usvg::Transform::from_translate(bbox.x(), bbox.y()),
            &mut pixmap.as_mut(),
        )
        .ok_or("authorized SVG raster image has no renderable pixels")?;
        let name = self.bitmap(&pixmap, resources)?;
        content.save_state();
        content.transform([size.width(), 0.0, 0.0, -size.height(), 0.0, size.height()]);
        content.x_object(Name(name.as_bytes()));
        content.restore_state();
        Ok(())
    }

    // Original issued clusters remain the authority for effect replacement
    // text. The image's region never substitutes for selected glyph geometry.
    fn effect_text(
        &self,
        group: &usvg::Group,
        transform: usvg::Transform,
        region: usvg::NonZeroRect,
    ) -> Result<String, String> {
        let plan = crate::filter_logic_validator::source_plan(group);
        let mut logical = String::new();
        if plan.original {
            for node in group.children() {
                match node {
                    usvg::Node::Group(child) if child.opacity().get() != 0.0 => {
                        logical.push_str(&self.effect_text(
                            child,
                            transform.pre_concat(child.transform()),
                            region,
                        )?);
                    }
                    usvg::Node::Image(image) if image.is_visible() => {
                        if let usvg::ImageKind::SVG(tree) = image.kind() {
                            logical.push_str(&self.effect_text(tree.root(), transform, region)?);
                        }
                        if let Some(input) = self.html_surfaces.get(group.id()) {
                            if let Some(bounds) = image.bounding_box().transform(transform) {
                                let inside_region = bounds.right() > region.left()
                                    && bounds.left() < region.right()
                                    && bounds.bottom() > region.top()
                                    && bounds.top() < region.bottom();
                                let inside_clip =
                                    match (self.clip, bounds.transform(self.transform)) {
                                        (Some(clip), Some(bounds)) => {
                                            bounds.right() > clip[0]
                                                && bounds.left() < clip[2]
                                                && bounds.bottom() > clip[1]
                                                && bounds.top() < clip[3]
                                        }
                                        (None, _) => true,
                                        _ => false,
                                    };
                                if inside_region && inside_clip {
                                    for (text, bounds) in &input.clusters {
                                        let rect = usvg::Rect::from_ltrb(
                                            bounds[0], bounds[1], bounds[2], bounds[3],
                                        );
                                        let Some(bounds) =
                                            rect.and_then(|rect| rect.transform(transform))
                                        else {
                                            continue;
                                        };
                                        let inside_region = bounds.right() > region.left()
                                            && bounds.left() < region.right()
                                            && bounds.bottom() > region.top()
                                            && bounds.top() < region.bottom();
                                        let inside_clip =
                                            match (self.clip, bounds.transform(self.transform)) {
                                                (Some(clip), Some(bounds)) => {
                                                    bounds.right() > clip[0]
                                                        && bounds.left() < clip[2]
                                                        && bounds.bottom() > clip[1]
                                                        && bounds.top() < clip[3]
                                                }
                                                (None, _) => true,
                                                _ => false,
                                            };
                                        if inside_region && inside_clip {
                                            logical.push_str(text);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    usvg::Node::Text(text) => {
                        for span in text.layouted().iter().filter(|span| {
                            span.visible
                                && span
                                    .fill
                                    .as_ref()
                                    .is_some_and(|fill| fill.opacity().get() != 0.0)
                        }) {
                            let clusters = self
                                .clusters
                                .get(&(span as *const usvg::layout::Span as usize))
                                .ok_or("internal SVG effect lost its issued text clusters")?;
                            for cluster in clusters {
                                let mut paints = false;
                                for glyph in cluster.glyphs {
                                    let bounds = self
                                        .database
                                        .with_face_data(glyph.font, |data, index| {
                                            ttf_parser::Face::parse(data, index)
                                                .ok()
                                                .and_then(|face| face.glyph_bounding_box(glyph.id))
                                        })
                                        .flatten();
                                    if let Some(bounds) = bounds {
                                        let bounds = usvg::Rect::from_ltrb(
                                            bounds.x_min as f32,
                                            bounds.y_min as f32,
                                            bounds.x_max as f32,
                                            bounds.y_max as f32,
                                        )
                                        .and_then(|rect| {
                                            rect.transform(
                                                transform.pre_concat(glyph.outline_transform()),
                                            )
                                        });
                                        if let Some(bounds) = bounds {
                                            let inside_region = bounds.right() > region.left()
                                                && bounds.left() < region.right()
                                                && bounds.bottom() > region.top()
                                                && bounds.top() < region.bottom();
                                            let inside_clip =
                                                match (self.clip, bounds.transform(self.transform))
                                                {
                                                    (Some(clip), Some(bounds)) => {
                                                        bounds.right() > clip[0]
                                                            && bounds.left() < clip[2]
                                                            && bounds.bottom() > clip[1]
                                                            && bounds.top() < clip[3]
                                                    }
                                                    (None, _) => true,
                                                    _ => false,
                                                };
                                            paints |= inside_region && inside_clip;
                                        }
                                    }
                                }
                                if paints || cluster.text.chars().all(char::is_whitespace) {
                                    logical.push_str(cluster.text);
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        for (input, placement) in plan.images {
            logical.push_str(&self.effect_text(input, transform.pre_concat(placement), region)?);
        }
        if logical.chars().all(char::is_whitespace) {
            logical.clear();
        }
        Ok(logical)
    }

    fn mask(
        &mut self,
        mask: &usvg::Mask,
        content: &mut Content,
        resources: &mut Resources,
    ) -> Result<(), String> {
        let mut body = Content::new();
        let mut child_resources = Resources::default();
        let previous = self.transform;
        let previous_luminance = self.luminance;
        let previous_pattern = self.pattern_transform;
        self.transform = usvg::Transform::identity();
        self.pattern_transform = usvg::Transform::identity();
        self.luminance = mask.kind() == usvg::MaskType::Luminance;
        if let Some(parent) = mask.mask() {
            self.mask(parent, &mut body, &mut child_resources)?;
        }
        self.children(mask.root(), &mut body, &mut child_resources)?;
        self.transform = previous;
        self.pattern_transform = previous_pattern;
        self.luminance = previous_luminance;
        let id = self.reference()?;
        let stream = body.finish();
        let rect = mask.rect();
        let mut form = self.pdf.form_xobject(id, &stream);
        form.bbox(Rect::new(
            rect.left(),
            rect.top(),
            rect.right(),
            rect.bottom(),
        ));
        form.group()
            .transparency()
            .isolated(true)
            .color_space()
            .device_rgb();
        child_resources.write(form.resources());
        form.finish();
        let state = self.reference()?;
        self.pdf
            .ext_graphics(state)
            .soft_mask()
            .subtype(if mask.kind() == usvg::MaskType::Alpha {
                MaskType::Alpha
            } else {
                MaskType::Luminosity
            })
            .group(id);
        let name = format!("M{}", state.get());
        content.set_parameters(Name(name.as_bytes()));
        resources.states.push((name, state));
        Ok(())
    }

    // SVG luminance uses these fixed sRGB coefficients. Convert colors to gray
    // before the PDF luminosity mask, whose DeviceRGB gray weights can differ.
    fn rgb(&self, color: usvg::Color) -> [f32; 3] {
        let rgb = [
            color.red as f32 / 255.0,
            color.green as f32 / 255.0,
            color.blue as f32 / 255.0,
        ];
        if self.luminance {
            let gray = rgb[0] * 0.2126 + rgb[1] * 0.7152 + rgb[2] * 0.0722;
            [gray; 3]
        } else {
            rgb
        }
    }

    fn children(
        &mut self,
        group: &usvg::Group,
        content: &mut Content,
        resources: &mut Resources,
    ) -> Result<(), String> {
        for node in group.children() {
            match node {
                usvg::Node::Group(group) => self.group(group, content, resources)?,
                usvg::Node::Path(path) if self.slots.contains(path.id()) && path.is_visible() => {
                    self.html_slot(path, content, resources)?;
                }
                usvg::Node::Image(image)
                    if self.slots.contains(group.id()) && image.is_visible() =>
                {
                    let input = self
                        .html_surfaces
                        .get(group.id())
                        .ok_or("internal HTML image lost its issued source")?;
                    let (width, height) = (input.width, input.height);
                    self.html_slot_box(
                        group.id(),
                        image.bounding_box(),
                        width,
                        height,
                        content,
                        resources,
                    )?;
                }
                usvg::Node::Path(path) if path.is_visible() => {
                    let stroke_first = path.paint_order() == usvg::PaintOrder::StrokeAndFill;
                    for stroke in [stroke_first, !stroke_first] {
                        if stroke {
                            if let Some(stroke) = path.stroke() {
                                let style = stroke.to_tiny_skia();
                                let resolution =
                                    usvg::tiny_skia_path::PathStroker::compute_resolution_scale(
                                        &path.abs_transform(),
                                    );
                                // Expand the authored dash and stroke in SVG user space,
                                // before the enclosing affine transform. PDF dash flattening
                                // otherwise changes cap intersections on curved segments.
                                let dashed;
                                let data = if let Some(dash) = &style.dash {
                                    dashed = path
                                        .data()
                                        .dash(dash, resolution)
                                        .ok_or("SVG dash expansion failed")?;
                                    &dashed
                                } else {
                                    path.data()
                                };
                                let outline = data
                                    .stroke(&style, resolution)
                                    .ok_or("SVG stroke expansion failed")?;
                                self.fill(
                                    &outline,
                                    stroke.paint(),
                                    stroke.opacity().get(),
                                    usvg::FillRule::NonZero,
                                    content,
                                    resources,
                                )?;
                            }
                        } else if let Some(fill) = path.fill() {
                            self.fill(
                                path.data(),
                                fill.paint(),
                                fill.opacity().get(),
                                fill.rule(),
                                content,
                                resources,
                            )?;
                        }
                    }
                }
                usvg::Node::Text(text) => {
                    for span in text.layouted().iter().filter(|span| {
                        span.visible && (span.fill.is_some() || span.stroke.is_some())
                    }) {
                        let clusters = self
                            .clusters
                            .get(&(span as *const usvg::layout::Span as usize))
                            .cloned()
                            .ok_or("internal SVG text painter lost its issued cluster plan")?;
                        // A server is shared by every real glyph in this span. Its
                        // geometry was resolved against the whole text object's box,
                        // never against one cluster or its PDF text matrix.
                        content.save_state();
                        self.text_paint(span.fill.as_ref().unwrap(), content, resources)?;
                        for cluster in clusters {
                            self.text_cluster(cluster, content, resources)?;
                        }
                        content.restore_state();
                    }
                }
                usvg::Node::Image(image) if image.is_visible() => {
                    self.image(image, content, resources)?
                }
                usvg::Node::Image(_) => {}
                usvg::Node::Path(_) => {}
            }
        }
        Ok(())
    }

    fn html_slot(
        &mut self,
        path: &usvg::Path,
        content: &mut Content,
        resources: &mut Resources,
    ) -> Result<(), String> {
        let bbox = path.bounding_box();
        self.html_slot_box(
            path.id(),
            bbox,
            bbox.width() * 0.75,
            bbox.height() * 0.75,
            content,
            resources,
        )
    }

    fn html_slot_box(
        &mut self,
        slot_id: &str,
        bbox: usvg::Rect,
        width: f32,
        height: f32,
        content: &mut Content,
        resources: &mut Resources,
    ) -> Result<(), String> {
        let local = usvg::Transform::from_row(
            bbox.width() / width,
            0.0,
            0.0,
            -bbox.height() / height,
            bbox.x(),
            bbox.bottom(),
        );
        let t = self.transform.pre_concat(local);
        let id = self.reference()?;
        let mut form = self.pdf.form_xobject(id, &[]);
        form.bbox(Rect::new(0.0, 0.0, width, height));
        form.pair(Name(b"NativeHtmlSlot"), Str(slot_id.as_bytes()));
        form.pair(Name(b"NativeHtmlWidth"), width);
        form.pair(Name(b"NativeHtmlHeight"), height);
        form.insert(Name(b"NativeHtmlTransform"))
            .array()
            .items([t.sx, t.ky, t.kx, t.sy, t.tx, t.ty]);
        if let Some(clip) = self.clip {
            form.insert(Name(b"NativeHtmlClip")).array().items(clip);
        }
        form.resources().finish();
        form.finish();
        let name = format!("H{}", id.get());
        resources.forms.push((name.clone(), id));
        content.save_state();
        content.transform([local.sx, local.ky, local.kx, local.sy, local.tx, local.ty]);
        content.x_object(Name(name.as_bytes()));
        content.restore_state();
        Ok(())
    }

    // ActualText starts only after this genuine text object has its selected
    // face, authored size and first glyph matrix. Readers can then preserve
    // the cluster's real advance and font bounds without a detached setup run.
    fn text_cluster(
        &mut self,
        cluster: crate::vector_validator::Cluster<'_>,
        content: &mut Content,
        resources: &mut Resources,
    ) -> Result<(), String> {
        let first = cluster
            .glyphs
            .first()
            .ok_or("internal SVG text painter received an empty cluster plan")?;
        let (name, id, units) = self.selected_font(first.font)?;
        if !resources.fonts.iter().any(|(_, resource)| *resource == id) {
            resources.fonts.push((name.clone(), id));
        }
        content.save_state();
        content.begin_text();
        content.set_font(Name(name.as_bytes()), first.font_size());
        let scale = units as f32 / first.font_size();
        let t = first.outline_transform();
        content.set_text_matrix([
            t.sx * scale,
            t.ky * scale,
            t.kx * scale,
            t.sy * scale,
            t.tx,
            t.ty,
        ]);
        content
            .begin_marked_content_with_properties(Name(b"Span"))
            .properties()
            .actual_text(TextStr(cluster.text));
        for (index, glyph) in cluster.glyphs.iter().enumerate() {
            if index != 0 {
                let t = glyph.outline_transform();
                content.set_text_matrix([
                    t.sx * scale,
                    t.ky * scale,
                    t.kx * scale,
                    t.sy * scale,
                    t.tx,
                    t.ty,
                ]);
            }
            content.show(Str(&glyph.id.0.to_be_bytes()));
        }
        content.end_marked_content();
        content.end_text();
        content.restore_state();
        Ok(())
    }

    fn selected_font(&mut self, selected: usvg::fontdb::ID) -> Result<(String, Ref, u16), String> {
        if let Some(resource) = self.fonts.get(&selected) {
            return Ok(resource.clone());
        }
        // Full authorized bytes are embedded. This also honors a font's
        // no-subsetting flag without replacing its original glyph IDs.
        let (data, index) = self
            .database
            .with_face_data(selected, |data, index| (data.to_vec(), index))
            .ok_or("internal selected SVG face lost its authorized bytes")?;
        let face = ttf_parser::Face::parse(&data, index)
            .map_err(|_| "internal selected SVG face no longer parses")?;
        let scale = 1000.0 / face.units_per_em() as f32;
        let id = self.reference()?;
        let cid = self.reference()?;
        let descriptor = self.reference()?;
        let source = self.reference()?;
        let unicode = self.reference()?;
        let name = format!("F{}", id.get());
        let postscript = self
            .database
            .face(selected)
            .ok_or("internal selected SVG face lost its source name")?
            .post_script_name
            .clone();
        self.pdf
            .type0_font(id)
            .base_font(Name(postscript.as_bytes()))
            .encoding_predefined(Name(b"Identity-H"))
            .descendant_font(cid)
            .to_unicode(unicode);
        let mut font = self.pdf.cid_font(cid);
        font.subtype(CidFontType::Type2)
            .base_font(Name(postscript.as_bytes()))
            .system_info(SystemInfo {
                registry: Str(b"Adobe"),
                ordering: Str(b"Identity"),
                supplement: 0,
            })
            .font_descriptor(descriptor)
            .cid_to_gid_map_predefined(Name(b"Identity"));
        font.widths().consecutive(
            0,
            (0..face.number_of_glyphs()).map(|id| {
                face.glyph_hor_advance(ttf_parser::GlyphId(id)).unwrap_or(0) as f32 * scale
            }),
        );
        font.finish();
        let bbox = face.global_bounding_box();
        self.pdf
            .font_descriptor(descriptor)
            .name(Name(postscript.as_bytes()))
            .flags(
                FontFlags::SYMBOLIC
                    | if face.is_italic() {
                        FontFlags::ITALIC
                    } else {
                        FontFlags::empty()
                    }
                    | if face.is_monospaced() {
                        FontFlags::FIXED_PITCH
                    } else {
                        FontFlags::empty()
                    },
            )
            .bbox(Rect::new(
                bbox.x_min as f32 * scale,
                bbox.y_min as f32 * scale,
                bbox.x_max as f32 * scale,
                bbox.y_max as f32 * scale,
            ))
            .italic_angle(face.italic_angle())
            .ascent(face.ascender() as f32 * scale)
            .descent(face.descender() as f32 * scale)
            .cap_height(face.capital_height().unwrap_or(face.ascender()) as f32 * scale)
            .stem_v(0.0)
            .font_file2(source);
        self.pdf
            .stream(source, &data)
            .pair(Name(b"Length1"), data.len() as i32);
        let mut cmap = UnicodeCmap::new(
            Name(b"NativeSvgUnicode"),
            SystemInfo {
                registry: Str(b"Adobe"),
                ordering: Str(b"UCS"),
                supplement: 0,
            },
        );
        for (glyph, text) in &self.mappings[&selected] {
            cmap.pair_with_multiple(*glyph, text.chars());
        }
        self.pdf.stream(unicode, &cmap.finish());
        self.fonts
            .insert(selected, (name.clone(), id, face.units_per_em()));
        Ok((name, id, face.units_per_em()))
    }

    fn fill(
        &mut self,
        path: &Path,
        paint: &usvg::Paint,
        opacity: f32,
        rule: usvg::FillRule,
        content: &mut Content,
        resources: &mut Resources,
    ) -> Result<(), String> {
        content.save_state();
        match paint {
            usvg::Paint::Color(color) => {
                self.opacity(opacity, content, resources)?;
                let [r, g, b] = self.rgb(*color);
                content.set_fill_rgb(r, g, b);
                draw_path(path, content);
                match rule {
                    usvg::FillRule::NonZero => content.fill_nonzero(),
                    usvg::FillRule::EvenOdd => content.fill_even_odd(),
                };
            }
            usvg::Paint::Pattern(pattern) => {
                let (name, id) = self.tiling_pattern(pattern)?;
                resources.patterns.push((name.clone(), id));
                self.opacity(opacity, content, resources)?;
                content.set_fill_color_space(Name(b"Pattern"));
                content.set_fill_pattern([], Name(name.as_bytes()));
                draw_path(path, content);
                match rule {
                    usvg::FillRule::NonZero => content.fill_nonzero(),
                    usvg::FillRule::EvenOdd => content.fill_even_odd(),
                };
            }
            _ => {
                let (id, t, alpha) = self.gradient_shading(paint)?;
                self.opacity(opacity * alpha, content, resources)?;
                draw_path(path, content);
                match rule {
                    usvg::FillRule::NonZero => content.clip_nonzero(),
                    usvg::FillRule::EvenOdd => content.clip_even_odd(),
                };
                content.end_path();
                content.transform([t.sx, t.ky, t.kx, t.sy, t.tx, t.ty]);
                let name = format!("G{}", id.get());
                content.shading(Name(name.as_bytes()));
                resources.shadings.push((name, id));
            }
        }
        content.restore_state();
        Ok(())
    }
    // PDF patterns fill genuine CID glyphs directly. No surrogate outline,
    // clipping-only text, or hidden logical text run is needed.
    fn text_paint(
        &mut self,
        fill: &usvg::Fill,
        content: &mut Content,
        resources: &mut Resources,
    ) -> Result<(), String> {
        let (name, id, opacity) = match fill.paint() {
            usvg::Paint::Color(color) => {
                let [r, g, b] = self.rgb(*color);
                content.set_fill_rgb(r, g, b);
                self.opacity(fill.opacity().get(), content, resources)?;
                return Ok(());
            }
            usvg::Paint::Pattern(pattern) => {
                let (name, id) = self.tiling_pattern(pattern)?;
                (name, id, fill.opacity().get())
            }
            paint => {
                let (shading, transform, alpha) = self.gradient_shading(paint)?;
                let id = self.reference()?;
                let t = self.pattern_transform.pre_concat(transform);
                self.pdf
                    .shading_pattern(id)
                    .shading_ref(shading)
                    .matrix([t.sx, t.ky, t.kx, t.sy, t.tx, t.ty]);
                (format!("P{}", id.get()), id, fill.opacity().get() * alpha)
            }
        };
        resources.patterns.push((name.clone(), id));
        self.opacity(opacity, content, resources)?;
        content.set_fill_color_space(Name(b"Pattern"));
        content.set_fill_pattern([], Name(name.as_bytes()));
        Ok(())
    }

    // Geometry and text share the exact same SVG tile and page-space matrix.
    fn tiling_pattern(&mut self, pattern: &usvg::Pattern) -> Result<(String, Ref), String> {
        let mut body = Content::new();
        let mut child_resources = Resources::default();
        let previous = self.transform;
        let previous_pattern = self.pattern_transform;
        self.transform = usvg::Transform::identity();
        self.pattern_transform = usvg::Transform::identity();
        self.children(pattern.root(), &mut body, &mut child_resources)?;
        self.transform = previous;
        self.pattern_transform = previous_pattern;
        let rect = pattern.rect();
        let t = previous_pattern
            .pre_concat(pattern.transform())
            .pre_translate(rect.x(), rect.y());
        let id = self.reference()?;
        let stream = body.finish();
        let mut tile = self.pdf.tiling_pattern(id, &stream);
        tile.paint_type(PaintType::Colored)
            .tiling_type(TilingType::NoDistortion)
            .bbox(Rect::new(0.0, 0.0, rect.width(), rect.height()))
            .x_step(rect.width())
            .y_step(rect.height())
            .matrix([t.sx, t.ky, t.kx, t.sy, t.tx, t.ty]);
        child_resources.write(tile.resources());
        tile.finish();
        Ok((format!("P{}", id.get()), id))
    }

    // One server occurrence allocates one shading, matching gradient_work's
    // pre-allocation accounting over the immutable prepared tree.
    fn gradient_shading(
        &mut self,
        paint: &usvg::Paint,
    ) -> Result<(Ref, usvg::Transform, f32), String> {
        let (gradient, kind, coords): (&usvg::BaseGradient, _, Vec<f32>) = match paint {
            usvg::Paint::LinearGradient(g) => (
                g,
                FunctionShadingType::Axial,
                vec![g.x1(), g.y1(), g.x2(), g.y2()],
            ),
            usvg::Paint::RadialGradient(g) => (
                g,
                FunctionShadingType::Radial,
                vec![g.fx(), g.fy(), 0.0, g.cx(), g.cy(), g.r().get()],
            ),
            _ => return Err("internal SVG scene contains an unsupported gradient server".into()),
        };
        let function = self.gradient_function(gradient.stops())?;
        let id = self.reference()?;
        let mut shading = self.pdf.function_shading(id);
        shading
            .shading_type(kind)
            .coords(coords)
            .function(function)
            .extend([true, true]);
        shading.color_space().device_rgb();
        shading.finish();
        Ok((
            id,
            gradient.transform(),
            gradient.stops()[0].opacity().get(),
        ))
    }

    fn gradient_function(&mut self, stops: &[usvg::Stop]) -> Result<Ref, String> {
        let mut functions = Vec::new();
        for pair in stops.windows(2) {
            let id = self.reference()?;
            let a = self.rgb(pair[0].color());
            let b = self.rgb(pair[1].color());
            self.pdf
                .exponential_function(id)
                .domain([0.0, 1.0])
                .c0(a)
                .c1(b)
                .n(1.0);
            functions.push(id);
        }
        if functions.len() == 1 {
            Ok(functions[0])
        } else {
            let id = self.reference()?;
            self.pdf
                .stitching_function(id)
                .domain([0.0, 1.0])
                .functions(functions)
                .bounds(
                    stops[1..stops.len() - 1]
                        .iter()
                        .map(|stop| stop.offset().get()),
                )
                .encode(stops.windows(2).flat_map(|_| [0.0, 1.0]));
            Ok(id)
        }
    }
}

// Bounds used for rectangular transport come from resolved clip geometry,
// including linked intersections in the same native coordinate system.
fn clip_bounds(clip: &usvg::ClipPath, transform: usvg::Transform) -> Option<[f32; 4]> {
    let rect = clip
        .root()
        .bounding_box()
        .transform(clip.transform())?
        .transform(transform)?;
    let mut bounds = [rect.left(), rect.top(), rect.right(), rect.bottom()];
    if let Some(parent) = clip.clip_path() {
        let parent = clip_bounds(parent, transform)?;
        bounds = [
            bounds[0].max(parent[0]),
            bounds[1].max(parent[1]),
            bounds[2].min(parent[2]),
            bounds[3].min(parent[3]),
        ];
    }
    Some(bounds)
}

// A single resolved path can use the PDF clip operator directly. More than
// one path must use union composition even when their fill rules match.
fn simple_clip(clip: &usvg::ClipPath) -> bool {
    fn paths(group: &usvg::Group) -> Option<usize> {
        if group.clip_path().is_some() {
            return None;
        }
        let mut count = 0;
        for node in group.children() {
            count += match node {
                usvg::Node::Group(group) => paths(group)?,
                usvg::Node::Path(_) => 1,
                _ => return None,
            };
        }
        Some(count)
    }
    clip.clip_path().is_none() && paths(clip.root()).is_some_and(|count| count <= 1)
}

// A transformed single clip shape may be wrapped in generated groups by usvg.
fn draw_clip(
    group: &usvg::Group,
    transform: usvg::Transform,
    content: &mut Content,
) -> Result<usvg::FillRule, String> {
    let mut rule = usvg::FillRule::NonZero;
    for node in group.children() {
        match node {
            usvg::Node::Group(group) => {
                rule = draw_clip(group, transform.pre_concat(group.transform()), content)?
            }
            usvg::Node::Path(path) if path.is_visible() => {
                let data = path
                    .data()
                    .clone()
                    .transform(transform)
                    .ok_or("SVG clip transform failed")?;
                draw_path(&data, content);
                rule = path.fill().map(|fill| fill.rule()).unwrap_or_default();
            }
            usvg::Node::Path(_) => {}
            _ => return Err("internal SVG geometry clip contains non-shape artwork".into()),
        }
    }
    Ok(rule)
}

fn draw_path(path: &Path, content: &mut Content) {
    let mut previous = usvg::tiny_skia_path::Point::from_xy(0.0, 0.0);
    for segment in path.segments() {
        match segment {
            PathSegment::MoveTo(p) => {
                content.move_to(p.x, p.y);
                previous = p;
            }
            PathSegment::LineTo(p) => {
                content.line_to(p.x, p.y);
                previous = p;
            }
            PathSegment::QuadTo(control, end) => {
                content.cubic_to(
                    (previous.x + 2.0 * control.x) / 3.0,
                    (previous.y + 2.0 * control.y) / 3.0,
                    (end.x + 2.0 * control.x) / 3.0,
                    (end.y + 2.0 * control.y) / 3.0,
                    end.x,
                    end.y,
                );
                previous = end;
            }
            PathSegment::CubicTo(a, b, end) => {
                content.cubic_to(a.x, a.y, b.x, b.y, end.x, end.y);
                previous = end;
            }
            PathSegment::Close => {
                content.close_path();
            }
        }
    }
}

pub fn convert(tree: &usvg::Tree) -> Result<Vec<u8>, String> {
    convert_with_slots(tree, std::collections::HashSet::new())
}

pub fn convert_with_slots(
    tree: &usvg::Tree,
    slots: std::collections::HashSet<String>,
) -> Result<Vec<u8>, String> {
    convert_with_effects(tree, slots, (1.0, 1.0))
}

pub fn convert_with_effects(
    tree: &usvg::Tree,
    slots: std::collections::HashSet<String>,
    effect_scale: (f32, f32),
) -> Result<Vec<u8>, String> {
    convert_prepared(
        tree,
        slots,
        effect_scale,
        false,
        HashMap::new(),
        None,
        u64::MAX,
    )
}

pub fn probe_slots(
    tree: &usvg::Tree,
    slots: std::collections::HashSet<String>,
) -> Result<Vec<u8>, String> {
    convert_prepared(
        tree,
        slots,
        (1.0, 1.0),
        true,
        HashMap::new(),
        None,
        u64::MAX,
    )
}

pub fn convert_with_html_surfaces(
    tree: &usvg::Tree,
    surfaces: HashMap<String, HtmlSurface>,
    scale: (f32, f32),
) -> Result<Vec<u8>, String> {
    let slots = surfaces.keys().cloned().collect();
    convert_prepared(tree, slots, scale, false, surfaces, None, u64::MAX)
}

pub fn convert_with_annotations(
    tree: &usvg::Tree,
    plan: crate::annotation_validator::Plan,
    ceiling: u64,
    slots: std::collections::HashSet<String>,
    effect_scale: (f32, f32),
) -> Result<Vec<u8>, String> {
    convert_prepared(
        tree,
        slots,
        effect_scale,
        false,
        HashMap::new(),
        Some(plan),
        ceiling,
    )
}

fn convert_prepared(
    tree: &usvg::Tree,
    slots: std::collections::HashSet<String>,
    effect_scale: (f32, f32),
    slot_probe: bool,
    html_surfaces: HashMap<String, HtmlSurface>,
    annotations: Option<crate::annotation_validator::Plan>,
    object_ceiling: u64,
) -> Result<Vec<u8>, String> {
    if object_ceiling < 4 {
        return Err("max_pdf_objects".into());
    }
    let prepared = crate::vector_validator::text_with_surfaces(tree.root(), tree.fontdb(), &slots)?;

    let mut writer = Writer {
        pdf: Pdf::new(),
        next: 5,
        object_ceiling,
        database: tree.fontdb(),
        fonts: HashMap::new(),
        mappings: prepared.mappings,
        clusters: prepared.clusters,
        transform: usvg::Transform::from_row(
            0.75,
            0.0,
            0.0,
            -0.75,
            0.0,
            tree.size().height() * 0.75,
        ),
        pattern_transform: usvg::Transform::from_row(
            0.75,
            0.0,
            0.0,
            -0.75,
            0.0,
            tree.size().height() * 0.75,
        ),
        luminance: false,
        effect_scale,
        slot_probe,
        html_surfaces,
        slots,
        clip: Some([
            0.0,
            0.0,
            tree.size().width() * 0.75,
            tree.size().height() * 0.75,
        ]),
    };
    let mut content = Content::new();
    let mut resources = Resources::default();
    let width = tree.size().width() * 0.75;
    let height = tree.size().height() * 0.75;
    content.transform([0.75, 0.0, 0.0, -0.75, 0.0, height]);
    writer.group(tree.root(), &mut content, &mut resources)?;
    let mut links = vec![];
    if let Some(plan) = &annotations {
        for link in &plan.links {
            let target = if let Some(fragment) = link.destination.strip_prefix('#') {
                if fragment.is_empty() {
                    Some([0.0, 0.0, width, height])
                } else {
                    plan.targets.get(fragment).copied()
                }
            } else {
                None
            };
            let id = writer.reference()?;
            let mut annotation = writer.pdf.annotation(id);
            annotation
                .subtype(AnnotationType::Link)
                .rect(Rect::new(
                    link.rect[0],
                    link.rect[1],
                    link.rect[2],
                    link.rect[3],
                ))
                .border(0.0, 0.0, 0.0, None);
            annotation.pair(Name(b"NativeSvgAnchor"), Str(link.anchor.as_bytes()));
            annotation.pair(Name(b"NativeSvgHref"), Str(link.destination.as_bytes()));
            annotation.pair(Name(b"NativeSvgAnchorIndex"), link.index as i32);
            if let Some(target) = target {
                annotation
                    .action()
                    .action_type(ActionType::GoTo)
                    .destination()
                    .page(Ref::new(3))
                    .xyz(target[0], target[3], None);
            } else {
                annotation
                    .action()
                    .action_type(ActionType::Uri)
                    .uri(Str(link.destination.as_bytes()));
            }
            annotation.finish();
            links.push(id);
        }
    }
    writer.pdf.catalog(Ref::new(1)).pages(Ref::new(2));
    writer.pdf.pages(Ref::new(2)).kids([Ref::new(3)]).count(1);
    let mut page = writer.pdf.page(Ref::new(3));
    page.parent(Ref::new(2))
        .media_box(Rect::new(0.0, 0.0, width, height))
        .contents(Ref::new(4));
    if !links.is_empty() {
        page.annotations(links);
    }
    if let Some(plan) = &annotations {
        let mut targets = page.insert(Name(b"NativeSvgTargets")).array();
        for (id, bounds) in &plan.targets {
            let mut record = targets.push().dict();
            record.pair(Name(b"Id"), Str(id.as_bytes()));
            record.insert(Name(b"Rect")).array().items(*bounds);
        }
    }
    resources.write(page.resources());
    page.finish();
    writer.pdf.stream(Ref::new(4), &content.finish());
    Ok(writer.pdf.finish())
}
