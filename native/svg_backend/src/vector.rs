//! Shape-only PDF composition over the same usvg tree as the raster adapter.
//! Authorization and supported input semantics belong to the Elixir validators.
use pdf_writer::types::FunctionShadingType;
use pdf_writer::{Content, Finish, Name, Pdf, Rect, Ref};
use usvg::tiny_skia_path::{Path, PathSegment};

#[derive(Default)]
struct Resources {
    forms: Vec<(String, Ref)>,
    states: Vec<(String, Ref)>,
    shadings: Vec<(String, Ref)>,
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
    }
}

struct Writer {
    pdf: Pdf,
    next: i32,
}

impl Writer {
    fn reference(&mut self) -> Ref {
        let id = Ref::new(self.next);
        self.next += 1;
        id
    }

    fn opacity(&mut self, opacity: f32, content: &mut Content, resources: &mut Resources) {
        if opacity != 1.0 {
            let id = self.reference();
            self.pdf.ext_graphics(id).non_stroking_alpha(opacity);
            let name = format!("A{}", id.get());
            content.set_parameters(Name(name.as_bytes()));
            resources.states.push((name, id));
        }
    }

    fn group(
        &mut self,
        group: &usvg::Group,
        content: &mut Content,
        resources: &mut Resources,
    ) -> Result<(), String> {
        content.save_state();
        let t = group.transform();
        content.transform([t.sx, t.ky, t.kx, t.sy, t.tx, t.ty]);
        // Validators admit one authored geometry child, or a generated viewport clip.
        if let Some(clip) = group.clip_path() {
            // An empty PDF current path does not establish an empty clip.
            // Keep a zero-area subpath so hidden geometry clips all artwork.
            content.rect(0.0, 0.0, 0.0, 0.0);
            let rule = draw_clip(clip.root(), clip.transform(), content)?;
            match rule {
                usvg::FillRule::NonZero => content.clip_nonzero(),
                usvg::FillRule::EvenOdd => content.clip_even_odd(),
            };
            content.end_path();
        }
        if group.opacity().get() != 1.0 {
            let mut body = Content::new();
            let mut child_resources = Resources::default();
            self.children(group, &mut body, &mut child_resources)?;
            let id = self.reference();
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
            self.opacity(group.opacity().get(), content, resources);
            content.x_object(Name(name.as_bytes()));
            resources.forms.push((name, id));
        } else {
            self.children(group, content, resources)?;
        }
        content.restore_state();
        Ok(())
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
                usvg::Node::Path(_) => {}
                _ => return Err("internal SVG shape scene contains non-shape artwork".into()),
            }
        }
        Ok(())
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
                self.opacity(opacity, content, resources);
                content.set_fill_rgb(
                    color.red as f32 / 255.0,
                    color.green as f32 / 255.0,
                    color.blue as f32 / 255.0,
                );
                draw_path(path, content);
                match rule {
                    usvg::FillRule::NonZero => content.fill_nonzero(),
                    usvg::FillRule::EvenOdd => content.fill_even_odd(),
                };
            }
            _ => {
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
                    _ => {
                        return Err("internal SVG scene contains an unsupported paint server".into())
                    }
                };
                self.opacity(
                    opacity * gradient.stops()[0].opacity().get(),
                    content,
                    resources,
                );
                draw_path(path, content);
                match rule {
                    usvg::FillRule::NonZero => content.clip_nonzero(),
                    usvg::FillRule::EvenOdd => content.clip_even_odd(),
                };
                content.end_path();
                let t = gradient.transform();
                content.transform([t.sx, t.ky, t.kx, t.sy, t.tx, t.ty]);
                let function = self.gradient_function(gradient.stops());
                let id = self.reference();
                let mut shading = self.pdf.function_shading(id);
                shading
                    .shading_type(kind)
                    .coords(coords)
                    .function(function)
                    .extend([true, true]);
                shading.color_space().device_rgb();
                shading.finish();
                let name = format!("G{}", id.get());
                content.shading(Name(name.as_bytes()));
                resources.shadings.push((name, id));
            }
        }
        content.restore_state();
        Ok(())
    }
    fn gradient_function(&mut self, stops: &[usvg::Stop]) -> Ref {
        let mut functions = Vec::new();
        for pair in stops.windows(2) {
            let id = self.reference();
            let a = pair[0].color();
            let b = pair[1].color();
            self.pdf
                .exponential_function(id)
                .domain([0.0, 1.0])
                .c0([
                    a.red as f32 / 255.0,
                    a.green as f32 / 255.0,
                    a.blue as f32 / 255.0,
                ])
                .c1([
                    b.red as f32 / 255.0,
                    b.green as f32 / 255.0,
                    b.blue as f32 / 255.0,
                ])
                .n(1.0);
            functions.push(id);
        }
        if functions.len() == 1 {
            functions[0]
        } else {
            let id = self.reference();
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
            id
        }
    }
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
    let mut writer = Writer {
        pdf: Pdf::new(),
        next: 5,
    };
    let mut content = Content::new();
    let mut resources = Resources::default();
    let width = tree.size().width() * 0.75;
    let height = tree.size().height() * 0.75;
    content.transform([0.75, 0.0, 0.0, -0.75, 0.0, height]);
    writer.group(tree.root(), &mut content, &mut resources)?;
    writer.pdf.catalog(Ref::new(1)).pages(Ref::new(2));
    writer.pdf.pages(Ref::new(2)).kids([Ref::new(3)]).count(1);
    let mut page = writer.pdf.page(Ref::new(3));
    page.parent(Ref::new(2))
        .media_box(Rect::new(0.0, 0.0, width, height))
        .contents(Ref::new(4));
    resources.write(page.resources());
    page.finish();
    writer.pdf.stream(Ref::new(4), &content.finish());
    Ok(writer.pdf.finish())
}
