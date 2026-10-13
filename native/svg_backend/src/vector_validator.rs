//! Work validation before either raster or vector stroke expansion.
use usvg::tiny_skia_path::{PathSegment, Point};

pub fn work(group: &usvg::Group, ceiling: u64) -> Result<u64, String> {
    let mut total = match group.mask() {
        Some(mask) => mask_work(mask, ceiling)?,
        None => 0,
    };
    total = total.saturating_add(match group.clip_path() {
        Some(clip) => work(clip.root(), ceiling)?,
        None => 0,
    });
    for node in group.children() {
        let amount = match node {
            usvg::Node::Group(group) => work(group, ceiling)?,
            usvg::Node::Text(text) => work(text.flattened(), ceiling)?,
            usvg::Node::Image(image) => match image.kind() {
                usvg::ImageKind::SVG(tree) => work(tree.root(), ceiling)?,
                _ => 0,
            },
            usvg::Node::Path(path) => {
                let mut length = 0.0f64;
                let mut previous = Point::from_xy(0.0, 0.0);
                let mut start = previous;
                let mut segments = 0u64;
                for segment in path.data().segments() {
                    segments += 1;
                    let points = match segment {
                        PathSegment::MoveTo(p) => {
                            previous = p;
                            start = p;
                            vec![]
                        }
                        PathSegment::LineTo(p) => vec![p],
                        PathSegment::QuadTo(a, b) => vec![a, b],
                        PathSegment::CubicTo(a, b, c) => vec![a, b, c],
                        PathSegment::Close => vec![start],
                    };
                    // A Bezier's control polygon bounds its arc length.
                    for p in points {
                        length += ((p.x as f64 - previous.x as f64).powi(2)
                            + (p.y as f64 - previous.y as f64).powi(2))
                        .sqrt();
                        previous = p;
                    }
                }
                let repetitions = path
                    .stroke()
                    .and_then(|stroke| stroke.dasharray())
                    .map(|dash| {
                        let period: f64 = dash.iter().map(|n| *n as f64).sum();
                        ((length / period).ceil() + 1.0) * dash.len() as f64
                    })
                    .unwrap_or(1.0);
                let amount = segments as f64 * repetitions;
                if !amount.is_finite() || amount > ceiling as f64 {
                    return Err("max_svg_stroke_work".into());
                }
                let mut total = amount.ceil() as u64;
                for paint in path
                    .fill()
                    .map(|f| f.paint())
                    .into_iter()
                    .chain(path.stroke().map(|s| s.paint()))
                {
                    if let usvg::Paint::Pattern(pattern) = paint {
                        total = total.saturating_add(work(pattern.root(), ceiling)?);
                    }
                }
                total
            }
        };
        total = total.saturating_add(amount);
        if total > ceiling {
            return Err("max_svg_stroke_work".into());
        }
    }
    Ok(total)
}

fn mask_work(mask: &usvg::Mask, ceiling: u64) -> Result<u64, String> {
    let mut total = work(mask.root(), ceiling)?;
    if let Some(parent) = mask.mask() {
        total = total.saturating_add(mask_work(parent, ceiling)?);
    }
    if total > ceiling {
        return Err("max_svg_stroke_work".into());
    }
    Ok(total)
}

/// Counts functions needed for every prepared fill/stroke gradient occurrence.
pub fn gradient_work(group: &usvg::Group, ceiling: u64) -> Result<u64, String> {
    let mut total = match group.mask() {
        Some(mask) => mask_gradient_work(mask, ceiling)?,
        None => 0,
    };
    for node in group.children() {
        total = total.saturating_add(match node {
            usvg::Node::Group(group) => gradient_work(group, ceiling)?,
            usvg::Node::Text(text) => gradient_work(text.flattened(), ceiling)?,
            usvg::Node::Image(image) => match image.kind() {
                usvg::ImageKind::SVG(tree) => gradient_work(tree.root(), ceiling)?,
                _ => 0,
            },
            usvg::Node::Path(path) => {
                let paints = path
                    .fill()
                    .map(|fill| fill.paint())
                    .into_iter()
                    .chain(path.stroke().map(|stroke| stroke.paint()));
                paints
                    .map(|paint| {
                        Ok(match paint {
                            usvg::Paint::LinearGradient(g) => g.stops().len() as u64,
                            usvg::Paint::RadialGradient(g) => g.stops().len() as u64,
                            usvg::Paint::Pattern(p) => gradient_work(p.root(), ceiling)?,
                            _ => 0,
                        })
                    })
                    .collect::<Result<Vec<u64>, String>>()?
                    .into_iter()
                    .sum()
            }
        });
        if total > ceiling {
            return Err("max_svg_gradient_work".into());
        }
    }
    Ok(total)
}

fn mask_gradient_work(mask: &usvg::Mask, ceiling: u64) -> Result<u64, String> {
    let mut total = gradient_work(mask.root(), ceiling)?;
    if let Some(parent) = mask.mask() {
        total = total.saturating_add(mask_gradient_work(parent, ceiling)?);
    }
    if total > ceiling {
        return Err("max_svg_gradient_work".into());
    }
    Ok(total)
}

/// Issued cluster plans and CMaps over one immutable prepared usvg tree.
#[derive(Default)]
pub struct PreparedText<'a> {
    pub mappings:
        std::collections::HashMap<usvg::fontdb::ID, std::collections::BTreeMap<u16, String>>,
    pub clusters: std::collections::HashMap<usize, Vec<Cluster<'a>>>,
}

/// Original Unicode belongs to the complete shaped cluster. The painter
/// consumes these plans without reinterpreting input or selection rules.
pub fn text<'a>(
    group: &'a usvg::Group,
    database: &usvg::fontdb::Database,
) -> Result<PreparedText<'a>, String> {
    text_with_surfaces(group, database, &std::collections::HashSet::new())
}

pub fn text_with_surfaces<'a>(
    group: &'a usvg::Group,
    database: &usvg::fontdb::Database,
    surfaces: &std::collections::HashSet<String>,
) -> Result<PreparedText<'a>, String> {
    let mut prepared = PreparedText::default();
    if let Some(mask) = group.mask() {
        let mut current = Some(mask);
        while let Some(mask) = current {
            let children = text_with_surfaces(mask.root(), database, surfaces)?;
            for (font, glyphs) in children.mappings {
                prepared
                    .mappings
                    .entry(font)
                    .or_insert_with(std::collections::BTreeMap::new)
                    .extend(glyphs);
            }
            prepared.clusters.extend(children.clusters);
            current = mask.mask();
        }
    }
    for node in group.children() {
        match node {
            usvg::Node::Group(group) => {
                let children = text_with_surfaces(group, database, surfaces)?;
                for (font, glyphs) in children.mappings {
                    prepared
                        .mappings
                        .entry(font)
                        .or_insert_with(std::collections::BTreeMap::new)
                        .extend(glyphs);
                }
                prepared.clusters.extend(children.clusters);
            }
            usvg::Node::Text(text_node) => {
                if text_node.writing_mode() != usvg::WritingMode::LeftToRight {
                    return Err(
                        "SVG vertical text requires a complete positioned glyph adapter".into(),
                    );
                }
                for span in text_node.layouted() {
                    if span.stroke.is_some()
                        || span
                            .fill
                            .as_ref()
                            .is_some_and(|fill| !matches!(fill.paint(), usvg::Paint::Color(_)))
                        || span.underline.is_some()
                        || span.overline.is_some()
                        || span.line_through.is_some()
                    {
                        return Err("SVG text currently requires solid RGB fill without stroke or decorations; native gradient/stroke text paint remains pending".into());
                    }
                    let clusters = clusters(span, database)?;
                    for cluster in &clusters {
                        for glyph in cluster.glyphs {
                            if glyph.id.0 == 0 {
                                return Err("selected SVG font shaping produced a missing glyph; fallback is disabled".into());
                            }
                            let entry = prepared
                                .mappings
                                .entry(glyph.font)
                                .or_insert_with(std::collections::BTreeMap::new)
                                .entry(glyph.id.0)
                                .or_insert_with(String::new);
                            if entry.is_empty() {
                                *entry = cluster.text.to_string();
                            }
                        }
                    }
                    prepared
                        .clusters
                        .insert(span as *const usvg::layout::Span as usize, clusters);
                }
                // Verify requested lengths against actual selected glyphs.
                // Bind every authored byte range to its corresponding native
                // layout span before accepting independent tspan adjustments.
                let has_lengths = text_node
                    .chunks()
                    .iter()
                    .flat_map(|chunk| chunk.spans())
                    .any(|span| span.text_length().is_some());
                if has_lengths {
                    let source_count: usize = text_node
                        .chunks()
                        .iter()
                        .map(|chunk| chunk.spans().len())
                        .sum();
                    if source_count != text_node.layouted().len()
                        || text_node.rotate().iter().any(|angle| *angle != 0.0)
                    {
                        return Err("SVG textLength requires complete rotated or unmatched span length distribution for this scene".into());
                    }
                    let mut layout_index = 0;
                    for chunk in text_node.chunks() {
                        for source in chunk.spans() {
                            let layout = &text_node.layouted()[layout_index];
                            layout_index += 1;
                            let Some(target) = source.text_length() else {
                                continue;
                            };
                            if !matches!(chunk.text_flow(), usvg::TextFlow::Linear) {
                                return Err("SVG path textLength requires complete curved length distribution".into());
                            }
                            if source.length_adjust() == usvg::LengthAdjust::SpacingAndGlyphs
                                && chunk.spans().len() > 1
                            {
                                return Err("SVG scaled independent spans require complete prefix and following-cursor distribution".into());
                            }
                            let original = chunk
                                .text()
                                .get(source.start()..source.end())
                                .ok_or("SVG length span lost its original UTF-8 range")?;
                            let glyphs = &layout.positioned_glyphs;
                            let shaped: String =
                                glyphs.iter().map(|glyph| glyph.text.as_str()).collect();
                            if original != shaped {
                                return Err("SVG textLength span does not preserve its original Unicode range".into());
                            }
                            if let (Some(first), Some(last)) = (glyphs.first(), glyphs.last()) {
                                let a = first.outline_transform();
                                let actual = glyph_end(last, database)? - a.tx;
                                // A single original scalar has no inter-character
                                // gap for spacing adjustment. Its glyph stays exact.
                                let no_gap = source.length_adjust() == usvg::LengthAdjust::Spacing
                                    && original.chars().count() == 1;
                                // Fixed f32 precision invariant, not a work ceiling.
                                let tolerance =
                                    f32::EPSILON * target.abs().max(actual.abs()).max(1.0) * 16.0;
                                if !actual.is_finite()
                                    || (!no_gap && (actual - target).abs() > tolerance)
                                {
                                    return Err("SVG textLength requested advance is not preserved by selected native glyph geometry; complete length adjustment is required".into());
                                }
                            }
                        }
                    }
                }
                let logical: String = text_node
                    .chunks()
                    .iter()
                    .map(|chunk| chunk.text())
                    .collect();
                let shaped: String = text_node
                    .layouted()
                    .iter()
                    .flat_map(|span| &span.positioned_glyphs)
                    .map(|glyph| glyph.text.as_str())
                    .collect();
                if logical != shaped {
                    return Err("SVG positioned glyph clusters do not preserve source Unicode order; bidi/unsupported shaping requires a complete logical adapter".into());
                }
                // Pinned usvg applies relative shifts only at a shaped cluster's
                // first codepoint. Nonzero continuation offsets would silently
                // change later glyph geometry, so the initial list profile must
                // reject them before either inline paint mode executes.
                if text_node
                    .dx()
                    .iter()
                    .chain(text_node.dy())
                    .any(|offset| *offset != 0.0)
                {
                    let scalar_count = logical.chars().count();
                    if text_node.dx().len() != scalar_count || text_node.dy().len() != scalar_count
                    {
                        return Err("SVG relative positions require complete original Unicode/codepoint mapping; hidden or metadata offsets need a complete positioning adapter".into());
                    }
                    let mut offset = 0;
                    for glyph in text_node
                        .layouted()
                        .iter()
                        .flat_map(|span| &span.positioned_glyphs)
                    {
                        let count = glyph.text.chars().count();
                        for index in offset + usize::from(count > 0)..offset + count {
                            if text_node.dx()[index] != 0.0 || text_node.dy()[index] != 0.0 {
                                return Err("SVG nonzero relative offsets inside a shaped Unicode cluster require a complete character-positioning adapter".into());
                            }
                        }
                        offset += count;
                    }
                }
                if text_node.rotate().iter().any(|angle| *angle != 0.0) {
                    if text_node.rotate().len() != logical.chars().count() {
                        return Err("SVG rotations require complete original Unicode/codepoint mapping; hidden or metadata angles need a complete positioning adapter".into());
                    }
                    let mut offset = 0;
                    for glyph in text_node
                        .layouted()
                        .iter()
                        .flat_map(|span| &span.positioned_glyphs)
                    {
                        let count = glyph.text.chars().count();
                        for index in offset + usize::from(count > 0)..offset + count {
                            if text_node.rotate()[index] != text_node.rotate()[offset] {
                                return Err("SVG differing rotations inside a shaped Unicode cluster require complete character-positioned glyph mapping".into());
                            }
                        }
                        offset += count;
                    }
                }
                // The admitted font profile cannot contain color/raster glyphs.
                text(text_node.flattened(), database)?;
            }
            usvg::Node::Path(path) => {
                for paint in path
                    .fill()
                    .map(|f| f.paint())
                    .into_iter()
                    .chain(path.stroke().map(|s| s.paint()))
                {
                    if let usvg::Paint::Pattern(pattern) = paint {
                        let children = text_with_surfaces(pattern.root(), database, surfaces)?;
                        for (font, glyphs) in children.mappings {
                            prepared
                                .mappings
                                .entry(font)
                                .or_insert_with(std::collections::BTreeMap::new)
                                .extend(glyphs);
                        }
                        prepared.clusters.extend(children.clusters);
                    }
                }
            }
            usvg::Node::Image(image) if surfaces.contains(group.id()) && image.id().is_empty() => {}
            usvg::Node::Image(image) => {
                if let usvg::ImageKind::SVG(tree) = image.kind() {
                    let children = text_with_surfaces(tree.root(), database, surfaces)?;
                    for (font, glyphs) in children.mappings {
                        prepared
                            .mappings
                            .entry(font)
                            .or_insert_with(std::collections::BTreeMap::new)
                            .extend(glyphs);
                    }
                    prepared.clusters.extend(children.clusters);
                }
            }
        }
    }
    Ok(prepared)
}

/// Actual selected face advance at this glyph's issued matrix.
pub(crate) fn glyph_end(
    glyph: &usvg::layout::PositionedGlyph,
    database: &usvg::fontdb::Database,
) -> Result<f32, String> {
    let advance = database
        .with_face_data(glyph.font, |data, index| {
            ttf_parser::Face::parse(data, index)
                .ok()
                .and_then(|face| face.glyph_hor_advance(glyph.id))
        })
        .flatten()
        .ok_or("selected SVG glyph lost its actual font advance")? as f32;
    let t = glyph.outline_transform();
    Ok(t.tx + t.sx * advance)
}

/// One original shaping cluster and all real glyphs that paint it.
#[derive(Clone, Copy)]
pub struct Cluster<'a> {
    pub glyphs: &'a [usvg::layout::PositionedGlyph],
    pub text: &'a str,
}

/// Prepares complete logical replacements. Source-order LTR clusters with
/// non-monotonic offsets need a union selection adapter before admission.
fn clusters<'a>(
    span: &'a usvg::layout::Span,
    database: &usvg::fontdb::Database,
) -> Result<Vec<Cluster<'a>>, String> {
    let mut clusters = vec![];
    let mut start = 0;
    for (index, glyph) in span.positioned_glyphs.iter().enumerate() {
        if glyph.text.is_empty() {
            continue;
        }
        let glyphs = &span.positioned_glyphs[start..=index];
        let first = glyphs[0].outline_transform();
        let mut maximum_end = first.tx;
        let mut last_end = first.tx;
        for (glyph_index, glyph) in glyphs.iter().enumerate() {
            let t = glyph.outline_transform();
            if ![
                t.sx,
                t.ky,
                t.kx,
                t.sy,
                t.tx,
                t.ty,
                glyph.font_size(),
                1.0 / glyph.font_size(),
            ]
            .iter()
            .all(|value| value.is_finite())
            {
                return Err(
                    "SVG selected glyph positions require finite native text matrices".into(),
                );
            }
            let advance = database.with_face_data(glyph.font, |data, index| {
                let face = ttf_parser::Face::parse(data, index).map_err(|_| "selected SVG glyph face no longer parses")?;
                let scale = face.units_per_em() as f32 / glyph.font_size();
                if ![t.sx * scale, t.ky * scale, t.kx * scale, t.sy * scale].iter().all(|value| value.is_finite()) {
                    return Err("SVG selected glyph normalization requires finite native text matrices");
                }
                let tables = face.tables();
                if tables.colr.is_some() || tables.svg.is_some() || tables.cbdt.is_some() || tables.sbix.is_some() {
                    return Err("SVG text color/raster glyph fonts require a complete glyph-paint adapter");
                }
                Ok(face.glyph_hor_advance(glyph.id).unwrap_or(0) as f32)
            }).ok_or("selected SVG glyph lost its byte-backed face")??;
            if glyph_index > 0 && advance != 0.0 {
                return Err("SVG multi-glyph cluster with nonzero continuation advances requires complete shaped selection bounds; this cluster is outside the initial text profile".into());
            }
            last_end = t.tx + t.sx * advance;
            maximum_end = maximum_end.max(last_end);
            if glyphs.len() > 1
                && (glyph.font != glyphs[0].font
                    || glyph.font_size() != glyphs[0].font_size()
                    || t.sx != first.sx
                    || t.sy != first.sy
                    || t.kx != first.kx
                    || t.ky != first.ky
                    || t.ty != first.ty
                    || t.tx < first.tx)
            {
                return Err("SVG multi-glyph cluster offsets require complete logical selection bounds; this cluster is outside the initial text profile".into());
            }
        }
        if glyphs.len() > 1 && last_end < maximum_end {
            return Err("SVG multi-glyph cluster endpoints require complete logical selection bounds; this cluster is outside the initial text profile".into());
        }
        clusters.push(Cluster {
            glyphs,
            text: &glyph.text,
        });
        start = index + 1;
    }
    if start != span.positioned_glyphs.len() {
        return Err("SVG glyph cluster has no matching original Unicode replacement".into());
    }
    Ok(clusters)
}
