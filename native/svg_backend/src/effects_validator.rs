//! Count the exact pattern pixmaps before the renderer allocates them.
//! The configured cumulative ceiling belongs to the Elixir Limits authority.
pub fn pattern_pixels(
    group: &usvg::Group,
    transform: usvg::Transform,
    ceiling: u64,
    canvas_pixels: u64,
) -> Result<u64, String> {
    let layer_pixels = group
        .layer_bounding_box()
        .transform(transform)
        .map(|bbox| {
            let width = (bbox.width().ceil() as u64).saturating_add(4);
            let height = (bbox.height().ceil() as u64).saturating_add(4);
            width
                .saturating_mul(height)
                .min(canvas_pixels.saturating_mul(25))
        })
        .unwrap_or(0);
    let mut total = match group.mask() {
        Some(mask) => mask_pixels(mask, transform, ceiling, canvas_pixels, layer_pixels)?,
        None => 0,
    };
    // Pinned resvg caps a layer at five canvas widths/heights, 25x area.
    // These fixed multipliers count backend allocations, not tunable ceilings.
    if group.clip_path().is_some() || (group.mask().is_none() && group.opacity().get() != 1.0) {
        total = total.saturating_add(layer_pixels.saturating_mul(3));
    }
    if total > ceiling {
        return Err("max_aggregate_svg_surface_pixels".into());
    }
    for node in group.children() {
        let amount = match node {
            usvg::Node::Group(child) => pattern_pixels(
                child,
                transform.pre_concat(child.transform()),
                ceiling,
                canvas_pixels,
            )?,
            usvg::Node::Text(text) => {
                pattern_pixels(text.flattened(), transform, ceiling, canvas_pixels)?
            }
            usvg::Node::Path(path) => {
                let mut pixels = 0u64;
                for paint in path
                    .fill()
                    .map(|f| f.paint())
                    .into_iter()
                    .chain(path.stroke().map(|s| s.paint()))
                {
                    if let usvg::Paint::Pattern(pattern) = paint {
                        let (sx, sy) = transform.pre_concat(pattern.transform()).get_scale();
                        let rect = pattern.rect();
                        // Mirror pinned resvg 0.47's round and u32 tile dimensions.
                        let width = (rect.width() * sx).round() as u32;
                        let height = (rect.height() * sy).round() as u32;
                        pixels = pixels
                            .saturating_add(u64::from(width).saturating_mul(u64::from(height)));
                        if pixels > ceiling {
                            return Err("max_aggregate_svg_surface_pixels".into());
                        }
                        pixels = pixels.saturating_add(pattern_pixels(
                            pattern.root(),
                            usvg::Transform::from_scale(sx, sy),
                            ceiling,
                            u64::from(width).saturating_mul(u64::from(height)),
                        )?);
                    }
                }
                pixels
            }
            usvg::Node::Image(image) => match image.kind() {
                usvg::ImageKind::SVG(tree) => canvas_pixels.saturating_add(pattern_pixels(
                    tree.root(),
                    transform,
                    ceiling,
                    canvas_pixels,
                )?),
                _ => 0,
            },
        };
        total = total.saturating_add(amount);
        if total > ceiling {
            return Err("max_aggregate_svg_surface_pixels".into());
        }
    }
    Ok(total)
}

fn mask_pixels(
    mask: &usvg::Mask,
    transform: usvg::Transform,
    ceiling: u64,
    canvas_pixels: u64,
    layer_pixels: u64,
) -> Result<u64, String> {
    // Group pixmap, mask pixmap, alpha region and converted mask may coexist.
    let mut total = layer_pixels.saturating_mul(4);
    if total > ceiling {
        return Err("max_aggregate_svg_surface_pixels".into());
    }
    total = total.saturating_add(pattern_pixels(
        mask.root(),
        transform,
        ceiling,
        canvas_pixels,
    )?);
    if let Some(parent) = mask.mask() {
        total = total.saturating_add(mask_pixels(
            parent,
            transform,
            ceiling,
            canvas_pixels,
            layer_pixels,
        )?);
    }
    if total > ceiling {
        return Err("max_aggregate_svg_surface_pixels".into());
    }
    Ok(total)
}

/// Local sampling density preserves the resolved ancestor/viewBox scale.
pub fn filter_scale(group: &usvg::Group, root_scale: (f32, f32)) -> (f32, f32) {
    let (sx, sy) = group.abs_transform().get_scale();
    (sx * root_scale.0, sy * root_scale.1)
}

/// Reserves all effect canvases/results and parameter-dependent pixel visits.
/// The factors include RGBA scratch buffers, straight RGB/alpha export and PDF
/// stream copies, counted before any of those buffers are allocated.
pub fn filter_work(
    group: &usvg::Group,
    root_scale: (f32, f32),
    pixel_ceiling: u64,
    work_ceiling: u64,
) -> Result<(u64, u64), String> {
    let mut pixels = 0u64;
    let mut work = 0u64;
    let (sx, sy) = filter_scale(group, root_scale);
    if !group.filters().is_empty() {
        let bbox = group.layer_bounding_box();
        let canvas = area(bbox.width(), bbox.height(), sx, sy);
        pixels = canvas.saturating_mul(12);
        work = canvas;
        for filter in group.filters() {
            let rect = filter.rect();
            let region = area(rect.width(), rect.height(), sx, sy);
            for primitive in filter.primitives() {
                // Each retained result and both cloned inputs can coexist with
                // conversion, clipping, algorithm scratch and output buffers.
                pixels = pixels.saturating_add(region.max(canvas).saturating_mul(8));
                let factor = match primitive.kind() {
                    usvg::filter::Kind::ConvolveMatrix(fe) => u64::from(fe.matrix().columns())
                        .saturating_mul(u64::from(fe.matrix().rows()))
                        .saturating_mul(4),
                    usvg::filter::Kind::Morphology(fe) => {
                        let columns = ((fe.radius_x().get() * sx).ceil() as u64)
                            .saturating_mul(2)
                            .min((rect.width() * sx).ceil() as u64);
                        let rows = ((fe.radius_y().get() * sy).ceil() as u64)
                            .saturating_mul(2)
                            .min((rect.height() * sy).ceil() as u64);
                        columns.saturating_mul(rows).saturating_mul(4)
                    }
                    usvg::filter::Kind::Turbulence(fe) => {
                        u64::from(fe.num_octaves()).saturating_mul(64)
                    }
                    usvg::filter::Kind::DiffuseLighting(_)
                    | usvg::filter::Kind::SpecularLighting(_) => 256,
                    usvg::filter::Kind::GaussianBlur(_) | usvg::filter::Kind::DropShadow(_) => 128,
                    _ => 64,
                };
                work = work.saturating_add(region.saturating_mul(factor.max(1)));
                // Pinned box blur initializes each row/column over its radius.
                let blur = match primitive.kind() {
                    usvg::filter::Kind::GaussianBlur(fe) => {
                        Some((fe.std_dev_x().get(), fe.std_dev_y().get()))
                    }
                    usvg::filter::Kind::DropShadow(fe) => {
                        Some((fe.std_dev_x().get(), fe.std_dev_y().get()))
                    }
                    _ => None,
                };
                if let Some((x, y)) = blur {
                    work = work.saturating_add(
                        ((x * sx + y * sy).ceil() as u64)
                            .saturating_mul((rect.width() * sx + rect.height() * sy).ceil() as u64)
                            .saturating_mul(20),
                    );
                }
                check(pixels, work, pixel_ceiling, work_ceiling)?;
            }
        }
        // Includes pattern, clip, mask, opacity and nested render buffers used
        // while producing the true SourceGraphic before the ancestor filter.
        pixels = pixels.saturating_add(pattern_pixels(
            group,
            usvg::Transform::from_scale(sx, sy),
            pixel_ceiling,
            canvas,
        )?);
    }
    if let Some(mask) = group.mask() {
        let mut current = Some(mask);
        while let Some(mask) = current {
            let result = filter_work(mask.root(), root_scale, pixel_ceiling, work_ceiling)?;
            pixels = pixels.saturating_add(result.0);
            work = work.saturating_add(result.1);
            current = mask.mask();
        }
    }
    for node in group.children() {
        let mut visit = |child: &usvg::Group| -> Result<(), String> {
            let result = filter_work(child, root_scale, pixel_ceiling, work_ceiling)?;
            pixels = pixels.saturating_add(result.0);
            work = work.saturating_add(result.1);
            Ok(())
        };
        match node {
            usvg::Node::Group(child) => visit(child)?,
            usvg::Node::Text(text) => visit(text.flattened())?,
            usvg::Node::Path(path) => {
                for paint in path
                    .fill()
                    .map(|f| f.paint())
                    .into_iter()
                    .chain(path.stroke().map(|s| s.paint()))
                {
                    if let usvg::Paint::Pattern(pattern) = paint {
                        visit(pattern.root())?;
                    }
                }
            }
            usvg::Node::Image(image) => {
                if let usvg::ImageKind::SVG(tree) = image.kind() {
                    let scale = image.abs_transform().get_scale();
                    let (child_pixels, child_work) = filter_work(
                        tree.root(),
                        (root_scale.0 * scale.0, root_scale.1 * scale.1),
                        pixel_ceiling,
                        work_ceiling,
                    )?;
                    pixels = pixels.saturating_add(child_pixels);
                    work = work.saturating_add(child_work);
                }
            }
        }
        check(pixels, work, pixel_ceiling, work_ceiling)?;
    }
    check(pixels, work, pixel_ceiling, work_ceiling)?;
    Ok((pixels, work))
}

fn area(width: f32, height: f32, sx: f32, sy: f32) -> u64 {
    ((width * sx).ceil() as u64).saturating_mul((height * sy).ceil() as u64)
}
fn check(pixels: u64, work: u64, pixel_ceiling: u64, work_ceiling: u64) -> Result<(), String> {
    if pixels > pixel_ceiling {
        return Err("max_aggregate_svg_surface_pixels".into());
    }
    if work > work_ceiling {
        return Err("max_svg_filter_work".into());
    }
    Ok(())
}
