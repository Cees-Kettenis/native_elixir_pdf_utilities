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
            _ => 0,
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
