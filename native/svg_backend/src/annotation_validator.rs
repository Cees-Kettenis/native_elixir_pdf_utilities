//! Original anchor scopes and real painted geometry, prepared before PDF emission.
use std::collections::HashMap;
use usvg::tiny_skia_path::{Rect, Transform};

pub struct Link {
    pub anchor: String,
    pub index: usize,
    pub destination: String,
    pub rect: [f32; 4],
}
pub struct Plan {
    pub links: Vec<Link>,
    pub targets: HashMap<String, [f32; 4]>,
    pub work: u64,
    pub boxes: u64,
}

pub fn prepare(source: &str, tree: &usvg::Tree, work: u64, boxes: u64) -> Result<Plan, String> {
    let xml = usvg::roxmltree::Document::parse_with_options(
        source,
        usvg::roxmltree::ParsingOptions {
            allow_dtd: false,
            ..Default::default()
        },
    )
    .map_err(|error| error.to_string())?;
    let mut anchors = HashMap::new();
    let mut source_work = 0;
    let mut index = 0;
    for node in xml.descendants().filter(|node| node.is_element()) {
        source_work += 1;
        if source_work > work {
            return Err("max_svg_expansion_work".into());
        }
        if node.tag_name().name() == "a"
            && matches!(
                node.tag_name().namespace(),
                None | Some("http://www.w3.org/2000/svg")
            )
        {
            if let Some(href) = node
                .attribute("href")
                .or_else(|| node.attribute(("http://www.w3.org/1999/xlink", "href")))
            {
                let id = node
                    .attribute("id")
                    .ok_or("issued SVG anchor lost its original source identity")?;
                if anchors.len() as u64 >= boxes {
                    return Err("max_layout_boxes".into());
                }
                anchors.insert(id.to_string(), (href.to_string(), index));
                index += 1;
            }
        }
    }
    let width = tree.size().width() * 0.75;
    let height = tree.size().height() * 0.75;
    let mut plan = Plan {
        links: vec![],
        targets: HashMap::new(),
        work: source_work,
        boxes: 0,
    };
    walk(
        tree.root(),
        tree.fontdb(),
        &anchors,
        None,
        Some([0.0, 0.0, width, height]),
        Transform::from_row(0.75, 0.0, 0.0, -0.75, 0.0, height),
        1.0,
        work,
        boxes,
        &mut plan,
    )?;
    if let Some(id) = xml.root_element().attribute("id") {
        target(&mut plan, id, [0.0, 0.0, width, height], boxes)?;
    }
    Ok(plan)
}

fn target(plan: &mut Plan, id: &str, rect: [f32; 4], ceiling: u64) -> Result<(), String> {
    if let Some(old) = plan.targets.get_mut(id) {
        old[0] = old[0].min(rect[0]);
        old[1] = old[1].min(rect[1]);
        old[2] = old[2].max(rect[2]);
        old[3] = old[3].max(rect[3]);
    } else {
        if plan.boxes >= ceiling {
            return Err("max_layout_boxes".into());
        }
        plan.boxes += 1;
        plan.targets.insert(id.to_string(), rect);
    }
    Ok(())
}

fn charge(plan: &mut Plan, ceiling: u64) -> Result<(), String> {
    plan.work += 1;
    if plan.work > ceiling {
        return Err("max_svg_expansion_work".into());
    }
    Ok(())
}

fn intersect(a: [f32; 4], b: [f32; 4]) -> Option<[f32; 4]> {
    let r = [
        a[0].max(b[0]),
        a[1].max(b[1]),
        a[2].min(b[2]),
        a[3].min(b[3]),
    ];
    if r[2] > r[0] && r[3] > r[1] {
        Some(r)
    } else {
        None
    }
}

fn mapped(rect: Rect, transform: Transform) -> Result<[f32; 4], String> {
    let rect = rect
        .transform(transform)
        .ok_or("SVG annotation geometry lost its finite affine bounds")?;
    Ok([rect.left(), rect.top(), rect.right(), rect.bottom()])
}

fn add(
    plan: &mut Plan,
    anchor: Option<&str>,
    anchors: &HashMap<String, (String, usize)>,
    bounds: [f32; 4],
    clip: Option<[f32; 4]>,
    boxes: u64,
) -> Result<(), String> {
    let Some(anchor) = anchor else {
        return Ok(());
    };
    let Some(clip) = clip else {
        return Ok(());
    };
    let Some(rect) = intersect(bounds, clip) else {
        return Ok(());
    };
    if plan.boxes >= boxes {
        return Err("max_layout_boxes".into());
    }
    plan.boxes += 1;
    plan.links.push(Link {
        anchor: anchor.to_string(),
        destination: anchors[anchor].0.clone(),
        index: anchors[anchor].1,
        rect,
    });
    Ok(())
}

fn visible_clip(group: &usvg::Group, ceiling: u64, plan: &mut Plan) -> Result<bool, String> {
    let mut visible = false;
    charge(plan, ceiling)?;
    for node in group.children() {
        charge(plan, ceiling)?;
        match node {
            usvg::Node::Group(child) => visible |= visible_clip(child, ceiling, plan)?,
            usvg::Node::Path(path) => visible |= path.is_visible(),
            _ => {}
        }
    }
    Ok(visible)
}

fn walk(
    group: &usvg::Group,
    database: &usvg::fontdb::Database,
    anchors: &HashMap<String, (String, usize)>,
    inherited: Option<&str>,
    mut clip: Option<[f32; 4]>,
    transform: Transform,
    opacity: f32,
    work: u64,
    boxes: u64,
    plan: &mut Plan,
) -> Result<(), String> {
    charge(plan, work)?;
    let transform = transform.pre_concat(group.transform());
    let opacity = opacity * group.opacity().get();
    if opacity == 0.0 || clip.is_none() {
        return Ok(());
    }
    let anchor = if anchors.contains_key(group.id()) {
        Some(group.id())
    } else {
        inherited
    };
    if let Some(path) = group.clip_path() {
        // Hidden clip geometry has an empty painted region, even when its
        // syntactic usvg bounding box is nonempty.
        let visible = visible_clip(path.root(), work, plan)?;
        if !visible {
            return Ok(());
        }
        let bounds = mapped(
            path.root().bounding_box(),
            transform.pre_concat(path.transform()),
        )?;
        clip = clip.and_then(|parent| intersect(parent, bounds));
    }
    if !group.id().is_empty() {
        let bounds = mapped(group.bounding_box(), transform)?;
        if let Some(rect) = clip.and_then(|parent| intersect(parent, bounds)) {
            target(plan, group.id(), rect, boxes)?;
        }
    }
    for node in group.children() {
        charge(plan, work)?;
        match node {
            usvg::Node::Group(child) => walk(
                child, database, anchors, anchor, clip, transform, opacity, work, boxes, plan,
            )?,
            usvg::Node::Path(path) if path.is_visible() => {
                if path.fill().is_some_and(|paint| paint.opacity().get() > 0.0)
                    || path
                        .stroke()
                        .is_some_and(|paint| paint.opacity().get() > 0.0)
                {
                    let rect = if path
                        .stroke()
                        .is_some_and(|paint| paint.opacity().get() > 0.0)
                    {
                        path.stroke_bounding_box()
                    } else {
                        path.bounding_box()
                    };
                    let bounds = mapped(rect, transform)?;
                    if !path.id().is_empty() {
                        if let Some(rect) = clip.and_then(|parent| intersect(parent, bounds)) {
                            target(plan, path.id(), rect, boxes)?;
                        }
                    }
                    add(plan, anchor, anchors, bounds, clip, boxes)?;
                }
            }
            usvg::Node::Text(text) => {
                let t = transform;
                for span in text.layouted().iter().filter(|span| {
                    span.visible
                        && (span
                            .fill
                            .as_ref()
                            .is_some_and(|paint| paint.opacity().get() > 0.0)
                            || span
                                .stroke
                                .as_ref()
                                .is_some_and(|paint| paint.opacity().get() > 0.0))
                }) {
                    for glyph in &span.positioned_glyphs {
                        charge(plan, work)?;
                        let bounds = database
                            .with_face_data(glyph.font, |bytes, index| {
                                let face = ttf_parser::Face::parse(bytes, index).map_err(|_| {
                                    "selected annotation glyph lost its approved face"
                                })?;
                                Ok::<_, &str>(face.glyph_bounding_box(glyph.id))
                            })
                            .ok_or("selected annotation glyph lost its byte-backed face")??;
                        if let Some(b) = bounds {
                            let rect = Rect::from_ltrb(
                                b.x_min as f32,
                                b.y_min as f32,
                                b.x_max as f32,
                                b.y_max as f32,
                            )
                            .ok_or("selected annotation glyph has invalid ink bounds")?;
                            let bounds = if let Some(stroke) = span
                                .stroke
                                .as_ref()
                                .filter(|paint| paint.opacity().get() > 0.0)
                            {
                                let program = crate::glyph_program_validator::prepare(
                                    glyph,
                                    Some(stroke),
                                    database,
                                )?;
                                let stroke_rect = Rect::from_ltrb(
                                    program.bbox[0],
                                    program.bbox[1],
                                    program.bbox[2],
                                    program.bbox[3],
                                );
                                let fill_bounds = span
                                    .fill
                                    .as_ref()
                                    .filter(|paint| paint.opacity().get() > 0.0)
                                    .map(|_| mapped(rect, t.pre_concat(glyph.outline_transform())))
                                    .transpose()?;
                                let stroke_bounds = stroke_rect
                                    .map(|rect| {
                                        mapped(
                                            rect,
                                            t.pre_concat(glyph.outline_transform()).pre_scale(
                                                program.units as f32 / 1000.0,
                                                program.units as f32 / 1000.0,
                                            ),
                                        )
                                    })
                                    .transpose()?;
                                match (fill_bounds, stroke_bounds) {
                                    (Some(a), Some(b)) => [
                                        a[0].min(b[0]),
                                        a[1].min(b[1]),
                                        a[2].max(b[2]),
                                        a[3].max(b[3]),
                                    ],
                                    (Some(a), None) | (None, Some(a)) => a,
                                    (None, None) => continue,
                                }
                            } else {
                                mapped(rect, t.pre_concat(glyph.outline_transform()))?
                            };
                            if !text.id().is_empty() {
                                if let Some(rect) =
                                    clip.and_then(|parent| intersect(parent, bounds))
                                {
                                    target(plan, text.id(), rect, boxes)?;
                                }
                            }
                            add(plan, anchor, anchors, bounds, clip, boxes)?;
                        }
                    }
                }
            }
            usvg::Node::Image(image) if image.is_visible() => {
                let bounds = mapped(image.bounding_box(), transform)?;
                if !image.id().is_empty() {
                    if let Some(rect) = clip.and_then(|parent| intersect(parent, bounds)) {
                        target(plan, image.id(), rect, boxes)?;
                    }
                }
                add(plan, anchor, anchors, bounds, clip, boxes)?;
            }
            _ => {}
        }
    }
    Ok(())
}
