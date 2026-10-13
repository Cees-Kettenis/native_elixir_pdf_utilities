//! Issued source normalization for accepted selected-text length semantics.
//! Original admission, font policy and all ceilings are supplied by Elixir validators.
use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::str::FromStr;
use usvg::roxmltree::{Document, Node};

pub struct PreparedSource {
    pub source: String,
    pub work: u64,
    pub extra_nodes: u64,
}

fn document(source: &str) -> Result<Document<'_>, String> {
    Document::parse_with_options(
        source,
        usvg::roxmltree::ParsingOptions {
            allow_dtd: false,
            ..Default::default()
        },
    )
    .map_err(|error| error.to_string())
}

// Newly issued IDs identify only roots whose length must use selected metrics.
// Every existing original ID, namespace, text byte and reference stays intact.
pub fn identify(source: &str, ceiling: usize) -> Result<PreparedSource, String> {
    let xml = document(source)?;
    let ids: HashSet<_> = xml
        .descendants()
        .filter_map(|node| node.attribute("id"))
        .collect();
    let mut edits = vec![];
    let mut work = 0;
    let mut index = 0;
    for node in xml.descendants().filter(|node| {
        node.is_element()
            && matches!(
                node.tag_name().namespace(),
                None | Some("http://www.w3.org/2000/svg")
            )
    }) {
        work += 1;
        if node.tag_name().name() == "text"
            && node
                .descendants()
                .any(|child| child.is_element() && child.has_attribute("textLength"))
            && !node.has_attribute("id")
        {
            let id = loop {
                index += 1;
                let id = format!("NativeTextLength{index}");
                if !ids.contains(id.as_str()) {
                    break id;
                }
            };
            // New IDs do not have to enter the borrowed set: index is monotonic.
            edits.push((
                insertion(node, source)..insertion(node, source),
                format!(" id=\"{id}\""),
            ));
        }
    }
    let source = apply(source, edits, ceiling)?;
    Ok(PreparedSource {
        source,
        work,
        extra_nodes: 0,
    })
}

fn insertion(node: Node<'_, '_>, source: &str) -> usize {
    node.attributes()
        .map(|attribute| attribute.range().end)
        .max()
        .unwrap_or_else(|| {
            let start = node.range().start + 1;
            start
                + source[start..]
                    .find(|character: char| {
                        character.is_ascii_whitespace() || character == '>' || character == '/'
                    })
                    .unwrap()
        })
}

fn attribute(
    node: Node<'_, '_>,
    name: &str,
    value: String,
    source: &str,
    edits: &mut Vec<(Range<usize>, String)>,
) {
    if let Some(attribute) = node
        .attributes()
        .find(|attribute| attribute.namespace().is_none() && attribute.name() == name)
    {
        edits.push((attribute.range_value(), value));
    } else {
        let at = insertion(node, source);
        edits.push((at..at, format!(" {name}=\"{value}\"")));
    }
}

fn apply(
    source: &str,
    mut edits: Vec<(Range<usize>, String)>,
    ceiling: usize,
) -> Result<String, String> {
    let size = edits
        .iter()
        .try_fold(source.len(), |size, (range, value)| {
            size.checked_sub(range.len())
                .and_then(|size| size.checked_add(value.len()))
        })
        .ok_or("max_svg_bytes")?;
    if size > ceiling {
        return Err("max_svg_bytes".into());
    }
    // Reverse source ranges keep every untouched original byte exact.
    edits.sort_by(|a, b| {
        b.0.start
            .cmp(&a.0.start)
            .then_with(|| b.0.end.cmp(&a.0.end))
    });
    let mut result = source.to_string();
    for (range, value) in edits {
        result.replace_range(range, &value);
    }
    Ok(result)
}

fn texts<'a>(group: &'a usvg::Group, output: &mut HashMap<&'a str, Vec<&'a usvg::Text>>) {
    for node in group.children() {
        match node {
            usvg::Node::Group(group) => texts(group, output),
            usvg::Node::Text(text) => {
                output.entry(text.id()).or_default().push(text);
            }
            _ => {}
        }
    }
}

// Canonical root/viewBox user-space axes come from the exact parsed document.
// Relative nested dimensions requiring a new host context remain diagnosed.
fn viewport_width(node: Node<'_, '_>, tree: &usvg::Tree) -> Result<f64, String> {
    let svg = node
        .ancestors()
        .find(|node| {
            node.is_element() && node.tag_name().name() == "svg"
                || node.is_element() && node.tag_name().name() == "symbol"
        })
        .ok_or("internal text length has no viewport")?;
    if let Some(view_box) = svg.attribute("viewBox") {
        return svgtypes::ViewBox::from_str(view_box)
            .map(|value| value.w)
            .map_err(|_| "internal validated text viewport lost its viewBox".into());
    }
    if svg.parent_element().is_none() {
        return Ok(tree.size().width() as f64);
    }
    let value = svg.attribute("width").unwrap_or("100%");
    let length = svgtypes::Length::from_str(value)
        .map_err(|_| "internal validated viewport width lost its length")?;
    use svgtypes::LengthUnit as Unit;
    let factor = match length.unit {
        Unit::None | Unit::Px => 1.0,
        Unit::Pt => 96.0 / 72.0,
        Unit::Pc => 16.0,
        Unit::In => 96.0,
        Unit::Cm => 96.0 / 2.54,
        Unit::Mm => 96.0 / 25.4,
        Unit::Percent => {
            return Ok(viewport_width(svg.parent_element().unwrap(), tree)? * length.number / 100.0)
        }
        _ => {
            return Err(
                "SVG relative nested textLength viewport requires validated host unit preparation"
                    .into(),
            )
        }
    };
    Ok(length.number * factor)
}

pub fn prepare(source: &str, tree: &usvg::Tree, ceiling: usize) -> Result<PreparedSource, String> {
    let xml = document(source)?;
    let mut roots = HashMap::new();
    texts(tree.root(), &mut roots);
    let mut edits = vec![];
    let mut work = 0;
    let mut extra_nodes = 0;
    for node in xml.descendants().filter(|node| {
        node.is_element()
            && matches!(
                node.tag_name().namespace(),
                None | Some("http://www.w3.org/2000/svg")
            )
            && node.has_attribute("textLength")
    }) {
        work += 1;
        let length = svgtypes::Length::from_str(node.attribute("textLength").unwrap())
            .map_err(|_| "internal validated textLength lost its length")?;
        let percent = if length.unit == svgtypes::LengthUnit::Percent {
            let target = viewport_width(node, tree)? * length.number / 100.0;
            if !target.is_finite() {
                return Err("SVG textLength viewport target requires finite geometry".into());
            }
            attribute(node, "textLength", target.to_string(), source, &mut edits);
            Some(target as f32)
        } else {
            None
        };
        if node.tag_name().name() == "tspan"
            && node.attribute("lengthAdjust").unwrap_or("spacing") == "spacing"
        {
            let parent = node
                .parent_element()
                .ok_or("internal text length span lost its parent")?;
            if parent.tag_name().name() != "text" {
                return Err(
                    "SVG nested adjusted spans require complete ancestral cursor distribution"
                        .into(),
                );
            }
            let children: Vec<_> = parent
                .children()
                .filter(|child| {
                    child.is_text() || (child.is_element() && child.tag_name().name() == "tspan")
                })
                .collect();
            let index = children
                .iter()
                .position(|child| *child == node)
                .ok_or("internal text length span lost its source order")?;
            if let Some(next) = children.get(index + 1) {
                if !next.is_text() {
                    return Err("SVG adjusted spans followed by positioned elements require complete cursor distribution".into());
                }
                let Some(instances) = roots.get(parent.attribute("id").unwrap_or("")) else {
                    continue;
                };
                let mut issued = None;
                for text in instances {
                    if text.chunks().len() != 1
                        || text.chunks()[0].spans().len() != children.len()
                        || text.layouted().len() != children.len()
                    {
                        return Err(
                            "SVG adjusted spans require complete source/whitespace chunk mapping"
                                .into(),
                        );
                    }
                    let chunk = &text.chunks()[0];
                    let range = &chunk.spans()[index];
                    if node.text().unwrap_or("")
                        != chunk.text().get(range.start()..range.end()).unwrap_or("")
                    {
                        return Err(
                            "SVG adjusted span does not preserve its original Unicode source range"
                                .into(),
                        );
                    }
                    let body = &text.layouted()[index].positioned_glyphs;
                    let following = &text.layouted()[index + 1].positioned_glyphs;
                    let (Some(last), Some(first)) = (body.last(), following.first()) else {
                        continue;
                    };
                    let end = crate::vector_validator::glyph_end(last, tree.fontdb())?;
                    let mut gap = first.outline_transform().tx - end;
                    if let (Some(new), Some(old)) = (percent, range.text_length()) {
                        let count = body.iter().filter(|glyph| !glyph.text.is_empty()).count();
                        if count > 1 {
                            gap += (new - old) / (count - 1) as f32;
                        }
                    }
                    if !gap.is_finite() {
                        return Err("SVG adjusted span cursor requires finite geometry".into());
                    }
                    if issued.is_some_and(|value: f32| value != gap) {
                        return Err("SVG reused adjusted spans with distinct fonts require complete instance cursor distribution".into());
                    }
                    if chunk.anchor() != usvg::TextAnchor::Start {
                        return Err("SVG independent adjusted span anchors require complete source anchor distribution".into());
                    }
                    issued = Some(gap);
                }
                if let Some(gap) = issued {
                    if gap != 0.0 {
                        let start = parent.range().start + 1;
                        let end = start
                            + source[start..]
                                .find(|character: char| {
                                    character.is_ascii_whitespace()
                                        || character == '>'
                                        || character == '/'
                                })
                                .unwrap();
                        let name = &source[start..end];
                        let qualified = name
                            .split_once(':')
                            .map_or("tspan".to_string(), |(prefix, _)| format!("{prefix}:tspan"));
                        let range = next.range();
                        let original = &source[range.clone()];
                        edits.push((
                            range,
                            format!("<{qualified} dx=\"{}\">{original}</{qualified}>", -gap),
                        ));
                        extra_nodes += 1;
                    }
                }
            }
            continue;
        }
        if node.tag_name().name() != "text" {
            continue;
        }
        let Some(instances) = roots.get(node.attribute("id").unwrap_or("")) else {
            continue;
        };
        let mut issued = None;
        for text in instances {
            if text.chunks().len() != 1 || text.chunks()[0].spans().len() != 1 {
                return Err(
                    "SVG ancestral textLength requires complete selected-span distribution".into(),
                );
            }
            let chunk = &text.chunks()[0];
            let span = &chunk.spans()[0];
            let target = percent
                .or(span.text_length())
                .ok_or("internal length plan lost its target")?;
            if span.length_adjust() != usvg::LengthAdjust::Spacing
                || chunk.text().chars().count() <= 1
                || chunk.anchor() == usvg::TextAnchor::Start
            {
                continue;
            }
            let shift = match chunk.anchor() {
                usvg::TextAnchor::Middle => target / 2.0,
                usvg::TextAnchor::End => target,
                usvg::TextAnchor::Start => 0.0,
            };
            let x = chunk.x().unwrap_or(0.0) - shift;
            if !x.is_finite() {
                return Err("SVG textLength anchor requires finite source positions".into());
            }
            if issued.is_some_and(|previous: f32| previous != x) {
                return Err("SVG reused textLength anchors with distinct instance contexts require complete instance source distribution".into());
            }
            issued = Some(x);
        }
        if let Some(x) = issued {
            attribute(node, "x", x.to_string(), source, &mut edits);
            attribute(node, "text-anchor", "start".into(), source, &mut edits);
        }
    }
    let source = apply(source, edits, ceiling)?;
    Ok(PreparedSource {
        source,
        work,
        extra_nodes,
    })
}
