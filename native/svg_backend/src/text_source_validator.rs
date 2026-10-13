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
    checks: Vec<PlacementCheck>,
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
pub fn identify(source: &str, ceiling: usize, links: &[String]) -> Result<PreparedSource, String> {
    let xml = document(source)?;
    let ids: HashSet<_> = xml
        .descendants()
        .filter_map(|node| node.attribute("id"))
        .collect();
    let mut edits = vec![];
    let mut work = 0;
    let mut index = 0;
    let mut anchor = 0;
    for node in xml.descendants().filter(|node| {
        node.is_element()
            && matches!(
                node.tag_name().namespace(),
                None | Some("http://www.w3.org/2000/svg")
            )
    }) {
        work += 1;
        if node.tag_name().name() == "a"
            && (node.has_attribute("href")
                || node.has_attribute(("http://www.w3.org/1999/xlink", "href")))
        {
            let url = links
                .get(anchor)
                .ok_or("issued SVG link source count does not match original anchors")?;
            anchor += 1;
            if !node.has_attribute("id") {
                let id = loop {
                    index += 1;
                    let id = format!("NativeSvgAnchor{index}");
                    if !ids.contains(id.as_str()) {
                        break id;
                    }
                };
                edits.push((
                    insertion(node, source)..insertion(node, source),
                    format!(" id=\"{id}\""),
                ));
            }
            let escaped = url
                .replace('&', "&amp;")
                .replace('\"', "&quot;")
                .replace('\'', "&apos;")
                .replace('<', "&lt;");
            if let Some(attribute) = node
                .attributes()
                .find(|a| a.name() == "href" && a.namespace().is_none())
                .or_else(|| {
                    node.attributes().find(|a| {
                        a.name() == "href" && a.namespace() == Some("http://www.w3.org/1999/xlink")
                    })
                })
            {
                edits.push((attribute.range_value(), escaped));
            } else {
                edits.push((
                    insertion(node, source)..insertion(node, source),
                    format!(" href=\"{escaped}\""),
                ));
            }
        }
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
    if anchor != links.len() {
        return Err("issued SVG link source count does not match original anchors".into());
    }
    let source = apply(source, edits, ceiling)?;
    Ok(PreparedSource {
        source,
        work,
        extra_nodes: 0,
        checks: vec![],
    })
}

// Composite length roots need the selected natural glyph geometry before any
// descendant or ancestor distribution. The temporary tree uses the same fonts,
// namespaces, source text, original positions and viewport as the final source.
fn composite_length_root(root: Node<'_, '_>) -> bool {
    root.descendants()
        .filter(|node| node.is_element() && node.has_attribute("textLength"))
        .any(|node| {
            node.children()
                .any(|child| child.is_element() && matches!(child.tag_name().name(), "tspan" | "a"))
                || (node.tag_name().name() == "tspan" && node.parent_element() != Some(root))
                || (node.tag_name().name() == "tspan"
                    && node
                        .next_siblings()
                        .skip(1)
                        .find(|next| next.is_text() || next.is_element())
                        .is_some_and(|next| next.is_element()))
        })
}

pub fn measurement(
    source: &str,
    ceiling: usize,
    work_ceiling: u64,
) -> Result<Option<PreparedSource>, String> {
    if !source.contains("textLength") {
        return Ok(None);
    }
    let xml = document(source)?;
    let mut edits = vec![];
    let mut work = 0;
    for root in xml
        .descendants()
        .filter(|node| node.is_element() && node.tag_name().name() == "text")
    {
        if !composite_length_root(root) {
            continue;
        }
        charge_length_work(&mut work, work_ceiling)?;
        for node in root.descendants().filter(|node| node.is_element()) {
            charge_length_work(&mut work, work_ceiling)?;
            if let Some(length) = node.attributes().find(|attribute| {
                attribute.namespace().is_none() && attribute.name() == "textLength"
            }) {
                edits.push((length.range(), String::new()));
            }
        }
    }
    if edits.is_empty() {
        return Ok(None);
    }
    Ok(Some(PreparedSource {
        source: apply(source, edits, ceiling)?,
        work,
        extra_nodes: 0,
        checks: vec![],
    }))
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

pub fn prepare(
    source: &str,
    tree: &usvg::Tree,
    ceiling: usize,
    work_ceiling: u64,
) -> Result<PreparedSource, String> {
    let xml = document(source)?;
    let mut roots = HashMap::new();
    texts(tree.root(), &mut roots);
    let mut edits = vec![];
    let mut work = 0;
    let mut extra_nodes = 0;
    let mut checks = vec![];
    let mut composite_roots = HashSet::new();
    for root in xml.descendants().filter(|node| {
        node.is_element() && node.tag_name().name() == "text" && composite_length_root(*node)
    }) {
        composite_roots.insert(root.id());
        if let Some(instances) = roots.get(root.attribute("id").unwrap_or("")) {
            let mut issued: Option<String> = None;
            let mut selected_edits = vec![];
            for (instance, text) in instances.iter().enumerate() {
                let mut candidate_edits = vec![];
                let (candidate, mut check) = composite_spacing(
                    root,
                    text,
                    tree,
                    source,
                    &mut candidate_edits,
                    &mut work,
                    work_ceiling,
                )?;
                if issued
                    .as_ref()
                    .is_some_and(|previous| previous != &candidate)
                {
                    return Err("SVG composite reused lengths require actual independent instance distribution".into());
                }
                if issued.is_none() {
                    selected_edits = candidate_edits;
                    issued = Some(candidate);
                }
                check.instance = instance;
                checks.push(check);
            }
            edits.extend(selected_edits);
        }
    }
    for node in xml.descendants().filter(|node| {
        node.is_element()
            && matches!(
                node.tag_name().namespace(),
                None | Some("http://www.w3.org/2000/svg")
            )
            && node.has_attribute("textLength")
    }) {
        if node
            .ancestors()
            .any(|parent| composite_roots.contains(&parent.id()))
        {
            continue;
        }
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
        checks,
    })
}

#[derive(Clone)]
struct PlacementCheck {
    root: String,
    instance: usize,
    starts: Vec<f32>,
    ends: Vec<f32>,
    logical: String,
    members: Vec<(usvg::fontdb::ID, u16, usize)>,
}

#[derive(Clone)]
struct PositionedCluster {
    start: usize,
    end: usize,
    x: f32,
    end_x: f32,
    shift: f32,
}

fn positioned_clusters(
    text: &usvg::Text,
    database: &usvg::fontdb::Database,
) -> Result<Vec<PositionedCluster>, String> {
    let mut result: Vec<PositionedCluster> = Vec::new();
    let mut offset = 0;
    for glyph in text
        .layouted()
        .iter()
        .flat_map(|span| &span.positioned_glyphs)
    {
        let x = glyph.outline_transform().tx;
        let end_x = crate::vector_validator::glyph_end(glyph, database)?;
        if glyph.text.is_empty() {
            if let Some(previous) = result.last_mut() {
                previous.end_x = previous.end_x.max(end_x);
            }
        } else {
            let count = glyph.text.chars().count();
            result.push(PositionedCluster {
                start: offset,
                end: offset + count,
                x,
                end_x,
                shift: 0.0,
            });
            offset += count;
        }
    }
    Ok(result)
}

// A text leaf may lose boundary whitespace under SVG's default collapsing.
// Bind its actual selected source span without replacing any original bytes.
fn collapsed_leaf(value: &str) -> String {
    let mut out = String::new();
    let mut space = false;
    for ch in value.chars() {
        if matches!(ch, ' ' | '\t' | '\r' | '\n') {
            if !space {
                out.push(' ');
            }
            space = true;
        } else {
            out.push(ch);
            space = false;
        }
    }
    out
}

fn composite_spacing(
    root: Node<'_, '_>,
    text: &usvg::Text,
    tree: &usvg::Tree,
    source: &str,
    edits: &mut Vec<(Range<usize>, String)>,
    work: &mut u64,
    work_ceiling: u64,
) -> Result<(String, PlacementCheck), String> {
    if text.chunks().len() != 1
        || !matches!(text.chunks()[0].text_flow(), usvg::TextFlow::Linear)
        || text.rotate().iter().any(|angle| *angle != 0.0)
    {
        return Err("SVG composite textLength requires complete rotated/path or absolute chunk distribution".into());
    }
    let chunk = &text.chunks()[0];
    let logical = chunk.text().to_string();
    let source_spans: Vec<_> = chunk
        .spans()
        .iter()
        .map(|span| {
            chunk
                .text()
                .get(span.start()..span.end())
                .ok_or("SVG composite length lost an original selected source range")
        })
        .collect::<Result<_, _>>()?;
    if source_spans.len() != text.layouted().len() {
        return Err("SVG composite length requires complete selected source-span mapping".into());
    }
    let leaves: Vec<_> = root
        .descendants()
        .filter(|node| {
            node.is_text()
                && node
                    .ancestors()
                    .take_while(|ancestor| *ancestor != root)
                    .all(|ancestor| {
                        !ancestor.is_element()
                            || matches!(ancestor.tag_name().name(), "tspan" | "a" | "textPath")
                    })
        })
        .collect();
    let mut ranges = vec![];
    let mut span = 0;
    let mut offset = 0;
    for leaf in leaves {
        charge_length_work(work, work_ceiling)?;
        let normalized = collapsed_leaf(leaf.text().unwrap_or(""));
        let Some(selected) = source_spans.get(span) else {
            if normalized.trim_matches(' ').is_empty() {
                continue;
            }
            return Err("SVG composite length lost its actual source leaf".into());
        };
        if ![
            normalized.as_str(),
            normalized.trim_start_matches(' '),
            normalized.trim_end_matches(' '),
            normalized.trim_matches(' '),
        ]
        .contains(selected)
        {
            if normalized.trim_matches(' ').is_empty() {
                continue;
            }
            return Err(
                "SVG composite textLength source whitespace does not match selected glyph text"
                    .into(),
            );
        }
        let end = offset + selected.chars().count();
        ranges.push((leaf, offset..end));
        offset = end;
        span += 1;
    }
    if span != source_spans.len() || offset != logical.chars().count() {
        return Err("SVG composite length requires complete original Unicode leaf mapping".into());
    }
    let mut glyphs = positioned_clusters(text, tree.fontdb())?;
    if text
        .layouted()
        .iter()
        .flat_map(|span| &span.positioned_glyphs)
        .map(|glyph| glyph.text.as_str())
        .collect::<String>()
        != logical
    {
        return Err("SVG composite length selected glyphs do not preserve original Unicode".into());
    }
    let mut targets: Vec<_> = root
        .descendants()
        .filter(|node| node.is_element() && node.has_attribute("textLength"))
        .collect();
    targets.sort_by_key(|node| std::cmp::Reverse(node.ancestors().count()));
    let mut resolved: Vec<(Node<'_, '_>, Range<usize>)> = vec![];
    for node in targets {
        charge_length_work(work, work_ceiling)?;
        if node.attribute("lengthAdjust").unwrap_or("spacing") != "spacing" {
            return Err(
                "SVG composite glyph scaling requires complete nested selected-glyph distribution"
                    .into(),
            );
        }
        let leaf_ranges: Vec<_> = ranges
            .iter()
            .filter(|(leaf, _)| leaf.ancestors().any(|parent| parent == node))
            .map(|(_, range)| range)
            .collect();
        let (Some(first), Some(last)) = (leaf_ranges.first(), leaf_ranges.last()) else {
            continue;
        };
        let range = first.start..last.end;
        let indexes: Vec<_> = glyphs
            .iter()
            .enumerate()
            .filter(|(_, glyph)| glyph.start >= range.start && glyph.end <= range.end)
            .map(|(index, _)| index)
            .collect();
        if indexes.is_empty() {
            continue;
        }
        let children: Vec<_> = resolved
            .iter()
            .filter(|(descendant, _)| {
                descendant.ancestors().skip(1).any(|parent| parent == node)
                    && !resolved.iter().any(|(other, _)| {
                        other != descendant
                            && descendant
                                .ancestors()
                                .skip(1)
                                .any(|parent| parent == *other)
                            && other.ancestors().skip(1).any(|parent| parent == node)
                    })
            })
            .map(|(_, range)| range)
            .collect();
        let step_count = indexes
            .iter()
            .filter(|index| {
                !children.iter().any(|child| {
                    glyphs[**index].start > child.start && glyphs[**index].start < child.end
                })
            })
            .count();
        let length = svgtypes::Length::from_str(node.attribute("textLength").unwrap())
            .map_err(|_| "internal validated composite length lost its value")?;
        let target = match length.unit {
            svgtypes::LengthUnit::None | svgtypes::LengthUnit::Px => length.number,
            svgtypes::LengthUnit::Percent => viewport_width(node, tree)? * length.number / 100.0,
            _ => {
                return Err(
                    "SVG composite textLength requires its actual validated host unit context"
                        .into(),
                )
            }
        } as f32;
        let first_index = indexes[0];
        let last_index = *indexes.last().unwrap();
        let current = glyphs[last_index].end_x + glyphs[last_index].shift
            - glyphs[first_index].x
            - glyphs[first_index].shift;
        if !target.is_finite() || !current.is_finite() {
            return Err("SVG composite textLength requires finite selected glyph geometry".into());
        }
        if step_count > 1 {
            let adjustment = (target - current) / (step_count - 1) as f32;
            let mut shift = 0.0;
            for index in indexes {
                charge_length_work(work, work_ceiling)?;
                glyphs[index].shift += shift;
                if !children.iter().any(|child| {
                    glyphs[index].start > child.start && glyphs[index].start < child.end
                }) {
                    shift += adjustment;
                }
            }
            // The selected character cursor includes the adjustment on the
            // last typographic character. Parent adjustment subsequently uses
            // the actual resulting positions, including following source text.
            for glyph in glyphs.iter_mut().skip(last_index + 1) {
                charge_length_work(work, work_ceiling)?;
                glyph.shift += shift;
            }
        }
        resolved.push((node, range));
    }
    let original_width = glyphs.last().map_or(0.0, |glyph| glyph.end_x)
        - glyphs.first().map_or(0.0, |glyph| glyph.x);
    let final_width = glyphs.last().map_or(0.0, |glyph| glyph.end_x + glyph.shift)
        - glyphs.first().map_or(0.0, |glyph| glyph.x + glyph.shift);
    let anchor_shift = match chunk.anchor() {
        usvg::TextAnchor::Start => 0.0,
        usvg::TextAnchor::Middle => -(final_width - original_width) / 2.0,
        usvg::TextAnchor::End => -(final_width - original_width),
    };
    for glyph in &mut glyphs {
        glyph.shift += anchor_shift;
    }
    let mut dx = text.dx().to_vec();
    if dx.len() != offset {
        return Err("SVG composite length lost its actual character offsets".into());
    }
    let mut previous = 0.0;
    for glyph in &glyphs {
        dx[glyph.start] += glyph.shift - previous;
        previous = glyph.shift;
    }
    let issued = dx
        .iter()
        .map(|value| value.to_string())
        .collect::<Vec<_>>()
        .join(" ");
    for node in root.descendants().filter(|node| node.is_element()) {
        for attribute in node.attributes().filter(|attribute| {
            attribute.namespace().is_none()
                && (attribute.name() == "textLength" || (node != root && attribute.name() == "dx"))
        }) {
            edits.push((attribute.range(), String::new()));
        }
    }
    attribute(root, "dx", issued.clone(), source, edits);
    let check = PlacementCheck {
        instance: 0,
        root: root.attribute("id").unwrap_or("").to_string(),
        starts: glyphs.iter().map(|glyph| glyph.x + glyph.shift).collect(),
        ends: glyphs
            .iter()
            .map(|glyph| glyph.end_x + glyph.shift)
            .collect(),
        logical,
        members: text
            .layouted()
            .iter()
            .flat_map(|span| &span.positioned_glyphs)
            .map(|glyph| (glyph.font, glyph.id.0, glyph.text.len()))
            .collect(),
    };
    Ok((issued, check))
}

// Verify the compiler's actual selected-glyph placement after final parsing.
// This is internal geometry evidence, never an original-source or permission receipt.
pub fn verify(prepared: &PreparedSource, tree: &usvg::Tree) -> Result<(), String> {
    if prepared.checks.is_empty() {
        return Ok(());
    }
    let mut roots = HashMap::new();
    texts(tree.root(), &mut roots);
    for check in &prepared.checks {
        let text = roots
            .get(check.root.as_str())
            .and_then(|instances| instances.get(check.instance))
            .ok_or("SVG prepared length root lost its actual final instance")?;
        let logical: String = text.chunks().iter().map(|chunk| chunk.text()).collect();
        let glyphs = positioned_clusters(text, tree.fontdb())?;
        let members: Vec<_> = text
            .layouted()
            .iter()
            .flat_map(|span| &span.positioned_glyphs)
            .map(|glyph| (glyph.font, glyph.id.0, glyph.text.len()))
            .collect();
        if members != check.members {
            return Err("SVG prepared length changed actual selected font, glyph or source cluster identity".into());
        }
        if logical != check.logical || glyphs.len() != check.starts.len() {
            return Err(
                "SVG prepared composite length lost original selected Unicode clusters".into(),
            );
        }
        for ((glyph, start), end) in glyphs.iter().zip(&check.starts).zip(&check.ends) {
            let tolerance = f32::EPSILON * start.abs().max(end.abs()).max(1.0) * 32.0;
            if (glyph.x - start).abs() > tolerance || (glyph.end_x - end).abs() > tolerance {
                return Err("SVG composite textLength final selected glyph geometry differs from its actual placement plan".into());
            }
        }
    }
    Ok(())
}

fn charge_length_work(work: &mut u64, ceiling: u64) -> Result<(), String> {
    *work = work.checked_add(1).ok_or("max_svg_expansion_work")?;
    if *work > ceiling {
        return Err("max_svg_expansion_work".into());
    }
    Ok(())
}

pub fn retained_source_bytes(prepared: &PreparedSource) -> u64 {
    prepared
        .checks
        .iter()
        .map(|check| (check.logical.len() + check.root.len()) as u64)
        .sum()
}
