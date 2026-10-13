//! Logical source ownership over the resolved static effect graph.
//! Generated/constant output cannot claim original glyphs; independent image
//! inputs retain their own genuinely painted selected source groups.
use std::collections::{HashMap, HashSet};
use usvg::filter::{ColorMatrixKind, CompositeOperator, Input, Kind, TransferFunction};

pub struct SourcePlan<'a> {
    pub original: bool,
    pub images: Vec<(&'a usvg::Group, usvg::Transform)>,
}

struct Node<'a> {
    inputs: Vec<usize>,
    image: Option<(&'a usvg::Group, usvg::Transform)>,
}

pub fn source_plan(group: &usvg::Group) -> SourcePlan<'_> {
    // Node zero is the group's genuine initial SourceGraphic/SourceAlpha.
    let mut nodes = vec![Node {
        inputs: Vec::new(),
        image: None,
    }];
    let mut source = 0usize;
    for filter in group.filters() {
        let mut results = HashMap::<String, usize>::new();
        let mut output = source;
        for primitive in filter.primitives() {
            let input = |input: &Input| match input {
                Input::SourceGraphic | Input::SourceAlpha => source,
                Input::Reference(name) => {
                    *results.get(name).expect("validated earlier filter result")
                }
            };
            let mut node = Node {
                inputs: Vec::new(),
                image: None,
            };
            match primitive.kind() {
                Kind::Flood(_) | Kind::Turbulence(_) => {}
                Kind::Image(image) => {
                    let rect = primitive.rect();
                    node.image = Some((
                        image.root(),
                        usvg::Transform::from_translate(rect.x(), rect.y()),
                    ));
                }
                Kind::Composite(fe) => match fe.operator() {
                    CompositeOperator::Arithmetic { k1, k2, k3, .. } => {
                        if k1 != 0.0 || k2 != 0.0 {
                            node.inputs.push(input(fe.input1()));
                        }
                        if k1 != 0.0 || k3 != 0.0 {
                            node.inputs.push(input(fe.input2()));
                        }
                    }
                    _ => node.inputs.extend([input(fe.input1()), input(fe.input2())]),
                },
                Kind::ColorMatrix(fe) => {
                    let live = match fe.kind() {
                        ColorMatrixKind::Matrix(values) => values
                            .iter()
                            .enumerate()
                            .any(|(i, value)| i % 5 != 4 && *value != 0.0),
                        _ => true,
                    };
                    if live {
                        node.inputs.push(input(fe.input()));
                    }
                }
                Kind::ComponentTransfer(fe) => {
                    if [fe.func_r(), fe.func_g(), fe.func_b(), fe.func_a()]
                        .iter()
                        .any(|function| retains_component(function))
                    {
                        node.inputs.push(input(fe.input()));
                    }
                }
                Kind::Blend(fe) => node.inputs.extend([input(fe.input1()), input(fe.input2())]),
                Kind::ConvolveMatrix(fe) => node.inputs.push(input(fe.input())),
                Kind::DiffuseLighting(fe) => node.inputs.push(input(fe.input())),
                Kind::DisplacementMap(fe) => {
                    node.inputs.extend([input(fe.input1()), input(fe.input2())])
                }
                Kind::DropShadow(fe) => node.inputs.push(input(fe.input())),
                Kind::GaussianBlur(fe) => node.inputs.push(input(fe.input())),
                Kind::Merge(fe) => node.inputs.extend(fe.inputs().iter().map(input)),
                Kind::Morphology(fe) => node.inputs.push(input(fe.input())),
                Kind::Offset(fe) => node.inputs.push(input(fe.input())),
                Kind::SpecularLighting(fe) => node.inputs.push(input(fe.input())),
                Kind::Tile(fe) => node.inputs.push(input(fe.input())),
            }
            output = nodes.len();
            nodes.push(node);
            results.insert(primitive.result().to_string(), output);
        }
        source = output;
    }
    let mut plan = SourcePlan {
        original: false,
        images: Vec::new(),
    };
    let mut pending = vec![source];
    let mut visited = HashSet::new();
    while let Some(index) = pending.pop() {
        if !visited.insert(index) {
            continue;
        }
        if index == 0 {
            plan.original = true;
        }
        let node = &nodes[index];
        if let Some(image) = node.image {
            plan.images.push(image);
        }
        pending.extend(node.inputs.iter().rev());
    }
    plan
}

fn retains_component(function: &TransferFunction) -> bool {
    match function {
        TransferFunction::Identity => true,
        TransferFunction::Table(values) | TransferFunction::Discrete(values) => {
            values.is_empty() || values.windows(2).any(|pair| pair[0] != pair[1])
        }
        TransferFunction::Linear { slope, .. } => *slope != 0.0,
        TransferFunction::Gamma {
            amplitude,
            exponent,
            ..
        } => *amplitude != 0.0 && *exponent != 0.0,
    }
}
