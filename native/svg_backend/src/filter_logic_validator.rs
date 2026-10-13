//! Logical source ownership over the resolved static effect graph.
//! A generated flood/noise or constant remap cannot claim the original glyphs.
use std::collections::HashMap;
use usvg::filter::{ColorMatrixKind, CompositeOperator, Input, Kind, TransferFunction};

pub fn retains_source(group: &usvg::Group) -> bool {
    let mut source = true;
    for filter in group.filters() {
        let mut results = HashMap::<String, bool>::new();
        let mut output = source;
        for primitive in filter.primitives() {
            let kind = primitive.kind();
            let input = |input: &Input| match input {
                Input::SourceGraphic | Input::SourceAlpha => source,
                Input::Reference(name) => results.get(name).copied().unwrap_or(false),
            };
            output = match kind {
                Kind::Flood(_) | Kind::Turbulence(_) | Kind::Image(_) => false,
                Kind::Composite(fe) => match fe.operator() {
                    CompositeOperator::Arithmetic { k1, k2, k3, .. } => {
                        (input(fe.input1()) && (k1 != 0.0 || k2 != 0.0))
                            || (input(fe.input2()) && (k1 != 0.0 || k3 != 0.0))
                    }
                    _ => input(fe.input1()) || input(fe.input2()),
                },
                Kind::ColorMatrix(fe) => {
                    input(fe.input())
                        && match fe.kind() {
                            ColorMatrixKind::Matrix(values) => values
                                .iter()
                                .enumerate()
                                .any(|(i, value)| i % 5 != 4 && *value != 0.0),
                            _ => true,
                        }
                }
                Kind::ComponentTransfer(fe) => {
                    input(fe.input())
                        && [fe.func_r(), fe.func_g(), fe.func_b(), fe.func_a()]
                            .iter()
                            .any(|function| retains_component(function))
                }
                Kind::Blend(fe) => input(fe.input1()) || input(fe.input2()),
                Kind::ConvolveMatrix(fe) => input(fe.input()),
                Kind::DiffuseLighting(fe) => input(fe.input()),
                Kind::DisplacementMap(fe) => input(fe.input1()) || input(fe.input2()),
                Kind::DropShadow(fe) => input(fe.input()),
                Kind::GaussianBlur(fe) => input(fe.input()),
                Kind::Merge(fe) => fe.inputs().iter().any(input),
                Kind::Morphology(fe) => input(fe.input()),
                Kind::Offset(fe) => input(fe.input()),
                Kind::SpecularLighting(fe) => input(fe.input()),
                Kind::Tile(fe) => input(fe.input()),
            };
            results.insert(primitive.result().to_string(), output);
        }
        source = output;
    }
    source
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
