//! Byte-only backend. Caller policy and resource budgets live in Elixir validators.
mod vector;
mod vector_validator;
use rustler::{Binary, Encoder, Env, NifResult, OwnedBinary, Term};
use std::{collections::HashMap, sync::Arc};

mod atoms {
    rustler::atoms! { ok, error }
}

#[rustler::nif(schedule = "DirtyCpu")]
fn render<'a>(
    env: Env<'a>,
    source: String,
    width: u32,
    height: u32,
    fonts: Vec<Binary<'a>>,
    resources: HashMap<String, Binary<'a>>,
    authored_hints: bool,
) -> NifResult<Term<'a>> {
    let result = render_bytes(&source, width, height, fonts, resources, authored_hints);
    match result {
        Ok(png) => {
            let mut binary = OwnedBinary::new(png.len()).ok_or(rustler::Error::BadArg)?;
            binary.as_mut_slice().copy_from_slice(&png);
            Ok((atoms::ok(), binary.release(env)).encode(env))
        }
        Err(message) => Ok((atoms::error(), message).encode(env)),
    }
}

#[rustler::nif(schedule = "DirtyCpu")]
fn vectorize<'a>(
    env: Env<'a>,
    source: String,
    fonts: Vec<Binary<'a>>,
    resources: HashMap<String, Binary<'a>>,
) -> NifResult<Term<'a>> {
    let result = vector_bytes(&source, fonts, resources);
    match result {
        Ok(pdf) => {
            let mut binary = OwnedBinary::new(pdf.len()).ok_or(rustler::Error::BadArg)?;
            binary.as_mut_slice().copy_from_slice(&pdf);
            Ok((atoms::ok(), binary.release(env)).encode(env))
        }
        Err(message) => Ok((atoms::error(), message).encode(env)),
    }
}

fn vector_bytes(
    source: &str,
    fonts: Vec<Binary>,
    resources: HashMap<String, Binary>,
) -> Result<Vec<u8>, String> {
    if !fonts.is_empty() || !resources.is_empty() {
        return Err(
            "SVG vector output requires selected font and resource embedding support".to_string(),
        );
    }

    vector::convert(&shape_tree(source)?)
}

fn shape_tree(source: &str) -> Result<usvg::Tree, String> {
    let xml = usvg::roxmltree::Document::parse_with_options(
        source,
        usvg::roxmltree::ParsingOptions {
            allow_dtd: false,
            ..Default::default()
        },
    )
    .map_err(|error| error.to_string())?;
    let options = usvg::Options {
        resources_dir: None,
        fontdb: Arc::new(usvg::fontdb::Database::new()),
        image_href_resolver: usvg::ImageHrefResolver {
            resolve_data: Box::new(|_, _, _| None),
            resolve_string: Box::new(|_, _| None),
        },
        ..Default::default()
    };
    usvg::Tree::from_xmltree(&xml, &options).map_err(|error| error.to_string())
}

fn render_bytes(
    source: &str,
    width: u32,
    height: u32,
    fonts: Vec<Binary>,
    resources: HashMap<String, Binary>,
    authored_hints: bool,
) -> Result<Vec<u8>, String> {
    let xml = usvg::roxmltree::Document::parse_with_options(
        source,
        usvg::roxmltree::ParsingOptions {
            allow_dtd: false,
            ..Default::default()
        },
    )
    .map_err(|error| error.to_string())?;
    let mut fontdb = usvg::fontdb::Database::new();
    for font in fonts {
        let source = usvg::fontdb::Source::Binary(Arc::new(font.as_slice().to_vec()));
        if fontdb.load_font_source(source).is_empty() {
            return Err(
                "selected SVG font bytes could not be imported by pinned fontdb 0.23.0; supply a named TrueType face supported by the SVG backend".to_string(),
            );
        }
    }
    let family = fontdb
        .faces()
        .next()
        .and_then(|face| face.families.first())
        .map(|(name, _)| name.clone());
    if let Some(family) = family {
        fontdb.set_serif_family(family.clone());
        fontdb.set_sans_serif_family(family.clone());
        fontdb.set_monospace_family(family.clone());
        fontdb.set_cursive_family(family.clone());
        fontdb.set_fantasy_family(family);
    }
    let resources: HashMap<_, _> = resources
        .into_iter()
        .map(|(name, bytes)| (name, Arc::new(bytes.as_slice().to_vec())))
        .collect();
    let options = usvg::Options {
        resources_dir: None,
        fontdb: Arc::new(fontdb),
        // Pinned default selectors query only this byte-backed database. They do
        // not load files or system fonts. No dynamic loader callback is installed.
        font_resolver: usvg::FontResolver::default(),
        // Image data URLs are also disabled; only explicitly supplied bytes resolve.
        image_href_resolver: usvg::ImageHrefResolver {
            resolve_data: Box::new(|_, _, _| None),
            resolve_string: Box::new(move |name, _| {
                resources
                    .get(name)
                    .map(|bytes| usvg::ImageKind::PNG(bytes.clone()))
            }),
        },
        // Preserve the restricted asset converter's default hints in this contract batch.
        shape_rendering: if authored_hints {
            usvg::ShapeRendering::default()
        } else {
            usvg::ShapeRendering::OptimizeSpeed
        },
        text_rendering: if authored_hints {
            usvg::TextRendering::default()
        } else {
            usvg::TextRendering::OptimizeSpeed
        },
        image_rendering: if authored_hints {
            usvg::ImageRendering::default()
        } else {
            usvg::ImageRendering::OptimizeSpeed
        },
        ..Default::default()
    };
    let tree = usvg::Tree::from_xmltree(&xml, &options).map_err(|error| error.to_string())?;
    let mut pixmap = tiny_skia::Pixmap::new(width, height)
        .ok_or_else(|| "SVG pixel allocation failed".to_string())?;
    let transform = tiny_skia::Transform::from_scale(
        width as f32 / tree.size().width(),
        height as f32 / tree.size().height(),
    );
    resvg::render(&tree, transform, &mut pixmap.as_mut());
    pixmap.encode_png().map_err(|error| error.to_string())
}

#[rustler::nif(schedule = "DirtyCpu")]
fn validate_shape_work<'a>(env: Env<'a>, source: String, ceilings: Term<'a>) -> Term<'a> {
    // Retain the old scalar request/result for already-loaded older bindings.
    // New validated callers issue both shared work ceilings in one request.
    if let Ok(ceiling) = ceilings.decode::<u64>() {
        return match shape_tree(&source)
            .and_then(|tree| vector_validator::work(tree.root(), ceiling))
        {
            Ok(work) => (atoms::ok(), work).encode(env),
            Err(message) => (atoms::error(), message).encode(env),
        };
    }
    let ceilings = match ceilings.decode::<(u64, u64)>() {
        Ok(ceilings) => ceilings,
        Err(_) => return (atoms::error(), "invalid internal SVG work ceilings").encode(env),
    };
    let result = shape_tree(&source).and_then(|tree| {
        let strokes = vector_validator::work(tree.root(), ceilings.0)?;
        let gradients = vector_validator::gradient_work(tree.root(), ceilings.1)?;
        Ok((strokes, gradients))
    });
    match result {
        Ok(work) => (atoms::ok(), work).encode(env),
        Err(message) => (atoms::error(), message).encode(env),
    }
}

// Called only by the Elixir geometry validator. usvg otherwise drops malformed
// paths; validation must diagnose them before visible rendering can succeed.
#[rustler::nif(schedule = "DirtyCpu")]
fn validate_path<'a>(
    env: Env<'a>,
    source: String,
    max_segments: u64,
    max_coordinate: f64,
) -> Term<'a> {
    let mut count = 0u64;
    for segment in svgtypes::PathParser::from(source.as_str()) {
        match segment {
            Ok(segment) => {
                if count == 0 && !matches!(segment, svgtypes::PathSegment::MoveTo { .. }) {
                    return (atoms::error(), "SVG path must begin with moveto").encode(env);
                }
                let values = match segment {
                    svgtypes::PathSegment::MoveTo { x, y, .. }
                    | svgtypes::PathSegment::LineTo { x, y, .. }
                    | svgtypes::PathSegment::SmoothQuadratic { x, y, .. } => {
                        [x, y, 0.0, 0.0, 0.0, 0.0]
                    }
                    svgtypes::PathSegment::HorizontalLineTo { x, .. } => {
                        [x, 0.0, 0.0, 0.0, 0.0, 0.0]
                    }
                    svgtypes::PathSegment::VerticalLineTo { y, .. } => [y, 0.0, 0.0, 0.0, 0.0, 0.0],
                    svgtypes::PathSegment::CurveTo {
                        x1,
                        y1,
                        x2,
                        y2,
                        x,
                        y,
                        ..
                    } => [x1, y1, x2, y2, x, y],
                    svgtypes::PathSegment::SmoothCurveTo { x2, y2, x, y, .. } => {
                        [x2, y2, x, y, 0.0, 0.0]
                    }
                    svgtypes::PathSegment::Quadratic { x1, y1, x, y, .. } => {
                        [x1, y1, x, y, 0.0, 0.0]
                    }
                    svgtypes::PathSegment::EllipticalArc {
                        rx,
                        ry,
                        x_axis_rotation,
                        x,
                        y,
                        ..
                    } => [rx, ry, x_axis_rotation, x, y, 0.0],
                    svgtypes::PathSegment::ClosePath { .. } => [0.0; 6],
                };
                if values
                    .iter()
                    .any(|value| !value.is_finite() || value.abs() > max_coordinate)
                {
                    return (
                        atoms::error(),
                        "SVG path coordinates exceed the configured numeric range",
                    )
                        .encode(env);
                }
                count += 1;
                if count > max_segments {
                    return (atoms::error(), "max_svg_path_segments").encode(env);
                }
            }
            Err(error) => return (atoms::error(), error.to_string()).encode(env),
        }
    }
    (atoms::ok(), count).encode(env)
}

rustler::init!(
    "Elixir.NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes",
    [render, vectorize, validate_path, validate_shape_work]
);
