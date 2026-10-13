//! Byte-only backend. Caller policy and resource budgets live in Elixir validators.
mod effects_validator;
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
    let result = render_bytes(
        &source,
        width,
        height,
        fonts,
        resources,
        authored_hints,
        None,
    );
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
fn render_document<'a>(
    env: Env<'a>,
    source: String,
    width: u32,
    height: u32,
    fonts: Vec<Binary<'a>>,
    resources: HashMap<String, Binary<'a>>,
    matrix: (f64, f64, f64, f64, f64, f64),
) -> NifResult<Term<'a>> {
    // The document validator has already resolved and bounded this external
    // transform. The exact parsed source tree keeps its authored transforms.
    let transform = tiny_skia::Transform::from_row(
        matrix.0 as f32,
        matrix.1 as f32,
        matrix.2 as f32,
        matrix.3 as f32,
        matrix.4 as f32,
        matrix.5 as f32,
    );
    match render_bytes(
        &source,
        width,
        height,
        fonts,
        resources,
        false,
        Some(transform),
    ) {
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
    if !resources.is_empty() {
        return Err("SVG vector output requires image resource embedding support".to_string());
    }

    vector::convert(&shape_tree(source, fonts)?)
}

fn shape_tree(source: &str, fonts: Vec<Binary>) -> Result<usvg::Tree, String> {
    let xml = usvg::roxmltree::Document::parse_with_options(
        source,
        usvg::roxmltree::ParsingOptions {
            allow_dtd: false,
            ..Default::default()
        },
    )
    .map_err(|error| error.to_string())?;
    let (fontdb, resolver) = selected_fonts(fonts, true)?;
    let options = usvg::Options {
        resources_dir: None,
        fontdb: Arc::new(fontdb),
        font_resolver: resolver,
        image_href_resolver: usvg::ImageHrefResolver {
            resolve_data: Box::new(|_, _, _| None),
            resolve_string: Box::new(|_, _| None),
        },
        ..Default::default()
    };
    usvg::Tree::from_xmltree(&xml, &options).map_err(|error| error.to_string())
}

// This database is constructed exclusively from authorized binary sources.
// Inline aliases resolve to one exact selected face; there is no fallback loader.
fn selected_fonts(
    fonts: Vec<Binary>,
    aliases: bool,
) -> Result<(usvg::fontdb::Database, usvg::FontResolver<'static>), String> {
    let mut fontdb = usvg::fontdb::Database::new();
    let mut selected = HashMap::new();
    for (index, font) in fonts.into_iter().enumerate() {
        let source = usvg::fontdb::Source::Binary(Arc::new(font.as_slice().to_vec()));
        let ids = fontdb.load_font_source(source);
        if ids.len() != 1 {
            return Err(
                "selected SVG font bytes must import exactly one named static TrueType face".into(),
            );
        }
        selected.insert(format!("NativeSvgFont{index}"), ids[0]);
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
    let resolver = if aliases {
        usvg::FontResolver {
            select_font: Box::new(move |font, _| {
                font.families().iter().find_map(|family| match family {
                    svgtypes::FontFamily::Named(name) => selected.get(name).copied(),
                    _ => None,
                })
            }),
            select_fallback: Box::new(|_, _, _| None),
        }
    } else {
        usvg::FontResolver::default()
    };
    Ok((fontdb, resolver))
}

fn render_bytes(
    source: &str,
    width: u32,
    height: u32,
    fonts: Vec<Binary>,
    resources: HashMap<String, Binary>,
    authored_hints: bool,
    document_transform: Option<tiny_skia::Transform>,
) -> Result<Vec<u8>, String> {
    let xml = usvg::roxmltree::Document::parse_with_options(
        source,
        usvg::roxmltree::ParsingOptions {
            allow_dtd: false,
            ..Default::default()
        },
    )
    .map_err(|error| error.to_string())?;
    let (fontdb, resolver) = selected_fonts(fonts, authored_hints)?;
    let resources: HashMap<_, _> = resources
        .into_iter()
        .map(|(name, bytes)| (name, Arc::new(bytes.as_slice().to_vec())))
        .collect();
    let options = usvg::Options {
        resources_dir: None,
        fontdb: Arc::new(fontdb),
        // Pinned default selectors query only this byte-backed database. They do
        // not load files or system fonts. No dynamic loader callback is installed.
        font_resolver: resolver,
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
    let transform = document_transform.unwrap_or_else(|| {
        tiny_skia::Transform::from_scale(
            width as f32 / tree.size().width(),
            height as f32 / tree.size().height(),
        )
    });
    resvg::render(&tree, transform, &mut pixmap.as_mut());
    pixmap.encode_png().map_err(|error| error.to_string())
}

#[rustler::nif(schedule = "DirtyCpu")]
fn validate_shape_work<'a>(env: Env<'a>, source: String, ceilings: Term<'a>) -> Term<'a> {
    // Retain the old scalar request/result for already-loaded older bindings.
    // New validated callers issue both shared work ceilings in one request.
    if let Ok(ceiling) = ceilings.decode::<u64>() {
        return match shape_tree(&source, vec![])
            .and_then(|tree| vector_validator::work(tree.root(), ceiling))
        {
            Ok(work) => (atoms::ok(), work).encode(env),
            Err(message) => (atoms::error(), message).encode(env),
        };
    }
    let (stroke_ceiling, gradient_ceiling, fonts) =
        if let Ok((strokes, gradients, fonts)) = ceilings.decode::<(u64, u64, Vec<Binary>)>() {
            (strokes, gradients, fonts)
        } else if let Ok((strokes, gradients)) = ceilings.decode::<(u64, u64)>() {
            (strokes, gradients, vec![])
        } else {
            return (atoms::error(), "invalid internal SVG work ceilings").encode(env);
        };
    let result = shape_tree(&source, fonts).and_then(|tree| {
        vector_validator::text(tree.root(), tree.fontdb())?;
        let strokes = vector_validator::work(tree.root(), stroke_ceiling)?;
        let gradients = vector_validator::gradient_work(tree.root(), gradient_ceiling)?;
        Ok((strokes, gradients))
    });
    match result {
        Ok(work) => (atoms::ok(), work).encode(env),
        Err(message) => (atoms::error(), message).encode(env),
    }
}

#[rustler::nif(schedule = "DirtyCpu")]
fn validate_pattern_work<'a>(
    env: Env<'a>,
    source: String,
    width: u32,
    height: u32,
    ceiling: u64,
    fonts: Vec<Binary<'a>>,
) -> Term<'a> {
    let result = shape_tree(&source, fonts).and_then(|tree| {
        let scale = usvg::Transform::from_scale(
            width as f32 / tree.size().width(),
            height as f32 / tree.size().height(),
        );
        effects_validator::pattern_pixels(
            tree.root(),
            scale,
            ceiling,
            u64::from(width).saturating_mul(u64::from(height)),
        )
    });
    match result {
        Ok(pixels) => (atoms::ok(), pixels).encode(env),
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
    [
        render,
        render_document,
        vectorize,
        validate_path,
        validate_shape_work,
        validate_pattern_work
    ]
);
