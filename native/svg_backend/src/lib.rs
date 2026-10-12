//! Byte-only backend. Caller policy and resource budgets live in Elixir validators.
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
) -> NifResult<Term<'a>> {
    let result = render_bytes(&source, width, height, fonts, resources);
    match result {
        Ok(png) => {
            let mut binary = OwnedBinary::new(png.len()).ok_or(rustler::Error::BadArg)?;
            binary.as_mut_slice().copy_from_slice(&png);
            Ok((atoms::ok(), binary.release(env)).encode(env))
        }
        Err(message) => Ok((atoms::error(), message).encode(env)),
    }
}

fn render_bytes(
    source: &str,
    width: u32,
    height: u32,
    fonts: Vec<Binary>,
    resources: HashMap<String, Binary>,
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
        shape_rendering: usvg::ShapeRendering::OptimizeSpeed,
        text_rendering: usvg::TextRendering::OptimizeSpeed,
        image_rendering: usvg::ImageRendering::OptimizeSpeed,
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

rustler::init!(
    "Elixir.NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes",
    [render]
);
