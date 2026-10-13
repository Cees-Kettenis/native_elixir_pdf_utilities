//! Byte-only raster bridge for validator-issued native HTML painting PDFs.
//! No public PDF rendering/embedding API is introduced by this internal module.
use hayro::hayro_interpret::util::TransformExt;
use hayro::hayro_interpret::{interpret_page, Context, InterpreterCache, InterpreterSettings};
use hayro::kurbo::{Affine, Rect};
use hayro::{hayro_syntax::Pdf, RenderCache, RenderSettings};
use std::sync::{Arc, Mutex};

pub fn prepare(
    source: &[u8],
    width: u32,
    height: u32,
    ceilings: (u64, u64),
    source_pixels: u64,
) -> Result<(u64, u64, u64, Vec<crate::html_surface_validator::GlyphPlan>), String> {
    let mut work = crate::html_surface_validator::Work::new(
        width,
        height,
        source.len(),
        source_pixels,
        ceilings,
    );
    if let Some(failure) = work.failure {
        return Err(failure);
    }
    let (settings, failures) = settings();
    let pdf = Pdf::new(source.to_vec())
        .map_err(|error| format!("issued native HTML PDF cannot parse: {error:?}"))?;
    let page = pdf
        .pages()
        .first()
        .ok_or("issued native HTML PDF has no page")?;
    let (page_width, page_height) = page.render_dimensions();
    let transform = Affine::scale_non_uniform(
        width as f64 / page_width as f64,
        height as f64 / page_height as f64,
    ) * page.initial_transform(true).to_kurbo();
    let cache = InterpreterCache::new();
    let mut context = Context::new(
        transform,
        Rect::new(0.0, 0.0, width as f64, height as f64),
        &cache,
        pdf.xref(),
        settings,
    );
    interpret_page(page, &mut context, &mut work);
    if let Some(failure) = work.failure {
        return Err(failure);
    }
    check(&failures)?;
    Ok((work.pixels, work.work, work.glyphs, work.selected))
}

pub fn render(source: &[u8], width: u32, height: u32) -> Result<Vec<u8>, String> {
    let (settings, failures) = settings();
    let pdf = Pdf::new(source.to_vec())
        .map_err(|error| format!("issued native HTML PDF cannot parse: {error:?}"))?;
    let page = pdf
        .pages()
        .first()
        .ok_or("issued native HTML PDF has no page")?;
    let (page_width, page_height) = page.render_dimensions();
    let mut context = hayro::vello_cpu::RenderContext::new(width as u16, height as u16);
    let transform = Affine::scale_non_uniform(
        width as f64 / page_width as f64,
        height as f64 / page_height as f64,
    ) * page.initial_transform(true).to_kurbo();
    hayro::render_into(
        page,
        &RenderCache::new(),
        &settings,
        &RenderSettings::default(),
        &mut context,
        transform,
    );
    context.flush();
    let mut pixmap = hayro::vello_cpu::Pixmap::new(width as u16, height as u16);
    context.render_with(
        &mut pixmap,
        &mut hayro::vello_cpu::Resources::default(),
        hayro::vello_cpu::RasterizerSettings::default(),
    );
    check(&failures)?;
    pixmap
        .into_png()
        .map_err(|error| format!("issued native HTML PNG encoding failed: {error:?}"))
}

fn settings() -> (InterpreterSettings, Arc<Mutex<Vec<String>>>) {
    let failures = Arc::new(Mutex::new(Vec::new()));
    let font_failures = failures.clone();
    let cmap_failures = failures.clone();
    let warnings = failures.clone();
    (
        InterpreterSettings {
            font_resolver: Arc::new(move |_| {
                font_failures
                    .lock()
                    .unwrap()
                    .push("unissued fallback font requested".into());
                None
            }),
            cmap_resolver: Arc::new(move |_| {
                cmap_failures
                    .lock()
                    .unwrap()
                    .push("unissued external CMap requested".into());
                None
            }),
            warning_sink: Arc::new(move |warning| {
                warnings.lock().unwrap().push(format!("{warning:?}"))
            }),
            render_annotations: false,
        },
        failures,
    )
}
fn check(failures: &Arc<Mutex<Vec<String>>>) -> Result<(), String> {
    if let Some(failure) = failures.lock().unwrap().first() {
        Err(format!(
            "issued native HTML paint cannot be faithfully interpreted: {failure}"
        ))
    } else {
        Ok(())
    }
}
