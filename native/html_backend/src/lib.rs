//! Existing byte-only HTML utilities. No SVG parsing or rendering dependencies.
mod color_validator;
use rustler::{Binary, Encoder, Env, Term};

mod atoms {
    rustler::atoms! { ok, error, currentcolor }
}

// The caller validator owns UTF-8, returned boundary, and resource validation.
#[rustler::nif(schedule = "DirtyCpu")]
fn unicode_line_breaks(source: String) -> Vec<(usize, bool)> {
    unicode_linebreak::linebreaks(&source)
        .map(|(offset, opportunity)| {
            (
                offset,
                opportunity == unicode_linebreak::BreakOpportunity::Mandatory,
            )
        })
        .collect()
}

#[rustler::nif(schedule = "DirtyCpu")]
fn css_color_math_components(source: Binary<'_>) -> Vec<(usize, String)> {
    color_validator::color_math_components(source.as_slice())
}

#[rustler::nif(schedule = "DirtyCpu")]
fn parse_css_color<'a>(
    env: Env<'a>,
    source: Binary<'a>,
    initial_input: bool,
    math: Vec<(usize, u8, f64)>,
) -> Term<'a> {
    let color = match color_validator::parse_css_color(source.as_slice(), initial_input, &math) {
        Some(color_validator::ValidatedColor::CurrentColor) => {
            Some(atoms::currentcolor().encode(env))
        }
        Some(color_validator::ValidatedColor::Absolute(color)) => {
            color_validator::srgb_channels(color).map(|channels| channels.encode(env))
        }
        _ => None,
    };
    match color {
        Some(color) => (atoms::ok(), color).encode(env),
        None => (atoms::error(), "invalid CSS color").encode(env),
    }
}

rustler::init!(
    "Elixir.NativeElixirPdfUtilities.HtmlToPdf.NativeBackend.NativeBytes",
    [
        unicode_line_breaks,
        parse_css_color,
        css_color_math_components
    ]
);
