//! CSS Color4 grammar and conversion authority. The token parser validates the
//! authored color once; Color consumes its typed channels only. Parsing uses
//! borrowed bytes and bounded decoded identifiers, no assets or palette reads.

use color::{ColorSpace, ColorSpaceTag, DynamicColor, Flags};
use cssparser::{color::PredefinedColorSpace, ParseError, Parser, ParserInput, Token};
use cssparser_color::{AngleOrNumber, ColorParser, FromParsedColor, NumberOrPercentage};

pub enum ValidatedColor {
    CurrentColor,
    Absolute(DynamicColor),
}

impl ValidatedColor {
    fn absolute(cs: ColorSpaceTag, components: [Option<f32>; 4]) -> Self {
        Self::Absolute(DynamicColor {
            cs,
            flags: Flags::default(),
            components: components.map(|value| value.unwrap_or(0.0)),
        })
    }
}

impl FromParsedColor for ValidatedColor {
    fn from_current_color() -> Self {
        Self::CurrentColor
    }
    fn from_rgba(r: u8, g: u8, b: u8, a: f32) -> Self {
        Self::from_float_rgba(r as f32 / 255.0, g as f32 / 255.0, b as f32 / 255.0, a)
    }
    fn from_float_rgba(r: f32, g: f32, b: f32, a: f32) -> Self {
        Self::absolute(ColorSpaceTag::Srgb, [Some(r), Some(g), Some(b), Some(a)])
    }
    fn from_hsl(h: Option<f32>, s: Option<f32>, l: Option<f32>, a: Option<f32>) -> Self {
        let rgb = hsl_rgb(h.unwrap_or(0.0), s.unwrap_or(0.0), l.unwrap_or(0.0));
        Self::from_float_rgba(rgb[0], rgb[1], rgb[2], a.unwrap_or(0.0))
    }
    fn from_hwb(h: Option<f32>, w: Option<f32>, b: Option<f32>, a: Option<f32>) -> Self {
        // CSS Color4 section8.1 uses unit fractions. Preserve their parsed float
        // precision rather than multiply/divide by100 before8bit quantization.
        let white = w.unwrap_or(0.0);
        let black = b.unwrap_or(0.0);
        let rgb = if white + black >= 1.0 {
            [white / (white + black); 3]
        } else {
            let hue = hsl_rgb(h.unwrap_or(0.0), 1.0, 0.5);
            hue.map(|channel| white + channel * (1.0 - white - black))
        };
        Self::from_float_rgba(rgb[0], rgb[1], rgb[2], a.unwrap_or(0.0))
    }
    fn from_lab(l: Option<f32>, a: Option<f32>, b: Option<f32>, alpha: Option<f32>) -> Self {
        Self::absolute(ColorSpaceTag::Lab, [l, a, b, alpha])
    }
    fn from_lch(l: Option<f32>, c: Option<f32>, h: Option<f32>, a: Option<f32>) -> Self {
        Self::absolute(ColorSpaceTag::Lch, [l, c, h, a])
    }
    fn from_oklab(l: Option<f32>, a: Option<f32>, b: Option<f32>, alpha: Option<f32>) -> Self {
        Self::absolute(ColorSpaceTag::Oklab, [l, a, b, alpha])
    }
    fn from_oklch(l: Option<f32>, c: Option<f32>, h: Option<f32>, a: Option<f32>) -> Self {
        Self::absolute(ColorSpaceTag::Oklch, [l, c, h, a])
    }
    fn from_color_function(
        space: PredefinedColorSpace,
        r: Option<f32>,
        g: Option<f32>,
        b: Option<f32>,
        a: Option<f32>,
    ) -> Self {
        let space = match space {
            PredefinedColorSpace::Srgb => ColorSpaceTag::Srgb,
            PredefinedColorSpace::SrgbLinear => ColorSpaceTag::LinearSrgb,
            PredefinedColorSpace::DisplayP3 => ColorSpaceTag::DisplayP3,
            PredefinedColorSpace::DisplayP3Linear => {
                let encoded = color::Srgb::from_linear_srgb([
                    r.unwrap_or(0.0),
                    g.unwrap_or(0.0),
                    b.unwrap_or(0.0),
                ]);
                return Self::absolute(
                    ColorSpaceTag::DisplayP3,
                    [Some(encoded[0]), Some(encoded[1]), Some(encoded[2]), a],
                );
            }
            PredefinedColorSpace::A98Rgb => {
                // Chromium152 Color::GetSkColorSpaceParams uses SkNamedTransferFn
                // k2Dot2 for AdobeRGB. Map that transfer to Color's CSS rational
                // exponent before reusing its gamut/white-point conversion.
                let channels = [r, g, b]
                    .map(|value| value.map(|x| x.abs().powf(2.2 / (563.0 / 256.0)).copysign(x)));
                return Self::absolute(
                    ColorSpaceTag::A98Rgb,
                    [channels[0], channels[1], channels[2], a],
                );
            }
            PredefinedColorSpace::ProphotoRgb => ColorSpaceTag::ProphotoRgb,
            PredefinedColorSpace::Rec2020 => ColorSpaceTag::Rec2020,
            PredefinedColorSpace::XyzD50 => ColorSpaceTag::XyzD50,
            PredefinedColorSpace::XyzD65 => ColorSpaceTag::XyzD65,
        };
        Self::absolute(space, [r, g, b, a])
    }
}

// CSS Color4 section7.1, https://www.w3.org/TR/css-color-4/#hsl-to-rgb.
// Evaluate the specified divisions directly in f64. The general Color crate's
// f32 reciprocal approximation moves exact half-byte boundaries such as90deg.
fn hsl_rgb(hue: f32, saturation: f32, lightness: f32) -> [f32; 3] {
    let hue = if hue.is_finite() { hue as f64 } else { 0.0 };
    let saturation = saturation as f64;
    let lightness = lightness as f64;
    let amplitude = saturation * lightness.min(1.0 - lightness);
    [0.0, 8.0, 4.0].map(|n| {
        let k = (n + hue / 30.0).rem_euclid(12.0);
        (lightness - amplitude * (k - 3.0).min(9.0 - k).clamp(-1.0, 1.0)) as f32
    })
}

#[derive(PartialEq)]
enum Kind {
    Number,
    Percentage,
    Angle,
}
struct Value {
    number: f64,
    kind: Kind,
}
struct AbsoluteColorParser<'a> {
    math: &'a [(usize, u8, f64)],
}
impl AbsoluteColorParser<'_> {
    fn component<'i, 't>(&self, input: &mut Parser<'i, 't>) -> Result<Value, ParseError<'i, ()>> {
        match input.next()?.clone() {
            Token::Number { value, .. } => Ok(Value {
                number: value as f64,
                kind: Kind::Number,
            }),
            Token::Percentage { unit_value, .. } => Ok(Value {
                number: unit_value as f64,
                kind: Kind::Percentage,
            }),
            Token::Dimension { value, unit, .. } => {
                let scale = match unit.to_ascii_lowercase().as_str() {
                    "deg" => 1.0,
                    "grad" => 0.9,
                    "rad" => 180.0 / std::f64::consts::PI,
                    "turn" => 360.0,
                    _ => return Err(input.new_custom_error(())),
                };
                Ok(Value {
                    number: value as f64 * scale,
                    kind: Kind::Angle,
                })
            }
            Token::Function(_) => {
                let offset = input.position().byte_index();
                let (_, kind, number) = self
                    .math
                    .iter()
                    .find(|(position, _, _)| *position == offset)
                    .ok_or_else(|| input.new_custom_error(()))?;
                input.parse_nested_block(|nested| {
                    while nested.next().is_ok() {}
                    Ok(())
                })?;
                let kind = match kind {
                    0 => Kind::Number,
                    1 => Kind::Percentage,
                    2 => Kind::Angle,
                    _ => return Err(input.new_custom_error(())),
                };
                Ok(Value {
                    number: *number,
                    kind,
                })
            }
            _ => Err(input.new_custom_error(())),
        }
    }
}
impl<'i> ColorParser<'i> for AbsoluteColorParser<'_> {
    type Output = ValidatedColor;
    type Error = ();
    fn parse_percentage_or_number<'t>(
        &self,
        input: &mut Parser<'i, 't>,
    ) -> Result<f32, ParseError<'i, ()>> {
        let value = self.component(input)?;
        match value.kind {
            Kind::Number => Ok((value.number / 100.0) as f32),
            Kind::Percentage => Ok(value.number as f32),
            _ => Err(input.new_custom_error(())),
        }
    }
    fn parse_number_or_percentage<'t>(
        &self,
        input: &mut Parser<'i, 't>,
    ) -> Result<NumberOrPercentage, ParseError<'i, ()>> {
        let value = self.component(input)?;
        match value.kind {
            Kind::Number => Ok(NumberOrPercentage::Number {
                value: value.number as f32,
            }),
            Kind::Percentage => Ok(NumberOrPercentage::Percentage {
                unit_value: value.number as f32,
            }),
            _ => Err(input.new_custom_error(())),
        }
    }
    fn parse_number<'t>(&self, input: &mut Parser<'i, 't>) -> Result<f32, ParseError<'i, ()>> {
        let value = self.component(input)?;
        if value.kind == Kind::Number {
            Ok(value.number as f32)
        } else {
            Err(input.new_custom_error(()))
        }
    }
    fn parse_percentage<'t>(&self, input: &mut Parser<'i, 't>) -> Result<f32, ParseError<'i, ()>> {
        let value = self.component(input)?;
        if value.kind == Kind::Percentage {
            Ok(value.number as f32)
        } else {
            Err(input.new_custom_error(()))
        }
    }
    fn parse_angle_or_number<'t>(
        &self,
        input: &mut Parser<'i, 't>,
    ) -> Result<AngleOrNumber, ParseError<'i, ()>> {
        let value = self.component(input)?;
        match value.kind {
            Kind::Number => Ok(AngleOrNumber::Number {
                value: value.number as f32,
            }),
            Kind::Angle => Ok(AngleOrNumber::Angle {
                degrees: value.number as f32,
            }),
            _ => Err(input.new_custom_error(())),
        }
    }
}

// Borrowed CSS token boundaries identify actual component functions. Original
// slices go to the shared Elixir math authority; no synthetic color is reparsed.
pub fn color_math_components(source: &[u8]) -> Vec<(usize, String)> {
    let Ok(source) = std::str::from_utf8(source) else {
        return Vec::new();
    };
    let mut storage = ParserInput::new(source);
    let mut parser = Parser::new(&mut storage);
    let outer = match parser.next() {
        Ok(Token::Function(name)) => name.to_ascii_lowercase(),
        _ => return Vec::new(),
    };
    if ![
        "rgb", "rgba", "hsl", "hsla", "hwb", "lab", "lch", "oklab", "oklch", "color",
    ]
    .contains(&outer.as_str())
    {
        return Vec::new();
    }
    let mut components = Vec::new();
    let _: Result<(), ParseError<'_, ()>> = parser.parse_nested_block(|arguments| {
        loop {
            let start = arguments.position();
            match arguments.next() {
                Ok(Token::Function(_)) => {
                    let offset = arguments.position().byte_index();
                    arguments.parse_nested_block(|nested| {
                        while nested.next().is_ok() {}
                        Ok(())
                    })?;
                    components.push((offset, arguments.slice_from(start).to_owned()));
                }
                Ok(_) => {}
                Err(_) => break,
            }
        }
        Ok(())
    });
    components
}

pub fn parse_css_color(
    source: &[u8],
    initial_input: bool,
    math: &[(usize, u8, f64)],
) -> Option<ValidatedColor> {
    let source = std::str::from_utf8(source).ok()?;
    let mut input = ParserInput::new(source);
    let mut parser = Parser::new(&mut input);
    if initial_input {
        let state = parser.state();
        if let Ok(name) = parser.expect_ident_cloned() {
            if let Some(channels) = initial_light_system_color(&name) {
                parser.expect_exhausted().ok()?;
                return Some(ValidatedColor::from_rgba(
                    channels[0],
                    channels[1],
                    channels[2],
                    1.0,
                ));
            }
        }
        parser.reset(&state);
    }
    let authority = AbsoluteColorParser { math };
    let result: Result<ValidatedColor, ParseError<'_, ()>> =
        parser.parse_entirely(|parser| cssparser_color::parse_color_with(&authority, parser));
    result.ok()
}

pub fn srgb_channels(color: DynamicColor) -> Option<(f64, f64, f64, f64)> {
    let channels = color
        .to_alpha_color::<color::Srgb>()
        .components
        .map(|value| value.clamp(0.0, 1.0));
    if channels.iter().any(|value| !value.is_finite()) {
        return None;
    }
    Some((
        channels[0] as f64,
        channels[1] as f64,
        channels[2] as f64,
        channels[3] as f64,
    ))
}

// Pinned Chromium152 initial-input ParseSystemColor uses light fallback colors,
// no caller/platform color provider and no accent-color exposure. These differ
// from document CSS system colors and grant no palette or resource capability.
fn initial_light_system_color(name: &str) -> Option<[u8; 3]> {
    match name.to_ascii_lowercase().as_str() {
        "accentcolor" => Some([0, 0, 0]),
        "accentcolortext" => Some([0, 0, 0]),
        "activetext" => Some([255, 0, 0]),
        "buttonborder" => Some([118, 118, 118]),
        "buttonface" => Some([239, 239, 239]),
        "buttontext" => Some([0, 0, 0]),
        "canvas" => Some([255, 255, 255]),
        "canvastext" => Some([0, 0, 0]),
        "field" => Some([255, 255, 255]),
        "fieldtext" => Some([0, 0, 0]),
        "graytext" => Some([128, 128, 128]),
        "highlight" => Some([0, 65, 198]),
        "highlighttext" => Some([255, 255, 255]),
        "linktext" => Some([0, 0, 238]),
        "mark" => Some([255, 255, 0]),
        "marktext" => Some([0, 0, 0]),
        "selecteditem" => Some([25, 103, 210]),
        "selecteditemtext" => Some([255, 255, 255]),
        "visitedtext" => Some([85, 26, 139]),
        "activeborder" => Some([118, 118, 118]),
        "activecaption" => Some([255, 255, 255]),
        "appworkspace" => Some([255, 255, 255]),
        "background" => Some([255, 255, 255]),
        "buttonhighlight" => Some([239, 239, 239]),
        "buttonshadow" => Some([239, 239, 239]),
        "captiontext" => Some([0, 0, 0]),
        "inactiveborder" => Some([118, 118, 118]),
        "inactivecaption" => Some([255, 255, 255]),
        "inactivecaptiontext" => Some([128, 128, 128]),
        "infobackground" => Some([255, 255, 255]),
        "infotext" => Some([0, 0, 0]),
        "menu" => Some([255, 255, 255]),
        "menutext" => Some([0, 0, 0]),
        "scrollbar" => Some([255, 255, 255]),
        "threeddarkshadow" => Some([118, 118, 118]),
        "threedface" => Some([239, 239, 239]),
        "threedhighlight" => Some([118, 118, 118]),
        "threedlightshadow" => Some([118, 118, 118]),
        "threedshadow" => Some([118, 118, 118]),
        "window" => Some([255, 255, 255]),
        "windowframe" => Some([118, 118, 118]),
        "windowtext" => Some([0, 0, 0]),
        _ => None,
    }
}
