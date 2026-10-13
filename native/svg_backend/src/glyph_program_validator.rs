//! Glyph programs derived only from the original approved physical face/GID.
use usvg::tiny_skia_path::{Path, PathBuilder, Transform};

pub struct Program {
    pub path: Option<Path>,
    pub width: f32,
    pub bbox: [f32; 4],
    pub units: u16,
}

struct Outline {
    builder: PathBuilder,
    used: u64,
    ceiling: u64,
}
impl Outline {
    fn charge(&mut self) -> bool {
        self.used = self.used.saturating_add(1);
        self.used <= self.ceiling
    }
}
impl ttf_parser::OutlineBuilder for Outline {
    fn move_to(&mut self, x: f32, y: f32) {
        if self.charge() {
            self.builder.move_to(x, y);
        }
    }
    fn line_to(&mut self, x: f32, y: f32) {
        if self.charge() {
            self.builder.line_to(x, y);
        }
    }
    fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
        if self.charge() {
            self.builder.quad_to(x1, y1, x, y);
        }
    }
    fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
        if self.charge() {
            self.builder.cubic_to(x1, y1, x2, y2, x, y);
        }
    }
    fn close(&mut self) {
        if self.charge() {
            self.builder.close();
        }
    }
}

pub fn prepare(
    glyph: &usvg::layout::PositionedGlyph,
    stroke: Option<&usvg::Stroke>,
    database: &usvg::fontdb::Database,
) -> Result<Program, String> {
    prepare_bounded(glyph, stroke, database, u64::MAX)
}

pub fn prepare_bounded(
    glyph: &usvg::layout::PositionedGlyph,
    stroke: Option<&usvg::Stroke>,
    database: &usvg::fontdb::Database,
    ceiling: u64,
) -> Result<Program, String> {
    database
        .with_face_data(glyph.font, |data, index| {
            let face = ttf_parser::Face::parse(data, index)
                .map_err(|_| "approved glyph program lost its original face")?;
            let units = face.units_per_em() as f32;
            let width = face
                .glyph_hor_advance(glyph.id)
                .ok_or("approved glyph program lost its original advance")?
                as f32
                * 1000.0
                / units;
            let mut outline = Outline {
                builder: PathBuilder::new(),
                used: 0,
                ceiling,
            };
            face.outline_glyph(glyph.id, &mut outline);
            if outline.used > ceiling {
                return Err("max_svg_stroke_work".into());
            }
            let path = if let Some(path) = outline.builder.finish() {
                let transform =
                    Transform::from_scale(glyph.font_size() / units, -glyph.font_size() / units);
                let inverse = transform
                    .invert()
                    .ok_or("approved glyph program requires invertible original glyph geometry")?;
                let path = path
                    .transform(transform)
                    .ok_or("approved glyph program lost finite source geometry")?;
                // The same control-polygon/dash authority used for ordinary
                // shapes runs before allocating any expanded glyph dash path.
                crate::vector_validator::path_work(&path, stroke, ceiling)?;
                let resolution = units / glyph.font_size();
                let path = match stroke {
                    Some(stroke) => {
                        let style = stroke.to_tiny_skia();
                        let path = if let Some(dash) = style.dash.as_ref() {
                            path.dash(dash, resolution)
                                .ok_or("approved glyph program dash expansion failed")?
                        } else {
                            path
                        };
                        path.stroke(&style, resolution)
                            .ok_or("approved glyph program stroke expansion failed")?
                    }
                    None => path,
                };
                Some(
                    path.transform(
                        Transform::from_scale(1000.0 / units, 1000.0 / units).pre_concat(inverse),
                    )
                    .ok_or("approved glyph program lost normalized geometry")?,
                )
            } else {
                None
            };
            let bbox = path
                .as_ref()
                .and_then(|path| path.compute_tight_bounds())
                .map(|r| [r.left(), r.top(), r.right(), r.bottom()])
                .unwrap_or([0.0; 4]);
            Ok(Program {
                path,
                width,
                bbox,
                units: face.units_per_em(),
            })
        })
        .ok_or("approved glyph program requires the same byte-backed selected face")?
}
