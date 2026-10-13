//! Preparation for strictly internal, own-writer HTML effect input PDFs.
//! The Elixir slot/source validator owns provenance and caller authorization.
use hayro::hayro_interpret::font::{Glyph, GlyphRun};
use hayro::hayro_interpret::pattern::Pattern;
use hayro::hayro_interpret::{
    BlendMode, ClipPath, Device, DrawMode, DrawProps, Image, ImageDrawProps, Paint, SoftMask,
};
use hayro::kurbo::{Affine, BezPath, PathEl, Rect, Shape};

pub type GlyphPlan = (
    String,
    u32,
    (f64, f64, f64, f64, f64, f64),
    (f64, f64, f64, f64),
    bool,
);

pub struct Work {
    pub pixels: u64,
    pub work: u64,
    pub glyphs: u64,
    pub selected: Vec<GlyphPlan>,
    canvas: u64,
    viewport: Rect,
    ceilings: (u64, u64),
    clips: Vec<Rect>,
    opacity: Vec<bool>,
    pub failure: Option<String>,
}

impl Work {
    pub fn new(
        width: u32,
        height: u32,
        bytes: usize,
        source_pixels: u64,
        ceilings: (u64, u64),
    ) -> Self {
        let canvas = u64::from(width).saturating_mul(u64::from(height));
        let mut plan = Self {
            pixels: canvas
                .saturating_mul(16)
                .saturating_add(source_pixels.saturating_mul(12)),
            work: bytes as u64,
            glyphs: 0,
            selected: Vec::new(),
            canvas,
            viewport: Rect::new(0.0, 0.0, width as f64, height as f64),
            ceilings,
            clips: Vec::new(),
            opacity: Vec::new(),
            failure: None,
        };
        // vello_cpu's physical canvas dimensions use u16. This is a pinned
        // backend representation bound, independent of tunable caller budgets.
        if width == 0 || height == 0 || width > u16::MAX as u32 || height > u16::MAX as u32 {
            plan.failure = Some(
                "internal HTML surface exceeds the pinned native canvas representation".into(),
            );
        }
        plan.check();
        plan
    }
    fn check(&mut self) {
        if self.failure.is_none() {
            if self.pixels > self.ceilings.0 {
                self.failure = Some("max_aggregate_svg_surface_pixels".into());
            } else if self.work > self.ceilings.1 {
                self.failure = Some("max_svg_filter_work".into());
            }
        }
    }
    fn layer(&mut self) {
        self.pixels = self.pixels.saturating_add(self.canvas.saturating_mul(8));
        self.work = self.work.saturating_add(self.canvas.saturating_mul(64));
        self.check();
    }
    fn path(&mut self, path: &BezPath, transform: Affine) {
        if self.failure.is_some() {
            return;
        }
        let mut previous = hayro::kurbo::Point::ZERO;
        let mut first = previous;
        let mut perimeter = 0.0f64;
        for element in path.elements() {
            let points = match *element {
                PathEl::MoveTo(point) => {
                    previous = transform * point;
                    first = previous;
                    vec![]
                }
                PathEl::LineTo(point) => vec![point],
                PathEl::QuadTo(a, b) => vec![a, b],
                PathEl::CurveTo(a, b, c) => vec![a, b, c],
                PathEl::ClosePath => {
                    perimeter += previous.distance(first);
                    previous = first;
                    vec![]
                }
            };
            for point in points {
                let point = transform * point;
                perimeter += previous.distance(point);
                previous = point;
            }
        }
        if !perimeter.is_finite() {
            self.failure = Some("internal HTML path has nonfinite transformed geometry".into());
            return;
        }
        // The transformed control polygon bounds curve length and the number
        // of coverage intersections. Charge before native flattening/stroking.
        self.work = self
            .work
            .saturating_add((perimeter.ceil() as u64).saturating_mul(64));
        let bounds = (transform * path).bounding_box().intersect(self.viewport);
        let area = (bounds.width().max(0.0).ceil() as u64)
            .saturating_mul(bounds.height().max(0.0).ceil() as u64);
        self.work = self.work.saturating_add(area.saturating_mul(64));
        self.check();
    }
    fn paint<'a>(&mut self, props: &DrawProps<'a>, mode: &DrawMode) {
        if self.failure.is_some() {
            return;
        }
        if let Some(mask) = &props.soft_mask {
            self.layer();
            if self.failure.is_none() {
                mask.interpret(self);
            }
        }
        if let Paint::Pattern(pattern) = &props.paint {
            self.layer();
            if let Pattern::Tiling(tile) = &**pattern {
                let transformed =
                    (props.transform * tile.matrix * tile.bbox.to_path(0.1)).bounding_box();
                let pixels = (transformed.width().abs().ceil() as u64)
                    .saturating_mul(transformed.height().abs().ceil() as u64);
                self.pixels = self.pixels.saturating_add(pixels.saturating_mul(8));
                self.check();
                if self.failure.is_none() {
                    let _ = tile.interpret(
                        self,
                        props.transform * tile.matrix,
                        matches!(mode, DrawMode::Stroke(_)),
                    );
                }
            }
        }
    }
}

impl<'a> Device<'a> for Work {
    fn draw_path(&mut self, path: &BezPath, props: DrawProps<'a>, mode: &DrawMode) {
        self.path(path, props.transform);
        self.paint(&props, mode);
    }
    fn push_clip_path(&mut self, clip: &ClipPath) {
        self.layer();
        self.path(&clip.path, Affine::IDENTITY);
        let bounds = clip
            .path
            .bounding_box()
            .intersect(self.clips.last().copied().unwrap_or(self.viewport));
        self.clips.push(bounds);
    }
    fn push_transparency_group(&mut self, opacity: f32, mask: Option<SoftMask<'a>>, _: BlendMode) {
        self.opacity
            .push(opacity != 0.0 && self.opacity.last().copied().unwrap_or(true));
        self.layer();
        if let Some(mask) = mask {
            if self.failure.is_none() {
                mask.interpret(self);
            }
        }
    }
    fn draw_glyph_run(&mut self, run: &GlyphRun<'_, 'a>, props: DrawProps<'a>, mode: &DrawMode) {
        self.paint(&props, mode);
        for glyph in run.glyphs() {
            if self.failure.is_some() {
                break;
            }
            match &**glyph {
                Glyph::Outline(outline) if outline.font_data().is_some() => {
                    let path = outline.outline();
                    let transform = props.transform * glyph.transform();
                    let bounds = (transform * &path)
                        .bounding_box()
                        .intersect(self.clips.last().copied().unwrap_or(self.viewport));
                    let painted = !matches!(mode, DrawMode::Invisible)
                        && self.opacity.last().copied().unwrap_or(true)
                        && match &props.paint {
                            Paint::Color(color) => color.to_rgba().components()[3] != 0.0,
                            _ => true,
                        }
                        && bounds.width() > 0.0
                        && bounds.height() > 0.0;
                    if painted {
                        self.glyphs = self.glyphs.saturating_add(1);
                    }
                    self.work = self.work.saturating_add(256);
                    self.check();
                    if self.failure.is_none() {
                        let unicode = match outline.as_unicode() {
                            Some(hayro::hayro_interpret::hayro_cmap::BfString::Char(c)) => {
                                c.to_string()
                            }
                            Some(hayro::hayro_interpret::hayro_cmap::BfString::String(s)) => s,
                            None => {
                                self.failure = Some(
                                    "issued HTML glyph lost its original selected Unicode mapping"
                                        .into(),
                                );
                                break;
                            }
                        };
                        let coefficients = transform.as_coeffs();
                        let original = (transform * &path).bounding_box();
                        self.selected.push((
                            unicode,
                            outline.glyph_id().to_u32(),
                            (
                                coefficients[0],
                                coefficients[1],
                                coefficients[2],
                                coefficients[3],
                                coefficients[4],
                                coefficients[5],
                            ),
                            (original.x0, original.y0, original.x1, original.y1),
                            painted,
                        ));
                    }
                    self.path(&path, transform);
                }
                _ => {
                    self.failure = Some(
                        "internal HTML surface requires its exact embedded outline glyphs".into(),
                    );
                }
            }
        }
    }
    fn draw_image(&mut self, image: Image<'a, '_>, props: ImageDrawProps<'a>) {
        // All embedded image dimensions are reserved from the real writer PDF
        // dictionaries before interpretation or image decoding.
        let _ = image;
        self.layer();
        if let Some(mask) = props.soft_mask {
            if self.failure.is_none() {
                mask.interpret(self);
            }
        }
    }
    fn pop_clip(&mut self) {
        self.clips.pop();
    }
    fn pop_transparency_group(&mut self) {
        self.opacity.pop();
    }
}
