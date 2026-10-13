//! Work validation before either raster or vector stroke expansion.
use usvg::tiny_skia_path::{PathSegment, Point};

pub fn work(group: &usvg::Group, ceiling: u64) -> Result<u64, String> {
    let mut total = match group.clip_path() {
        Some(clip) => work(clip.root(), ceiling)?,
        None => 0,
    };
    for node in group.children() {
        let amount = match node {
            usvg::Node::Group(group) => work(group, ceiling)?,
            usvg::Node::Path(path) => {
                let mut length = 0.0f64;
                let mut previous = Point::from_xy(0.0, 0.0);
                let mut start = previous;
                let mut segments = 0u64;
                for segment in path.data().segments() {
                    segments += 1;
                    let points = match segment {
                        PathSegment::MoveTo(p) => {
                            previous = p;
                            start = p;
                            vec![]
                        }
                        PathSegment::LineTo(p) => vec![p],
                        PathSegment::QuadTo(a, b) => vec![a, b],
                        PathSegment::CubicTo(a, b, c) => vec![a, b, c],
                        PathSegment::Close => vec![start],
                    };
                    // A Bezier's control polygon bounds its arc length.
                    for p in points {
                        length += ((p.x as f64 - previous.x as f64).powi(2)
                            + (p.y as f64 - previous.y as f64).powi(2))
                        .sqrt();
                        previous = p;
                    }
                }
                let repetitions = path
                    .stroke()
                    .and_then(|stroke| stroke.dasharray())
                    .map(|dash| {
                        let period: f64 = dash.iter().map(|n| *n as f64).sum();
                        ((length / period).ceil() + 1.0) * dash.len() as f64
                    })
                    .unwrap_or(1.0);
                let amount = segments as f64 * repetitions;
                if !amount.is_finite() || amount > ceiling as f64 {
                    return Err("max_svg_stroke_work".into());
                }
                amount.ceil() as u64
            }
            _ => 0,
        };
        total = total.saturating_add(amount);
        if total > ceiling {
            return Err("max_svg_stroke_work".into());
        }
    }
    Ok(total)
}

/// Counts functions needed for every prepared fill/stroke gradient occurrence.
pub fn gradient_work(group: &usvg::Group, ceiling: u64) -> Result<u64, String> {
    let mut total = 0u64;
    for node in group.children() {
        total = total.saturating_add(match node {
            usvg::Node::Group(group) => gradient_work(group, ceiling)?,
            usvg::Node::Path(path) => {
                let paints = path
                    .fill()
                    .map(|fill| fill.paint())
                    .into_iter()
                    .chain(path.stroke().map(|stroke| stroke.paint()));
                paints
                    .map(|paint| match paint {
                        usvg::Paint::LinearGradient(g) => g.stops().len() as u64,
                        usvg::Paint::RadialGradient(g) => g.stops().len() as u64,
                        _ => 0,
                    })
                    .sum()
            }
            _ => 0,
        });
        if total > ceiling {
            return Err("max_svg_gradient_work".into());
        }
    }
    Ok(total)
}
