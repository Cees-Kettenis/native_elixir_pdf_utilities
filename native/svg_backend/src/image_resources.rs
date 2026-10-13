//! Only opaque, prevalidated byte resources enter this resolver. No paths or data URLs.
use rustler::Binary;
use std::{collections::HashMap, sync::Arc};

pub fn resolver(resources: HashMap<String, Binary>) -> usvg::ImageHrefResolver<'static> {
    let bytes: HashMap<_, _> = resources
        .into_iter()
        .map(|(key, data)| (key, Arc::new(data.as_slice().to_vec())))
        .collect();
    usvg::ImageHrefResolver {
        resolve_data: Box::new(|_, _, _| None),
        resolve_string: Box::new(move |key, options| {
            let data = bytes.get(key)?;
            if data.starts_with(b"\x89PNG\r\n\x1a\n") {
                Some(usvg::ImageKind::PNG(data.clone()))
            } else if data.starts_with(&[255, 216]) {
                Some(usvg::ImageKind::JPEG(data.clone()))
            } else {
                let source = std::str::from_utf8(data).ok()?;
                let xml = usvg::roxmltree::Document::parse_with_options(
                    source,
                    usvg::roxmltree::ParsingOptions {
                        allow_dtd: false,
                        ..Default::default()
                    },
                )
                .ok()?;
                usvg::Tree::from_xmltree(&xml, options)
                    .ok()
                    .map(usvg::ImageKind::SVG)
            }
        }),
    }
}
