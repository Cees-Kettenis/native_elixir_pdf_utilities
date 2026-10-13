//! Disposable byte-only HTML form-value backend. No files, URLs, scripts,
//! resources or caller authorization are evaluated by the parsers.
use regress::{budget, Flags, Regex};
use std::alloc::{GlobalAlloc, Layout, System};
use std::io::{Read, Write};

struct BoundedAllocator;
unsafe impl GlobalAlloc for BoundedAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        budget::allocation(layout.size());
        System.alloc(layout)
    }
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        budget::allocation(layout.size());
        System.alloc_zeroed(layout)
    }
    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, size: usize) -> *mut u8 {
        budget::allocation(size);
        System.realloc(ptr, layout, size)
    }
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        System.dealloc(ptr, layout)
    }
}
#[global_allocator]
static ALLOCATOR: BoundedAllocator = BoundedAllocator;

fn number(bytes: &[u8], offset: &mut usize) -> Option<usize> {
    let end = offset.checked_add(4)?;
    let value = u32::from_be_bytes(bytes.get(*offset..end)?.try_into().ok()?) as usize;
    *offset = end;
    Some(value)
}
fn string<'a>(bytes: &'a [u8], offset: &mut usize) -> Option<&'a str> {
    let len = number(bytes, offset)?;
    let end = offset.checked_add(len)?;
    let value = std::str::from_utf8(bytes.get(*offset..end)?).ok()?;
    *offset = end;
    Some(value)
}

fn evaluate(bytes: &[u8], match_work: u64) -> Option<(u8, u64)> {
    let mut offset = 1;
    match *bytes.first()? {
        1 => {
            let pattern = string(bytes, &mut offset)?;
            let count = number(bytes, &mut offset)?;
            // v enables code-point matching on UTF-16 as well as Unicode sets.
            let flags = Flags {
                unicode: true,
                unicode_sets: true,
                ..Flags::default()
            };
            // HTML compiles the original first, so invalid source cannot become
            // valid by closing an authored group against the implicit wrapper.
            if Regex::with_flags(pattern, flags).is_err() {
                return Some((0, 0));
            }
            let anchored = format!("^(?:{})$", pattern);
            let regex = Regex::with_flags(&anchored, flags).ok()?;
            let compile_work = budget::interpreter(match_work);
            let mut mismatch = false;
            for _ in 0..count {
                budget::step();
                let value = string(bytes, &mut offset)?;
                let utf16: Vec<u16> = value
                    .encode_utf16()
                    .map(|unit| {
                        budget::step();
                        unit
                    })
                    .collect();
                let matched = regex.find_from_utf16(&utf16, 0).next();
                mismatch |= !matches!(matched, Some(found) if found.start() == 0 && found.end() == utf16.len());
            }
            if offset != bytes.len() {
                return None;
            }
            Some((if mismatch { 2 } else { 1 }, compile_work))
        }
        2 => {
            let value = string(bytes, &mut offset)?;
            if offset != bytes.len() {
                return None;
            }
            // Genuine WHATWG parsing without a base. Parsing grants no access.
            for _ in value.chars() {
                budget::step();
            }
            let valid = url::Url::parse(value).is_ok();
            Some((if valid { 3 } else { 4 }, 0))
        }
        _ => None,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.len() != 6 {
        std::process::exit(80);
    }
    let ceilings: Option<Vec<u64>> = args
        .iter()
        .map(|value| value.parse::<u64>().ok().filter(|v| *v > 0))
        .collect();
    let Some(ceilings) = ceilings else {
        std::process::exit(80)
    };
    let timeout = ceilings[5];
    std::thread::spawn(move || {
        std::thread::sleep(std::time::Duration::from_millis(timeout));
        std::process::exit(78);
    });
    budget::start(ceilings[0], ceilings[2], ceilings[3]);
    let mut prefix = [0; 4];
    if std::io::stdin().read_exact(&mut prefix).is_err() {
        std::process::exit(80);
    }
    let size = u32::from_be_bytes(prefix) as usize;
    if size as u64 > ceilings[4] {
        std::process::exit(77);
    }
    let mut bytes = vec![0; size];
    if std::io::stdin().read_exact(&mut bytes).is_err() {
        std::process::exit(80);
    }
    let Some((state, compile)) = evaluate(&bytes, ceilings[1]) else {
        std::process::exit(80)
    };
    let (allocated, work) = budget::usage();
    let (compile, work) = if matches!(state, 0 | 3 | 4) {
        (work, 0)
    } else {
        (compile, work)
    };
    let mut result = [0; 25];
    result[0] = state;
    result[1..9].copy_from_slice(&compile.to_be_bytes());
    result[9..17].copy_from_slice(&work.to_be_bytes());
    result[17..25].copy_from_slice(&allocated.to_be_bytes());
    let mut output = std::io::stdout().lock();
    if output
        .write_all(&(result.len() as u32).to_be_bytes())
        .and_then(|_| output.write_all(&result))
        .is_err()
    {
        std::process::exit(80);
    }
}
