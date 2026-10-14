//! Private instrumentation. The caller supplies every ceiling from Limits.
//! Quota exhaustion exits only the disposable native helper before further
//! interpretation or allocation, avoiding unwinding from a global allocator.
use std::cell::Cell;

thread_local! {
    static ACTIVE: Cell<bool> = const { Cell::new(false) };
    static WORK: Cell<u64> = const { Cell::new(0) };
    static WORK_LIMIT: Cell<u64> = const { Cell::new(0) };
    static WORK_EXIT: Cell<i32> = const { Cell::new(74) };
    static ALLOCATED: Cell<u64> = const { Cell::new(0) };
    static ALLOCATION_LIMIT: Cell<u64> = const { Cell::new(0) };
    static DEPTH: Cell<u64> = const { Cell::new(0) };
    static DEPTH_LIMIT: Cell<u64> = const { Cell::new(0) };
}

/// Starts cumulative allocation accounting and compiler fuel.
pub fn start(work: u64, allocation: u64, depth: u64) {
    WORK_LIMIT.set(work);
    ALLOCATION_LIMIT.set(allocation);
    DEPTH_LIMIT.set(depth);
    WORK.set(0);
    ALLOCATED.set(0);
    DEPTH.set(0);
    WORK_EXIT.set(74);
    ACTIVE.set(true);
}

/// Switches to interpreter fuel without resetting cumulative allocation.
pub fn interpreter(work: u64) -> u64 {
    let compiled = WORK.get();
    WORK.set(0);
    WORK_LIMIT.set(work);
    WORK_EXIT.set(75);
    compiled
}

/// Returns actual reserved bytes and the current phase's consumed fuel.
pub fn usage() -> (u64, u64) { (ALLOCATED.get(), WORK.get()) }

/// Charges a real allocation request before the system allocator sees it.
pub fn allocation(bytes: usize) {
    if ACTIVE.try_with(|active| active.get()).unwrap_or(false) {
        let total = ALLOCATED.get().checked_add(bytes as u64);
        match total {
            Some(total) if total <= ALLOCATION_LIMIT.get() => ALLOCATED.set(total),
            _ => std::process::exit(73),
        }
    }
}

/// Charges each compiler/VM entry, backedge and character scan.
#[inline]
pub fn step() {
    if ACTIVE.get() {
        let next = WORK.get().checked_add(1);
        match next {
            Some(next) if next <= WORK_LIMIT.get() => WORK.set(next),
            _ => std::process::exit(WORK_EXIT.get()),
        }
    }
}

/// Prevents recursive parser and lookaround stack exhaustion.
#[inline]
pub fn enter() -> Depth {
    step();
    if ACTIVE.get() {
        let next = DEPTH.get().checked_add(1);
        match next {
            Some(next) if next <= DEPTH_LIMIT.get() => DEPTH.set(next),
            _ => std::process::exit(76),
        }
    }
    Depth(ACTIVE.get())
}

pub struct Depth(bool);
impl Drop for Depth {
    fn drop(&mut self) { if self.0 { DEPTH.set(DEPTH.get() - 1); } }
}

/// Compares real code units, charging each backreference or set-string scan.
pub fn equal_slice<T: PartialEq>(left: &[T], right: &[T]) -> bool {
    if left.len() != right.len() { step(); return false; }
    for (a, b) in left.iter().zip(right) {
        step();
        if a != b { return false; }
    }
    true
}

/// A fixed bytecode operand-width limit, independent of configured budgets.
pub fn representation_exhausted() -> ! { std::process::exit(79) }
