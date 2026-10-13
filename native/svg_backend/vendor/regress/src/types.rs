use crate::codepointset::CodePointSet;
use crate::position::PositionType;
#[cfg(not(feature = "std"))]
use alloc::string::String;
use core::ops;

/// A group index is u16.
/// CaptureGroupID 0 corresponds to the first capture group.
pub type CaptureGroupID = u16;

/// The name of a named capture group.
pub type CaptureGroupName = String;

/// Fixed u16 bytecode operand representation, not a tunable resource limit.
pub const MAX_CAPTURE_GROUPS: usize = 65535;

/// Fixed u16 bytecode operand representation, not a tunable resource limit.
pub const MAX_LOOPS: usize = 65535;
pub type LoopID = u16;


#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CharacterClassType {
    Digits,
    Spaces,
    Words,
}

/// The stuff in a bracket.
#[derive(Debug, Clone, Default)]
pub struct BracketContents {
    pub invert: bool,
    pub cps: CodePointSet,
}

impl BracketContents {
    // Return true if the bracket is empty.
    pub fn is_empty(&self) -> bool {
        let _budget_depth = crate::budget::enter();
        match self.invert {
            false => self.cps.is_empty(),
            true => self.cps.contains_all_codepoints(),
        }
    }
}

/// An instruction pointer.
pub type IP = usize;

/// Representation of a loop.
#[derive(Debug, Copy, Clone)]
pub struct LoopData<Position: PositionType> {
    pub iters: usize,
    pub entry: Position,
}

impl<Position: PositionType> LoopData<Position> {
    pub fn new(entry: Position) -> LoopData<Position> {
        let _budget_depth = crate::budget::enter();
        LoopData { iters: 0, entry }
    }
}

/// Representation of a capture group.
#[derive(Debug, Copy, Clone)]
pub struct GroupData<Position: PositionType> {
    pub start: Option<Position>,
    pub end: Option<Position>,
}

impl<Position: PositionType> GroupData<Position> {
    pub fn new() -> GroupData<Position> {
        let _budget_depth = crate::budget::enter();
        GroupData {
            start: None,
            end: None,
        }
    }

    pub fn start_matched(&self) -> bool {
        let _budget_depth = crate::budget::enter();
        self.start.is_some()
    }

    pub fn end_matched(&self) -> bool {
        let _budget_depth = crate::budget::enter();
        self.end.is_some()
    }

    pub fn as_range(&self) -> Option<ops::Range<Position>> {
        let _budget_depth = crate::budget::enter();
        // Note: we may have only start_matched (if forwards) or end_matched (if
        // backwards) set.
        match (self.start, self.end) {
            (Some(start), Some(end)) => Some(ops::Range { start, end }),
            _ => None,
        }
    }

    /// Reset the group to "not entered."
    pub fn reset(&mut self) {
        let _budget_depth = crate::budget::enter();
        self.start = None;
        self.end = None;
    }
}
