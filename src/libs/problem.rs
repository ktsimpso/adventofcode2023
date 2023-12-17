use std::fmt::{self, Display};

pub enum ProblemResult {
    Isize(isize),
    Usize(usize),
    U32(u32),
    String(String),
}

impl Display for ProblemResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProblemResult::Isize(val) => val.fmt(f),
            ProblemResult::Usize(val) => val.fmt(f),
            ProblemResult::U32(val) => val.fmt(f),
            ProblemResult::String(val) => val.fmt(f),
        }
    }
}

impl From<isize> for ProblemResult {
    fn from(item: isize) -> Self {
        ProblemResult::Isize(item)
    }
}

impl From<usize> for ProblemResult {
    fn from(item: usize) -> Self {
        ProblemResult::Usize(item)
    }
}

impl From<String> for ProblemResult {
    fn from(item: String) -> Self {
        ProblemResult::String(item)
    }
}

impl From<u32> for ProblemResult {
    fn from(value: u32) -> Self {
        ProblemResult::U32(value)
    }
}

pub trait Problem<I, A> {
    type Output: Into<ProblemResult>;

    fn run(input: I, arguments: &A) -> Self::Output;
}
