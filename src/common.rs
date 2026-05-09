use std::cmp::{max, min};
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start_index: usize,
    pub end_index: usize,
}

impl Span {
    pub fn merge(l1: Span, l2: Span) -> Span {
        let start_index = min(l1.start_index, l2.start_index);
        let end_index = max(l1.end_index, l2.end_index);
        Span{start_index, end_index}
    }

    // To be implemented. We can only consider the start_index.
    pub fn cal_line_number(location: Span) -> i32 {
        return 1;
    }
}
