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

    pub fn len(&self) -> i32 {
        return (self.end_index - self.start_index + 1).try_into().unwrap();
    }

    pub fn get_start_line(&self) -> usize {
        let starts = crate::LINE_STARTS.lock().unwrap();
        let line = starts.partition_point(|&s| s <= self.start_index) - 1;
        (line + 1)
    }

    pub fn get_start_column(&self) -> usize {
        let line_index = self.get_start_line() - 1;
        let starts = crate::LINE_STARTS.lock().unwrap();
        let first_char_index = starts[line_index];
        let column_index = self.start_index - first_char_index;
        (column_index + 1)
    }

    pub fn get_end_line(&self) -> usize {
        let starts = crate::LINE_STARTS.lock().unwrap();
        let line = starts.partition_point(|&s| s <= self.end_index) - 1;
        (line + 1)
    }

    pub fn get_end_column(&self) -> usize {
        let line_index = self.get_end_line() - 1;
        let starts = crate::LINE_STARTS.lock().unwrap();
        let first_char_index = starts[line_index];
        let column_index = self.end_index - first_char_index;
        (column_index + 1)
    }

    pub fn locate(&self) -> (usize, usize, usize, usize) {
        let start_line = self.get_start_line();
        let start_column = self.get_start_column();
        let end_line = self.get_end_line();
        let end_column = self.get_end_column();
        return (start_line, start_column, end_line, end_column);
    }
}

pub fn get_src_content_at_line(line_no: usize) -> String {
    let (start, end) = {
        let starts = crate::LINE_STARTS.lock().unwrap();
        let idx = line_no - 1;                       // 1-based -> table index
        let start = starts[idx];
        let end = starts.get(idx + 1).map(|&s| s - 1); // next start minus '\n'
        (start, end)
    }; // lock released here

    let src = crate::SRC.lock().unwrap();
    match end {
        Some(end) => src.chars().skip(start).take(end - start).collect(),
        None      => src.chars().skip(start).collect(),   // last line: to EOF
    }
}
