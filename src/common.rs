use std::cmp::{max, min};
use crate::Source_File;
use crate::FILE_RECORDS;
use colored::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub file_index: usize,
    pub start_index: usize,
    pub end_index: usize,
}

impl Span {
    pub fn merge(l1: Span, l2: Span) -> Span {
        let start_index = min(l1.start_index, l2.start_index);
        let end_index = max(l1.end_index, l2.end_index);
        assert!(l1.file_index == l2.file_index);
        Span{file_index: l1.file_index, start_index, end_index}
    }

    pub fn len(&self) -> i32 {
        return (self.end_index - self.start_index + 1).try_into().unwrap();
    }

    pub fn get_start_line(&self) -> usize {
        let mut files = FILE_RECORDS.lock().unwrap();
        let starts = &files[self.file_index].line_starts;
        let line = starts.partition_point(|&s| s <= self.start_index) - 1;
        (line + 1)
    }

    pub fn get_start_column(&self) -> usize {
        let line_index = self.get_start_line() - 1;
        let mut files = FILE_RECORDS.lock().unwrap();
        let starts = &files[self.file_index].line_starts;
        let first_char_index = starts[line_index];
        let column_index = self.start_index - first_char_index;
        (column_index + 1)
    }

    pub fn get_end_line(&self) -> usize {
        let mut files = FILE_RECORDS.lock().unwrap();
        let starts = &files[self.file_index].line_starts;
        let line = starts.partition_point(|&s| s <= self.end_index) - 1;
        (line + 1)
    }

    pub fn get_end_column(&self) -> usize {
        let line_index = self.get_end_line() - 1;
        let mut files = FILE_RECORDS.lock().unwrap();
        let starts = &files[self.file_index].line_starts;
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

pub fn get_content_at_line(source_file: &Source_File, line_no: usize) -> String {
    let (start, end) = {
        let starts = &source_file.line_starts;
        let idx = line_no - 1;                       // 1-based -> table index
        let start = starts[idx];
        let end = starts.get(idx + 1).map(|&s| s - 1); // next start minus '\n'
        (start, end)
    }; // lock released here

    let src = &source_file.content;
    match end {
        Some(end) => src[start..end].to_string(),
        None      => src[start..].to_string(),   // last line: to EOF
    }
}

pub fn error_span(span: Span, info: &str) -> String {
    let (start_line, start_column, end_line, end_column) = {
        span.locate()
    };
    let source_file = &FILE_RECORDS.lock().unwrap()[span.file_index];
    let source_file_path = source_file.path.clone();
    let line_content = get_content_at_line(source_file, start_line);

    let mut the_error = String::new();
                                                            // @Question: what is display()?
    let error_with_location = format!("{}:{}:{}: {}\n", source_file_path.display(), start_line, start_column, info.red());
    the_error.push_str(&error_with_location);
    the_error.push_str(&line_content);
    the_error.push_str("\n");
    let spaces = " ".repeat(start_column - 1);
    let arrows = if start_line == end_line {
        "^".repeat(span.end_index - span.start_index + 1)
    } else {
        "^".to_string()
    };
    the_error.push_str(&format!("{}{}", spaces, arrows.red()));
    return the_error;
}
