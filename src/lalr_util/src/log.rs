use std::io::{BufRead, BufReader, Write};
use std::fs::File;

use colored::{ColoredString, Colorize};

#[derive(Copy, Clone)]
pub enum Severity {
    Error,
    Warning,
}

impl Severity {
    pub fn as_str(&self) -> &'static str {
        match self {
            Severity::Error => "error",
            Severity::Warning => "warning",
        }
    }
    fn apply<T>(&self, text: T) -> ColoredString
    where
        T: Colorize,
    {
        match self {
            Severity::Error => text.red(),
            Severity::Warning => text.yellow(),
        }
    }
}

pub enum SourceLocationProvider<'a> {
    File(String),
    Line(&'a str)
}

pub struct SourceFileLocation<'a> {
    pub provider: SourceLocationProvider<'a>,
    pub from: (usize, usize),
    pub to: (usize, usize),
}

pub struct LogItem<'a> {
    pub level: Severity,
    pub location: Option<SourceFileLocation<'a>>,
    pub message: String,
}

impl<'a> LogItem<'a> {
    pub fn is_error(&self) -> bool {
        if let Severity::Error = self.level {
            true
        } else {
            false
        }
    }
}

fn replace_tab(text: &str, anchors: &mut [usize]) -> String {
    const TAB_SIZE: usize = 4;

    let mut res = String::with_capacity(128);
    let mut anchor_idx: usize = 0;
    let mut dx: usize = 0;
    for (idx, ch) in text.chars().enumerate() {
        if ch == '\t' {
            for _j in 0..TAB_SIZE {
                res.push(' ');
            }
            while anchor_idx < anchors.len() && anchors[anchor_idx] <= idx {
                anchors[anchor_idx] += dx;
                anchor_idx += 1;
            }
            dx += TAB_SIZE - 1;
        } else {
            res.push(ch);
        }
    }
    while anchor_idx < anchors.len() {
        anchors[anchor_idx] += dx;
        anchor_idx += 1;
    }
    res
}

pub struct Logger<'a> {
    writer: &'a mut Write,
}

impl<'a> Logger<'a> {
    pub fn from(writer: &'a mut Write) -> Self {
        Logger { writer }
    }
}

impl<'a> Logger<'a> {
    pub fn log<'b>(&mut self, item: &LogItem<'b>) {
        let LogItem {
            level,
            location: loc,
            message,
        } = item;

        let message = format!("{}: {}", level.apply(level.as_str()), message);

        writeln!(self.writer, "{}", message.bold()).unwrap();

        if let Some(loc) = loc {

            let text_lines= match loc.provider {
                SourceLocationProvider::File(ref file_name) => Some(
                    if let Ok(f) = File::open(file_name) {
                        BufReader::new(&f).lines().into_iter()
                            .skip(loc.from.0)
                            .take(loc.to.0 - loc.from.0 + 1)
                            .map(|x| x.unwrap())
                            .collect()
                    } else {
                        vec![]      // cant open file
                    }
                ),
                _ => None
            };
            let text_lines: Vec<_> = match loc.provider {
                SourceLocationProvider::Line(ref line) => vec![*line],
                _ => text_lines.as_ref().unwrap().iter()
                    .map(|x| x.as_str()).collect()
            }.into_iter()
                .map(|x| x.trim_end())
                .collect();

            let anchor_num = format!("{} | ", loc.from.0 + 1);
            let anchor = format!("{:width$} | ", "", width = anchor_num.len() - 3);

            // location line
            writeln!(
                self.writer,
                "{:>width$} {}:{}:{}",
                "-->".blue().bold(),
                match loc.provider {
                    SourceLocationProvider::File(ref file) => file.as_str(),
                    _ => "anonymous-input"
                },
                loc.from.0 + 1,
                loc.from.1 + 1,
                width = anchor_num.len()
            )
            .unwrap();

            // 0 - anchor line
            writeln!(self.writer, "{}", anchor.blue().bold()).unwrap();

//            let mut line_begin = begin;
            let mut line_id = loc.from.0;

            for line in text_lines.iter() {
                let mut line_begin= if line_id > loc.from.0 {
                    0
                } else {
                    loc.from.1
                };
                let mut line_end = if line_id < loc.to.0 {
                    line.len()
                } else {
                    loc.to.1
                };

                line_begin += line[line_begin..].len() - line[line_begin..].trim_start().len();
                line_end -= line[..line_end].len() - line[..line_end].trim_end().len();

                let mut pos = [line_begin, line_end];
                let trimmed = replace_tab(line, &mut pos);
                let [begin, end] = pos;

                write!(
                    self.writer,
                    "{}{}",
                    anchor_num.blue().bold(),
                    &trimmed[..begin]
                )
                    .unwrap();
                write!(
                    self.writer,
                    "{}",
                    level.apply(&trimmed[begin..end])
                )
                    .unwrap();
                writeln!(self.writer, "{}", &trimmed[end..]).unwrap();

                write!(
                    self.writer,
                    "{}{}",
                    anchor.blue().bold(),
                    std::iter::repeat(' ').take(begin).collect::<String>()
                )
                    .unwrap();
                writeln!(
                    self.writer,
                    "{}",
                    level
                        .apply(
                            std::iter::repeat('^')
                                .take(end - begin)
                                .collect::<String>()
                                .as_str()
                        )
                        .bold()
                )
                    .unwrap();

                line_id += 1;
            }
        }

        writeln!(self.writer).unwrap();
    }
}
