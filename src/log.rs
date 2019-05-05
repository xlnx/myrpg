use std::io::Write;

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

pub struct SourceFileLocation<'a> {
    pub name: String,
    pub line: &'a str,
    pub from: (usize, usize),
    pub to: (usize, usize),
}

pub struct Item<'a> {
    pub level: Severity,
    pub location: Option<SourceFileLocation<'a>>,
    pub message: String,
}

impl<'a> Item<'a> {
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
    pub fn log<'b>(&mut self, item: &Item<'b>) {
        let Item {
            level,
            location: loc,
            message,
        } = item;
        
        let message = format!("{}: {}", level.apply(level.as_str()), message);

        writeln!(self.writer, "{}", message.bold()).unwrap();

        if let Some(loc) = loc {
            let line = String::from(if let Some(pos) = loc.line.find(|c: char| c == '\n') {
                &loc.line[..pos]
            } else {
                loc.line
            });
            let trimmed = line.as_str(); //.trim_end();

            let (begin, end) = (loc.from.1, loc.to.1);

            let mut pos = [begin, end];
            let trimmed = replace_tab(trimmed, &mut pos);
            let [begin, end] = pos;

            let anchor_num = format!("{} | ", loc.from.0 + 1);
            let anchor = format!("{:width$} | ", "", width = anchor_num.len() - 3);

            writeln!(
                self.writer,
                "{:>width$} {}:{}:{}",
                "-->".blue().bold(),
                loc.name,
                loc.from.0 + 1,
                loc.from.1 + 1,
                width = anchor_num.len()
            )
            .unwrap();

            writeln!(self.writer, "{}", anchor.blue().bold()).unwrap();

            write!(
                self.writer,
                "{}{}",
                anchor_num.blue().bold(),
                &trimmed.as_str()[..begin]
            )
            .unwrap();
            write!(
                self.writer,
                "{}",
                level.apply(&trimmed.as_str()[begin..end])
            )
            .unwrap();
            writeln!(self.writer, "{}", &trimmed.as_str()[end..]).unwrap();

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
        }

        writeln!(self.writer).unwrap();
    }
}
