use std::io::Write;

use colored::Colorize;

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
    fn apply(&self) {}
}

pub struct SourceFileLocation<'a> {
    pub name: String,
    pub line: &'a str,
    pub from: (usize, usize),
    pub len: usize,
}

pub struct Item<'a> {
    pub level: Severity,
    pub location: SourceFileLocation<'a>,
    pub message: String,
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

// impl<'a> std::convert::From<&'a mut Write> for Logger<'a> {
//     fn from(writer: &'a mut Write) -> Self {
//         Logger {
//             writer
//         }
//     }
// }

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

        let line = String::from(if let Some(pos) = loc.line.find(|c: char| c == '\n') {
            &loc.line[..pos]
        } else {
            loc.line
        });
        let trimmed = line.as_str(); //.trim_end();

        //        let (begin, end) = if let Some(Token { val, pos, .. }) = self.token {
        //            (
        //                pos.1 as usize,
        //                self.token.as_ref().unwrap().val.len() + pos.1 as usize
        //            )
        //        } else {
        //            (
        //                trimmed.len(),
        //                trimmed.len() + 1
        //            )
        //        };
        let (begin, end) = (loc.from.1, loc.from.1 + loc.len);

        let mut pos = [begin, end];
        let trimmed = replace_tab(trimmed, &mut pos);
        let [begin, end] = pos;

        let message = format!("{}: {}", level.as_str().red(), message);

        writeln!(self.writer, "{}", message.bold()).unwrap();

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
        write!(self.writer, "{}", &trimmed.as_str()[begin..end].red()).unwrap();
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
            std::iter::repeat('^')
                .take(end - begin)
                .collect::<String>()
                .red()
                .bold()
        )
        .unwrap();

        writeln!(self.writer).unwrap();
    }
}
