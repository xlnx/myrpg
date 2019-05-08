use std::io;

use serde_json::ser::{CompactFormatter, Formatter, PrettyFormatter};

#[derive(Debug, Copy, Clone)]
enum State {
    None,
    Ast,
    AstPos,
    AfterAstPos,
    Child,
    Token,
    Pos,
}

pub struct AstFormatter<'a> {
    state: State,
    pretty: PrettyFormatter<'a>,
    compact: CompactFormatter,
}

impl<'a> AstFormatter<'a> {
    pub fn new() -> Self {
        AstFormatter {
            state: State::None,
            pretty: PrettyFormatter::new(),
            compact: CompactFormatter {},
        }
    }
    fn reduce(&mut self, is_array: bool, is_enter: bool) -> State {
        let old_state = self.state;
        match (self.state, is_array, is_enter) {
            (State::None, false, _) => {
                self.state = State::Ast;
            }
            (State::None, true, _) => {
                self.state = State::Child;
            }
            (State::Ast, _, true) => {
                self.state = State::AstPos;
            }
            (State::Ast, _, false) => {
                self.state = State::Child;
            }
            (State::AstPos, _, _) => {
                self.state = State::AfterAstPos;
            }
            (State::AfterAstPos, _, _) => {
                self.state = State::Child;
            }
            (State::Child, true, true) => {
                self.state = State::Token;
            }
            (State::Child, true, false) => {
                self.state = State::Ast;
            }
            (State::Child, false, true) => {
                self.state = State::Ast;
            }
            (State::Token, true, false) => {
                self.state = State::Child;
            }
            (State::Token, true, true) => {
                self.state = State::Pos;
            }
            (State::Pos, true, false) => {
                self.state = State::Token;
            }
            _ => panic!(),
        }
        if is_enter {
            self.state
        } else {
            old_state
        }
    }
}

impl<'a> Formatter for AstFormatter<'a> {
    #[inline]
    fn begin_array<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        match self.reduce(true, true) {
            State::Child => self.pretty.begin_array(writer),
            State::Token => self.compact.begin_array(writer),
            State::Pos | State::AstPos => self.compact.begin_array(writer),
            _ => panic!(),
        }
    }

    #[inline]
    fn end_array<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        match self.reduce(true, false) {
            State::Child => self.pretty.end_array(writer),
            State::Token => self.compact.end_array(writer),
            State::Pos | State::AstPos => self.compact.end_array(writer),
            _ => panic!(),
        }
    }

    #[inline]
    fn begin_array_value<W: ?Sized>(&mut self, writer: &mut W, first: bool) -> io::Result<()>
    where
        W: io::Write,
    {
        match self.state {
            State::Child => self.pretty.begin_array_value(writer, first),
            State::Token => self.compact.begin_array_value(writer, first),
            State::Pos | State::AstPos => self.compact.begin_array_value(writer, first),
            _ => panic!(),
        }
    }

    #[inline]
    fn end_array_value<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        match self.state {
            State::Child => self.pretty.end_array_value(writer),
            State::Token => self.compact.end_array_value(writer),
            State::Pos | State::AstPos => self.compact.end_array_value(writer),
            _ => panic!(),
        }
    }

    #[inline]
    fn begin_object<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        match self.reduce(false, true) {
            _ => self.pretty.begin_object(writer),
        }
    }

    #[inline]
    fn end_object<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        match self.reduce(false, false) {
            _ => self.pretty.end_object(writer),
        }
    }

    #[inline]
    fn begin_object_key<W: ?Sized>(&mut self, writer: &mut W, first: bool) -> io::Result<()>
    where
        W: io::Write,
    {
        self.pretty.begin_object_key(writer, first)
    }

    #[inline]
    fn end_object_key<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        self.pretty.end_object_key(writer)
    }

    #[inline]
    fn begin_object_value<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        self.pretty.begin_object_value(writer)
    }

    #[inline]
    fn end_object_value<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        self.pretty.end_object_value(writer)
    }
}
