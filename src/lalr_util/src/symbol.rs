use std::collections::{HashMap, *};
use std::fmt::Formatter;
use std::hash::*;

use ref_thread_local::RefThreadLocal;
use serde::{Serialize, Serializer};

#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub struct Symbol(i64);

impl Serialize for Symbol {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

impl Symbol {
    pub fn as_non_terminal(self) -> Symbol {
        Symbol(-self.0.abs())
    }
    pub fn as_terminal(self) -> Symbol {
        Symbol(self.0.abs())
    }
    pub fn is_terminal(&self) -> bool {
        self.0 > 0
    }
    pub fn is_non_terminal(&self) -> bool {
        self.0 < 0
    }
    pub fn is_bottom(&self) -> bool {
        self.0 == BOTTOM.0
    }
    pub fn as_str(&self) -> &'static str {
        decode(*self)
    }
}

impl std::convert::From<&str> for Symbol {
    fn from(s: &str) -> Self {
        hash(s)
    }
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if let Some(width) = f.width() {
            write!(f, "{:<width$}", decode(*self), width = width)
        } else {
            write!(f, "{}", decode(*self))
        }
    }
}

pub const BOTTOM: Symbol = Symbol(0);

ref_thread_local! {
    static managed SYMBOL: HashMap<Symbol, String> = HashMap::new();
}

fn hash(x: &str) -> Symbol {
    use hash_map::DefaultHasher;
    let mut s = DefaultHasher::new();
    s.write(x.as_bytes());
    let val = s.finish() as i64 & (-1i64 as u64 >> 1) as i64;
    let val = Symbol(val);
    SYMBOL
        .borrow_mut()
        .insert(val.as_terminal(), String::from(x));
    SYMBOL
        .borrow_mut()
        .insert(val.as_non_terminal(), String::from(x));
    // println!("{} => {:?}", x, val);
    val
}

fn decode(val: Symbol) -> &'static str {
    match val {
        BOTTOM => return &"$",
        _ => {}
    }
    match SYMBOL.borrow().get(&val) {
        Some(val) => unsafe { &*(val.as_str() as *const str) },
        _ => {
            panic!("unknown value");
        }
    }
}
