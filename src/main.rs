
#[macro_use]
extern crate ref_thread_local;

#[macro_use]
mod parser;
use parser::{ast::*, wrapper::Callback, LRLang, LRParser};

lang! {

    Name = MathExpr
    ValueType = i32

    ;;

    Number => r"[0-9]+",
    Add => r"\+",
    Sub => r"-",
    Mul => r"\*",
    Div => r"/",
    LBracket => r"\(",
    RBracket => r"\)",

    ;;

    S => [
        Value
    ],
    Value => [
        Expr => |ast: &Ast<_>| -> Option<i32> {
            println!("{}", ast.childs[0].as_ast().gen().unwrap());
            None
        }
    ],
    Expr => [
        Expr Add Term => |ast: &Ast<_>| -> i32 {
            let lhs = ast.childs[0].as_ast();
            let rhs = ast.childs[2].as_ast();

            lhs.gen().unwrap() + rhs.gen().unwrap()
        },
        Expr Sub Term => |ast: &Ast<_>| -> i32 {
            let lhs = ast.childs[0].as_ast();
            let rhs = ast.childs[2].as_ast();

            lhs.gen().unwrap() - rhs.gen().unwrap()
        },
        Term => |ast: &Ast<_>| -> _ {
            ast.childs[0].as_ast().gen()
        }
    ],
    Term => [
        Term Mul Factor => |ast: &Ast<_>| -> i32 {
            let lhs = ast.childs[0].as_ast();
            let rhs = ast.childs[2].as_ast();

            lhs.gen().unwrap() * rhs.gen().unwrap()
        },
        Term Div Factor => |ast: &Ast<_>| -> i32 {
            let lhs = ast.childs[0].as_ast();
            let rhs = ast.childs[2].as_ast();

            lhs.gen().unwrap() / rhs.gen().unwrap()
        },
        Factor => |ast: &Ast<_>| -> _ {
            ast.childs[0].as_ast().gen()
        }
    ],
    Factor => [
        Number => |ast: &Ast<_>| -> i32 {
            let tok = ast.childs[0].as_token();
            tok.val.parse().unwrap()
        },
        LBracket Expr RBracket => |ast: &Ast<_>| -> _ {
            ast.childs[1].as_ast().gen()
        }
    ]

}

fn main() {
    let parser = LRParser::<MathExpr>::new();

    let stdin = std::io::stdin();
    let mut line = String::new();
    while let Ok(_) = stdin.read_line(&mut line) {
        parser.parse(line.as_str());
        line.clear();
    }

}
