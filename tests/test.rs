use myrpg::{ast::*, log::*, LRParser, *};

lang! {

    Name = MathExpr
    ValueType = i32

    ;;

    Number => r"[0-9]+",
    Id => r"[a-zA-Z_]+"

    ;;

    S => [
        Expr => |child| -> _ {
            let res = child.gen().unwrap();
//			println!("{:?}", child);
            Some(res)
        }
    ],
    Expr => [
        Expr "+" Term => |lhs, _, rhs| -> _ {
            Some(lhs.gen().unwrap() + rhs.gen().unwrap())
        },
        Expr "-" Term => |lhs, _, rhs| -> _ {
            Some(lhs.gen().unwrap() - rhs.gen().unwrap())
        },
        Term => |child| -> _ {
            child.gen()
        }
    ],
    Term => [
        Term "*" Factor => |lhs, _, rhs| -> _ {
            Some(lhs.gen().unwrap() * rhs.gen().unwrap())
        },
        Term "/" Factor => |lhs, _, rhs| -> _ {
            Some(lhs.gen().unwrap() / rhs.gen().unwrap())
        },
        Factor => |child| -> _ {
            child.gen()
        }
    ],
    Factor => [
        Number => |tok| -> Option<i32> {
            Some(tok.val.parse().unwrap())
        },
        "(" Expr ")" => |_, child, _| -> _ {
            child.gen()
        }
    ]

}

// #[test]
// fn test_mathexpr() {
//     let parser = LRParser::<MathExpr>::new();
//     let mut stdout = std::io::stdout();
//     let mut logger = Logger::from(&mut stdout);
//     let res = parser.parse("4 * 2", &mut logger);
//     //	println!("{:?}", res);
// }

lang! {

    Name = IfExpr
    ValueType = i32

    ;;

    Id => r"[a-zA-Z_]+"

    ;;

    S => [
        Stmt
    ],
    Stmt => [
        "if" Id Stmt "else" Stmt,
        "if" Id Stmt,
        Id
    ]

}

#[test]
fn test_ifexpr() {
    let parser = LRParser::<IfExpr>::new();

    let mut stdout = std::io::stdout();
    let mut logger = Logger::from(&mut stdout);

    match parser.parse("if a b else c", &mut logger) {
        Ok(ast) => {
            let val = ast.to_json_pretty();
            println!("{}", val);
            //			println!("{:?}", val)
        }
        Err(err) => println!("{:?}", err),
    }
}
