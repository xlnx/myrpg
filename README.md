# Usage


```rust
lang! {

    i32

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
        Expr Add Term => |ast: &Ast<_>| -> Option<i32> {
            if let (AstNode::Ast(lhs), AstNode::Ast(rhs)) = (&ast.childs[0], &ast.childs[2]) {
                Some(lhs.gen().unwrap() + rhs.gen().unwrap())
            } else {
                panic!("WTF")
            }
        },
        Expr Sub Term => |ast: &Ast<_>| -> Option<i32> {
            if let (AstNode::Ast(lhs), AstNode::Ast(rhs)) = (&ast.childs[0], &ast.childs[2]) {
                Some(lhs.gen().unwrap() - rhs.gen().unwrap())
            } else {
                panic!("WTF")
            }
        },
        Term => |ast: &Ast<_>| -> Option<i32> {
            ast.childs[0].as_ast().gen()
        }
    ],
    Term => [
        Term Mul Factor => |ast: &Ast<_>| -> Option<i32> {
            if let (AstNode::Ast(lhs), AstNode::Ast(rhs)) = (&ast.childs[0], &ast.childs[2]) {
                Some(lhs.gen().unwrap() * rhs.gen().unwrap())
            } else {
                panic!("WTF")
            }
        },
        Term Div Factor => |ast: &Ast<_>| -> Option<i32> {
            if let (AstNode::Ast(lhs), AstNode::Ast(rhs)) = (&ast.childs[0], &ast.childs[2]) {
                Some(lhs.gen().unwrap() / rhs.gen().unwrap())
            } else {
                panic!("WTF")
            }
        },
        Factor => |ast: &Ast<_>| -> Option<i32> {
            ast.childs[0].as_ast().gen()
        }
    ],
    Factor => [
        Number => |ast: &Ast<_>| -> Option<i32> {
            if let AstNode::Token(tok) = &ast.childs[0] {
                Some(tok.val.parse().unwrap())
            } else {
                panic!("WTF")
            }
        },
        LBracket Expr RBracket => |ast: &Ast<_>| -> Option<i32> {
            ast.childs[1].as_ast().gen()
        }
    ]

}

#[allow(dead_code, non_snake_case, unused_variables)]
fn main() {
    let parser = LRParser::<Lang>::new();

    let stdin = std::io::stdin();
    let mut line = String::new();
    while let Ok(_) = stdin.read_line(&mut line) {
        parser.parse(line.as_str());
        line.clear();
    }

}

```

# Status

Under Construction
