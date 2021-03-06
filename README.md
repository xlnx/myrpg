# Grammar

LALR

# How does it work

MyRpg use the `lang!` macro to collect essential grammar details and build LALR parsing table **AT COMPILE TIME**.

The lexical analysis is proceeded along with parsing, instead of pre-collecting all the tokens. 

# Subscribe parsing events

MyRpg supports subscribing multiple parsing events, which makes it more flexible:

## Token Generation

Triggers when a new token was generated by lexer.

```rust
<terminal> => <regex> => |tok: Token| -> Option<Token> {
	// do something with tok
	Some(tok)  // or None
}
```

## Reduction (Ast Generation)

Triggers when a production was reduced by parser.

```rust
<non-terminal> => [
	[ <symbol> ] => @reduce |ast: &mut Ast<_>| -> () {
		// do something with ast
	}
]
```

## Execution

Triggers when the parsing job finishes successfully. Make a traversal over the AST.

```rust
<non-terminal> => [
	[ <symbol> ] => |/* match this production */| -> () {
		// do something with ast
	}
]
```

This function is marked as returning `Option<ValueType>`, in case some node doesn't return anything. As a result, this function has it's default behaviour: expand all child ASTs and return the last's value.

Note that current AST node will be automatically destructured into children, so the function parameters must match your production declaration, e.g:
```rust
Expr "+" Term => |lhs, _, rhs| -> _ {
	Some(lhs.gen().unwrap() + rhs.gen().unwrap())
}
```

If you override this function, you have to call `ast.gen()` manually to traverse child ASTs.



# Usage

## Default traverse method

```rust
use myrpg::{ast::*, LRParser, *};

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
			println!("{:?}", child);
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

#[test]
fn test_mathexpr() {
	let parser = LRParser::<MathExpr>::new();
	let res = parser.parse("4 * (2 + 1)");
	println!("{:?}", res.unwrap().gen());
}

```



```rust
use myrpg::{ast::*, LRParser, *};

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

    match parser.parse("if a b else c") {
        Ok(val) => println!("{:?}", val),
        Err(err) => println!("{:?}", err),
    }
}
```

# Status

Ready, under construction.
