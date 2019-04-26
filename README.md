# Usage


```rust
use myrpg::{ast::*, wrapper::Callback, LRParser, *};

lang! {

	Name = MathExpr
	ValueType = i32

	;;

	Number => r"[0-9]+",
	Id => r"[a-zA-Z_]+",
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
		Expr => |child: &Ast<_>| -> _ {
			let res = child.gen().unwrap();
			println!("{}", res);
			Some(res)
		}
	],
	Expr => [
		Expr Add Term => |lhs: &Ast<_>, _, rhs: &Ast<_>| -> _ {
			Some(lhs.gen().unwrap() + rhs.gen().unwrap())
		},
		Expr Sub Term => |lhs: &Ast<_>, _, rhs: &Ast<_>| -> _ {
			Some(lhs.gen().unwrap() - rhs.gen().unwrap())
		},
		Term => |child: &Ast<_>| -> _ {
			child.gen()
		}
	],
	Term => [
		Term Mul Factor => |lhs: &Ast<_>, _, rhs: &Ast<_>| -> _ {
			println!("{}", lhs.as_string(10));
			Some(lhs.gen().unwrap() * rhs.gen().unwrap())
		},
		Term Div Factor => |lhs: &Ast<_>, _, rhs: &Ast<_>| -> _ {
			Some(lhs.gen().unwrap() / rhs.gen().unwrap())
		},
		Factor => |child: &Ast<_>| -> _ {
			child.gen()
		}
	],
	Factor => [
		Number => |tok: &Token| -> Option<i32> {
			Some(tok.val.parse().unwrap())
		},
		LBracket Expr RBracket => |_, child: &Ast<_>, _| -> _ {
			child.gen()
		}
	]

}

#[test]
fn test_math_expr() {
	let parser = LRParser::<MathExpr>::new();

	let res = parser.parse("3 * (1 + 2)");

	assert_eq!(res, Ok(Some(9)));
}

```

# Status

Ready, under construction.
