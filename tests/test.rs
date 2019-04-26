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
		Expr => |ast: &Ast<_>| -> _ {
			let res = ast.childs[0].as_ast().gen().unwrap();
			println!("{}", res);
			res
		}
	],
	Expr => [
		Expr Add Term => |ast: &Ast<_>| -> _ {
			let lhs = ast.childs[0].as_ast();
			let rhs = ast.childs[2].as_ast();

			lhs.gen().unwrap() + rhs.gen().unwrap()
		},
		Expr Sub Term => |ast: &Ast<_>| -> _ {
			let lhs = ast.childs[0].as_ast();
			let rhs = ast.childs[2].as_ast();

			lhs.gen().unwrap() - rhs.gen().unwrap()
		},
		Term => |ast: &Ast<_>| -> _ {
			ast.childs[0].as_ast().gen()
		}
	],
	Term => [
		Term Mul Factor => |ast: &Ast<_>| -> _ {
			let lhs = ast.childs[0].as_ast();
			let rhs = ast.childs[2].as_ast();

			lhs.gen().unwrap() * rhs.gen().unwrap()
		},
		Term Div Factor => |ast: &Ast<_>| -> _ {
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

#[test]
fn test_math_expr() {
	let parser = LRParser::<MathExpr>::new();

	let res = parser.parse("3 * (1 + 2)");

	assert_eq!(res, Ok(Some(9)));
}
