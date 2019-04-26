use myrpg::{ast::*, LRParser, *};

// lang! {

// 	Name = MathExpr
// 	ValueType = i32

// 	;;

// 	Number => r"[0-9]+",
// 	Id => r"[a-zA-Z_]+",
// 	Add => r"\+",
// 	Sub => r"-",
// 	Mul => r"\*",
// 	Div => r"/",
// 	LBracket => r"\(",
// 	RBracket => r"\)",

// 	;;

// 	S => [
// 		Value
// 	],
// 	Value => [
// 		Expr => |child| -> _ {
// 			let res = child.gen().unwrap();
// 			println!("{:?}", child);
// 			Some(res)
// 		}
// 	],
// 	Expr => [
// 		Expr Add Term => |lhs, _, rhs| -> _ {
// 			Some(lhs.gen().unwrap() + rhs.gen().unwrap())
// 		},
// 		Expr Sub Term => |lhs, _, rhs| -> _ {
// 			Some(lhs.gen().unwrap() - rhs.gen().unwrap())
// 		},
// 		Term => |child| -> _ {
// 			child.gen()
// 		}
// 	],
// 	Term => [
// 		Term Mul Factor => |lhs, _, rhs| -> _ {
// 			Some(lhs.gen().unwrap() * rhs.gen().unwrap())
// 		},
// 		Term Div Factor => |lhs, _, rhs| -> _ {
// 			Some(lhs.gen().unwrap() / rhs.gen().unwrap())
// 		},
// 		Factor => |child| -> _ {
// 			child.gen()
// 		}
// 	],
// 	Factor => [
// 		Number => |tok| -> Option<i32> {
// 			Some(tok.val.parse().unwrap())
// 		},
// 		LBracket Expr RBracket => |_, child, _| -> _ {
// 			child.gen()
// 		}
// 	]

// }

// #[test]
// fn test_math_expr() {
// 	let parser = LRParser::<MathExpr>::new();

// 	let res = parser.parse("3 * (1 + 2)");

// 	assert_eq!(res, Ok(Some(9)));
// }

lang! {

	Name = MoveInReduce
	ValueType = i32

	;;

	Id => r"[0-9]+",
	Add => r"\+",
	Mul => r"\*",
	LBra => r"\(",
	RBra => r"\}"

	;;

	S => [
		E
	],
	E => [
		E Add T,
		T
	],
	T => [
		T Mul F,
		F
	],
	F => [
		LBra E RBra,
		Id
	]

}

#[test]
fn test_move_in_reduce() {
	let parser = LRParser::<MoveInReduce>::new();
	println!("{}", parser.get_closure());
	println!("{}", parser.get_parse_table());
}