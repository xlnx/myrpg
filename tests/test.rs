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
// fn test_mathexpr() {
// 	let parser = LRParser::<MathExpr>::new();
// 	// println!("{}", parser.get_closure());
// 	// println!("{}", parser.get_parse_table());
// 	// println!("{:?}", parser.parse("aaaabbb"));
// 	let res = parser.parse("4 * (2 + 1)");
// 	println!("{:?}", res);
// }

// lang! {

// 	Name = WTF
// 	ValueType = i32

// 	;;

// 	b => r"b",
// 	a => r"a"

// 	// int => r"int",
// 	// float => r"float",
// 	// id => r"[0-9]+",
// 	// comma => r","

// 	// a => r"a",
// 	// b => r"b",
// 	// c => r"c",
// 	// d => r"d",
// 	// e => r"e"

// 	;;

// 	S => [
// 		A S,
// 		b
// 	],
// 	A => [
// 		S A,
// 		a
// 	]

// 	// decl => [
// 	// 	ty var
// 	// ],
// 	// ty => [
// 	// 	int,
// 	// 	float
// 	// ],
// 	// var => [
// 	// 	var comma id,
// 	// 	id
// 	// ]

// 	// S => [
// 	// 	a A d,
// 	// 	b B d,
// 	// 	a B e,
// 	// 	b A e
// 	// ],
// 	// A => [
// 	// 	c
// 	// ],
// 	// B => [
// 	// 	c
// 	// ]

// }

// #[test]
// fn test_wtf() {
// 	let parser = LRParser::<WTF>::new();
// 	// println!("{}", parser.get_closure());
// 	// println!("{}", parser.get_parse_table());
// 	// println!("{:?}", parser.parse("aaaabbb"));
// 	let res = parser.parse("int 0, 1, 2");
// 	println!("{:?}", res);
// }

lang! {

	Name = IfExpr
	ValueType = i32

	;;

	If => r"if",
	Else => r"else",
	Id => r"[a-zA-Z_]+"

	;;

	S => [
		Stmt
	],
	Stmt => [
		If Id Stmt Else Stmt,
		If Id Stmt,
		Id
	]

}

#[test]
fn test_ifexpr() {
	let parser = LRParser::<IfExpr>::new();
	match parser.parse("if a b else else") {
		Ok(val) => println!("{:?}", val),
		Err(err) => println!("{:?}", err),
	}
}
