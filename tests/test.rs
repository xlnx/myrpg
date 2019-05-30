use myrpg::*;

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
//
//#[test]
//fn test_ifexpr() {
//    let parser = LRParser::<IfExpr>::new();
//
//    let mut stdout = std::io::stdout();
//    let mut logger = Logger::from(&mut stdout);
//
//    match parser.parse("if a b else c", &mut logger) {
//        Ok(ast) => {
//            let val = ast.to_json_pretty();
//            println!("{}", val);
//            ast.print_tree();
//        }
//        Err(err) => println!("{:?}", err),
//    }
//}


lang! {

    Name = SrcLocExpr
    ValueType = ()

    ;;

    SRC_LOC => r";" => |tok, ctrl| {
        ctrl.set_location("/usr/include/stdio.h", (0, 0));
        ctrl.discard();
    },
    ID => r"[a-zA-Z][a-zA-Z_]*"

    ;;

    S => [
        Stmt,
        S Stmt
    ],
    Stmt => [
        "if" ID Stmt "else" Stmt,
        "if" ID Stmt,
        ID
    ]
}

#[test]
fn test_srclocexpr() {
    let parser = LRParser::<SrcLocExpr>::new();

    let mut stdout = std::io::stdout();
    let mut logger = Logger::from(&mut stdout);

    match parser.parse("if a b ; else c else ", &mut logger) {
        Ok((ast,_)) => {
            let val = ast.to_json_pretty();
            println!("{}", val);
            ast.print_tree();
        }
        Err(err) => println!("{:?}", err),
    }
}
