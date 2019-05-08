use myrpg::*;

// lang! {

//     Name = MathExpr
//     ValueType = ()

//     ;;

//     IDENTIFIER => r"[a-zA-Z_]\w*\b",
//         // => |tok| -> _ {
//         //     tok.symbol = Symbol::from("Number")
//         // },
//     TYPE_NAME => r"$^$^",
//     CONSTANT => r#"
//         0[xX][0-9A-Fa-f]+(?:u|U|l|L)*? |
//         [0-9]+(?:u|U|l|L)*? |
//         [a-zA-Z_]?'(?:\\.|[^\\'])+' |
//         [0-9]+[Ee][\+-]?[0-9]+(?:f|F|l|L)? |
//         [0-9]*"."[0-9]+(?:[Ee][\+-]?[0-9]+)?(?:f|F|l|L)?
//     "#,
//     STRING_LITERAL => r#"[a-zA-Z_]?"(\\.|[^\\"])*"#

//     ;;

//     S => [
//         translation_unit
//     ],

//     primary_expr => [
//         IDENTIFIER |@flatten|,
//         CONSTANT |@flatten|,
//         STRING_LITERAL |@flatten|,
//         "(" expr ")"
//     ],
//     postfix_expr => [
//         primary_expr |@flatten|,
//         postfix_expr "[" expr "]",
//         postfix_expr "(" ")",
//         postfix_expr "(" argument_expr_list ")",
//         postfix_expr "." IDENTIFIER,
//         postfix_expr "->" IDENTIFIER,
//         postfix_expr "++",
//         postfix_expr "--"
//     ],
//     argument_expr_list => [
//         assignment_expr |@flatten|,
//         argument_expr_list "," assignment_expr
//     ],
//     unary_expr => [
//         postfix_expr |@flatten|,
//         "++" unary_expr,
//         "--" unary_expr,
//         unary_operator cast_expr,
//         "sizeof" unary_expr,
//         "sizeof" "(" type_name ")"
//     ],
//     unary_operator => [
//         "&" |@flatten|,
//         "*" |@flatten|,
//         "+" |@flatten|,
//         "-" |@flatten|,
//         "~" |@flatten|,
//         "!" |@flatten|
//     ],
//     cast_expr => [
//         unary_expr |@flatten|,
//         "(" type_name ")" cast_expr
//     ],
//     multiplicative_expr => [
//         cast_expr |@flatten|,
//         multiplicative_expr "*" cast_expr,
//         multiplicative_expr "/" cast_expr,
//         multiplicative_expr "%" cast_expr
//     ],
//     additive_expr => [
//         multiplicative_expr |@flatten|,
//         additive_expr "+" multiplicative_expr,
//         additive_expr "-" multiplicative_expr
//     ],
//     shift_expr => [
//         additive_expr |@flatten|,
//         shift_expr "<<" additive_expr,
//         shift_expr ">>" additive_expr
//     ],
//     relational_expr => [
//         shift_expr |@flatten|,
//         relational_expr "<" shift_expr,
//         relational_expr ">" shift_expr,
//         relational_expr "<=" shift_expr,
//         relational_expr ">=" shift_expr
//     ],
//     equality_expr => [
//         relational_expr |@flatten|,
//         equality_expr "==" relational_expr,
//         equality_expr "!=" relational_expr
//     ],
//     and_expr => [
//         equality_expr |@flatten|,
//         and_expr "&" equality_expr
//     ],
//     exclusive_or_expr => [
//         and_expr |@flatten|,
//         exclusive_or_expr "^" and_expr
//     ],
//     inclusive_or_expr => [
//         exclusive_or_expr |@flatten|,
//         inclusive_or_expr "|" exclusive_or_expr
//     ],
//     logical_and_expr => [
//         inclusive_or_expr |@flatten|,
//         logical_and_expr "&&" inclusive_or_expr
//     ],
//     logical_or_expr => [
//         logical_and_expr |@flatten|,
//         logical_or_expr "||" logical_and_expr
//     ],
//     conditional_expr => [
//         logical_or_expr |@flatten|,
//         logical_or_expr "?" expr ":" conditional_expr
//     ],
//     assignment_expr => [
//         conditional_expr |@flatten|,
//         unary_expr assignment_operator assignment_expr
//     ],
//     assignment_operator => [
//         "=" |@flatten|,
//         "*=" |@flatten|,
//         "/=" |@flatten|,
//         "%=" |@flatten|,
//         "+=" |@flatten|,
//         "-=" |@flatten|,
//         "<<=" |@flatten|,
//         ">>=" |@flatten|,
//         "&=" |@flatten|,
//         "^=" |@flatten|,
//         "|=" |@flatten|
//     ],
//     expr => [
//         assignment_expr |@flatten|,
//         expr "," assignment_expr
//     ],
//     const_expr => [
//         conditional_expr |@flatten|
//     ],

//     decl => [
//         decl_specs_i init_declarator_list_i ";"
//     ],
//     decl_specs_i => [
//         decl_specs
//     ],
//     init_declarator_list_i => [
//         init_declarator_list,
//         _
//     ],
//     decl_specs => [
//         storage_class_spec |@flatten|,
//         decl_specs storage_class_spec,
//         type_spec |@flatten|,
//         decl_specs type_spec |@flatten|,
//         type_qulf |@flatten|,
//         decl_specs type_qulf |@flatten|
//     ],
//     init_declarator_list => [
//         init_declarator |@flatten|,
//         init_declarator_list "," init_declarator |@flatten|
//     ],
//     init_declarator => [
//         declarator,
//         declarator "=" initializer
//     ],
//     storage_class_spec => [
//         "typedef",
//         "extern",
//         "static",
//         "auto",
//         "register"
//     ],
//     type_spec => [
//         "void",
//         "char",
//         "short",
//         "int",
//         "long",
//         "float",
//         "double",
//         "signed",
//         "unsigned",
//         struct_or_union_spec,
//         enum_spec,
//         TYPE_NAME
//     ],

//     struct_or_union_spec => [
//         struct_or_union IDENTIFIER "{" struct_decl_list "}",
//         struct_or_union "{" struct_decl_list "}",
//         struct_or_union IDENTIFIER
//     ],
//     struct_or_union => [
//         "struct" |@flatten|,
//         "union" |@flatten|
//     ],
//     struct_decl_list => [
//         struct_decl_list struct_decl |@flatten|,
//         _
//     ],
//     struct_decl => [
//         spec_qulf_list_i struct_declarator_list_i ";"
//     ],
//     spec_qulf_list_i => [
//         spec_qulf_list
//     ],
//     spec_qulf_list => [
//         spec_qulf_list type_spec |@flatten|,
//         type_spec |@flatten|,
//         spec_qulf_list type_qulf |@flatten|,
//         type_qulf |@flatten|
//     ],
//     struct_declarator_list_i => [
//         struct_declarator_list
//     ],
//     struct_declarator_list => [
//         struct_declarator |@flatten|,
//         struct_declarator_list "," struct_declarator |@flatten|
//     ],
//     struct_declarator => [
//         declarator
//         // ":" const_expr, // annonymous bit field
//         // declarator ":" const_expr // bit field
//     ],

//     enum_spec => [
//         "enum" "{" enumerator_list "}",
//         "enum" IDENTIFIER "{" enumerator_list "}",
//         "enum" IDENTIFIER
//     ],
//     enumerator_list => [
//         enumerator |@flatten|,
//         enumerator_list "," enumerator |@flatten|
//     ],
//     enumerator => [
//         IDENTIFIER,
//         IDENTIFIER "=" const_expr
//     ],

//     type_qulf => [
//         "const",
//         "volatile"
//     ],
//     declarator => [
//         pointer direct_declarator,
//         direct_declarator
//     ],
//     direct_declarator => [
//         IDENTIFIER,
//         "(" declarator ")",
//         direct_declarator "[" const_expr "]",
//         direct_declarator "[" "]",
//         direct_declarator "(" param_type_list ")",
//         direct_declarator "(" identifier_list ")",
//         direct_declarator "(" ")"
//     ],
//     pointer => [
//         "*",
//         "*" type_qulf_list,
//         "*" pointer,
//         "*" type_qulf_list pointer
//     ],
//     type_qulf_list => [
//         type_qulf,
//         type_qulf_list type_qulf
//     ],
//     param_type_list => [
//         param_list,
//         param_list "," "..."
//     ],
//     param_list => [
//         param_decl,
//         param_list "," param_decl
//     ],
//     param_decl => [
//         decl_specs declarator,
//         decl_specs abstract_declarator,
//         decl_specs
//     ],
//     identifier_list => [
//         IDENTIFIER,
//         identifier_list "," IDENTIFIER
//     ],
//     type_name => [
//         spec_qulf_list,
//         spec_qulf_list abstract_declarator
//     ],
//     abstract_declarator => [
//         pointer,
//         direct_abstract_declarator,
//         pointer direct_abstract_declarator
//     ],
//     direct_abstract_declarator => [
//         "(" abstract_declarator ")",
//         "[" "]",
//         "[" const_expr "]",
//         direct_abstract_declarator "[" "]",
//         direct_abstract_declarator "[" const_expr "]",
//         "(" ")",
//         "(" param_type_list ")",
//         direct_abstract_declarator "(" ")",
//         direct_abstract_declarator "(" param_type_list ")"
//     ],
//     initializer => [
//         assignment_expr,
//         "{" initializer_list "}",
//         "{" initializer_list "," "}"
//     ],
//     initializer_list => [
//         initializer,
//         initializer_list "," initializer
//     ],
//     stmt => [
//         labeled_stmt,
//         block_stmt,
//         expr_stmt,
//         selection_stmt,
//         iteration_stmt,
//         jump_stmt
//     ],
//     labeled_stmt => [
//         IDENTIFIER ":" stmt,
//         "case" const_expr ":" stmt,
//         "default" ":" stmt
//     ],
//     block_stmt => [
//         "{" block_list "}"
//     ],
//     decl_list => [
//         decl_list decl,
//         _
//     ],
//     block_list => [
//         block_list stmt,
//         block_list decl,
//         _
//     ],
//     expr_stmt => [
//         ";",
//         expr ";"
//     ],
//     selection_stmt => [
//         "if" "(" expr ")" stmt "else" stmt,
//         "if" "(" expr ")" stmt,
//         "switch" "(" expr ")" stmt
//     ],
//     iteration_stmt => [
//         "while" "(" expr ")" stmt,
//         "do" stmt "while" "(" expr ")" ";",
//         "for" "(" expr_stmt expr_stmt ")" stmt,
//         "for" "(" expr_stmt expr_stmt expr ")" stmt
//     ],
//     jump_stmt => [
//         "goto" IDENTIFIER ";",
//         "continue" ";",
//         "break" ";",
//         "return" ";",
//         "return" expr ";"
//     ],
//     translation_unit => [
//         external_decl,
//         translation_unit external_decl
//     ],
//     external_decl => [
//         function_definition,
//         decl
//     ],
//     function_definition => [
//         // decl_specs declarator decl_list block_stmt,
//         decl_specs declarator block_stmt,
//         // declarator decl_list block_stmt,
//         declarator block_stmt
//     ]

// }

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
            ast.print_tree();
        }
        Err(err) => println!("{:?}", err),
    }
}
