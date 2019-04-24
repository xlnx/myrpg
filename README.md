# Usage


```rust
lang! {

    Number => "[0-9]+",
    Id => "[a-zA-Z]+",
    Add => "+",
    Sub => "-",
    Mul => "-",
    Div => "/",

    ;;

    S => [
        Expr
    ],
    Expr => [
        Expr Add Term,
        Expr Sub Term,
        Term
    ],
    Term => [
        Term Mul Factor,
        Term Div Factor,
        Factor
    ],
    Factor => [
        Number
    ]
}

#[allow(dead_code, non_snake_case, unused_variables)]
fn main() {
    let parser = LRParser::<Lang>::new();

}
```

# Status

Under Construction
