#[macro_export]
macro_rules! any_expr {
	() => {
		false
	};
	( $x:expr ) => {
		true
	};
}

#[macro_export]
macro_rules! lang {
    (
        Name = $name: ident
        ValueType = $res: ty
        ;;
        $(
            $l: ident => $reg: expr
        ),* $(,)?
        ;;
        $(
            $a: ident => [
                $( $($b: ident)* $(=> $cb: expr)?),* $(,)?
            ]
        ),* $(,)?
    ) => {
        struct $name {}
        impl $crate::LRLang for $name {
            type Output = $res;

            fn new<'a>() -> (
				Vec<(&'a str, &'a str)>,
				Vec<(
					&'a str,
					Vec<(
						Vec<&'a str>,
						Option<Box<Fn(&Ast<$res>) -> Option<$res>>>
					)>
				)>
			) {

				type FnType = Fn(&Ast<$res>) -> Option<$res>;

				let mut lex: Vec<(&'a str, &'a str)> = vec![];
				$(
					lex.push((stringify!($l), $reg));
				)*
				let mut lng: Vec<(
						&'a str,
						Vec<(
							Vec<&'a str>,
							Option<Box<FnType>>
						)>
					)> = vec![];
                $({
					let mut rules: Vec<(
							Vec<&'a str>,
							Option<Box<FnType>>
						)> = vec![];
                    $(
						let ss = vec![$(
							stringify!($b),
						)*];
						// let rule: (Vec<&str>, Option<Box<FnType>>) =
						let cb: Option<Box<FnType>> =
						if any_expr!($($cb)*) {
							None as Option<Box<FnType>>
							$(
								;let cb: Box<Fn(&Ast<$res>) -> _> = Box::new($cb);
								Some(cb.wrap())
							)*
						} else {
							None
						};
						rules.push((ss, cb));
                    )*
					lng.push((stringify!($a), rules));
                })*

                (lex, lng)
            }
        }
    };
}
