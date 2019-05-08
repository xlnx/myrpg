#[macro_export]
macro_rules! lang {
	(@any_expr) => {
		false
	};
	(@any_expr $x:expr) => {
		true
	};
	(@expand_grammar
		$res: ty
		;;
		$tuple: tt
		@
		$(
			$a: ident => [
				$( $($b: pat)* $(|$(@$j: ident),*|)? $(=> $(@ $attr: ident)* $cb: expr)*),*
			]
		),*
	) => {{
		let mut lng: Vec<(
				&'a str,
				Vec<(
					Vec<&'a str>,
					EventsType,
					Vec<&'a str>
				)>
			)> = vec![];

		$({
			lng.push((
				stringify!($a),
				lang!(@expand_param $res ;; $tuple @
					$( $($b)* $(|$(@$j),*|)? $(=> $(@ $attr)* $cb)*),*
				)
			));
		})*

		let s = lng[0].0;
		lng.insert(0, (
			"@",
			vec![(
				vec![s],
				(None, None),
				vec!["flatten"]
			)]
		));

		lng
	} };
	(@expand_param
		$res: ty
		;;
		$tuple: tt
		@
		$( $($b: pat)* $(|$(@$j: ident),*|)? $(=> $(@ $attr: ident)* $cb: expr)*),*
	) => { {
		let mut rules: Vec<(
				Vec<&'a str>,
				EventsType,
				Vec<&'a str>
			)> = vec![];
		$({
			let ss: Vec<_> = vec![$(
				stringify!($b),
			)*].into_iter().filter(|x| x != &"_").collect();
			let js: Vec<_> = vec![$(
				$(stringify!($j))*
			)?];

			let evt: EventsType =
			if lang!(@any_expr $($cb)*) {
				lang!(@expand_callback
					$res
					;;
					$tuple
					@
					($($b)*)
					@
					$($(@ $attr)* $cb)*
				)
			} else {
				(None, None)
			};
			rules.push((ss, evt, js));
		})*
		rules
	} };
	(@expand_callback
		$res: ty ;; $terms: tt @ $patts: tt @
		$($(@ $attr: ident)* $cb: expr)*
	) => { {
		{
			struct __Dummy;
			impl __Dummy {
				wrap_callback!($res $terms $patts $( ( ($($attr)*) $cb ) )* );
			}
			__Dummy::apply()
		}
	} };

	(
		Name = $name: ident
		ValueType = $res: ty
		;;
		$(
			$l: ident => $reg: expr $(=> $lcb: expr)?
		),* $(,)?
		;;
		$(
			$a: ident => [
				$( $($b: pat)* $(|$(@$j: ident),*|)? $(=> $(@ $attr: ident)* $cb: expr)*),* $(,)?
			]
		),* $(,)?
	) => {
		pub struct $name {}
		impl $crate::LRLang for $name {
			type Output = $res;

			fn new<'a>() -> (
				Vec<(&'a str, &'a str, Option<Box<Fn(&mut Token, &mut TokenCtrl) -> ()>>)>,
				Vec<(
					&'a str,
					Vec<(
						Vec<&'a str>,
						(
							Option<Box<Fn(&Ast<$res>) -> Option<$res>>>,
							Option<Box<Fn(&mut Ast<$res>) -> ()>>,
						),
						Vec<&'a str>
					)>
				)>,
				Vec<::std::collections::HashMap<Symbol, CompactAction>>
			) {

				type EventsType = (
					Option<Box<Fn(&Ast<$res>) -> Option<$res>>>,
					Option<Box<Fn(&mut Ast<$res>) -> ()>>,
				);

				let lex = {
					struct __Dummy;
					impl __Dummy {
						classify_symbols!($($l $reg ($($lcb)?) )* @ $($a)* $($($($b)*)*)*);
					}
					__Dummy::apply()
				};

				let lng = lang!(@expand_grammar $res ;; ($($l)*) @
					$(
						$a => [
							$( $($b)* $(|$(@$j),*|)? $(=> $(@ $attr)* $cb)*),*
						]
					),*
				);

				let table = {
					struct __Dummy;
					impl __Dummy {
						build_lalr_table!(
							($($l $reg ($($lcb)?) )* @ $($a)* $($($($b)*)*)*)
							(
								$((
									$a
									$((
										$($b)*
									))*
								))*
							)
						);
					}
					__Dummy::apply()
				};

				(lex, lng, table)
			}
		}
	};
}
