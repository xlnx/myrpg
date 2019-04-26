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
				$( $($b: ident)* $(=> $(@ $attr: ident)? $cb: expr)?),*
			]
		),*
	) => {{
		let mut lng: Vec<(
				&'a str,
				Vec<(
					Vec<&'a str>,
					Option<Box<FnType>>
				)>
			)> = vec![];

		$({
			lng.push((
				stringify!($a),
				lang!(@expand_param $res ;; $tuple @
					$( $($b)* $(=> $(@ $attr)? $cb)?),*
				)
			));
		})*
		lng
	} };
	(@expand_param
		$res: ty
		;;
		$tuple: tt
		@
		$( $($b: ident)* $(=> $(@ $attr: ident)? $cb: expr)?),*
	) => { {
		let mut rules: Vec<(
				Vec<&'a str>,
				Option<Box<FnType>>
			)> = vec![];
		$({
			let ss = vec![$(
				stringify!($b),
			)*];
			// let rule: (Vec<&str>, Option<Box<FnType>>) =
			let cb: Option<Box<FnType>> =
			if lang!(@any_expr $($cb)*) {
				lang!(@expand_callback_del
					$res
					;;
					$tuple
					@
					($($b)*)
					@
					$($(@ $attr)? $cb)?
				)
			} else {
				None
			};
			rules.push((ss, cb));
		})*
		rules
	} };
	(@expand_callback_del
		$res: ty ;; $terms: tt @ $patts: tt @
		$($(@ $attr: ident)? $cb: expr)?
	) => { {
		None as Option<Box<FnType>>
		$(
			;lang!(@expand_callback $res ;; $terms @ $patts @ ($($attr)?) @ $cb)
		)?
	} };
	(@expand_callback
		$res: ty ;; $terms: tt @ $patts: tt @ $attr: tt @
		$cb: expr
	) => { {
		Some({
			struct __Dummy;
			impl __Dummy {
				dest_callback!($res $terms $patts $attr $cb);
			}
			__Dummy::apply()
		})
		// Some(cb.wrap())
	} };

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
				$( $($b: ident)* $(=> $(@ $attr: ident)? $cb: expr)?),* $(,)?
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

				let lng = lang!(@expand_grammar $res ;; ($($l)*) @
					$(
						$a => [
							$( $($b)* $(=> $(@ $attr)? $cb)?),*
						]
					),*
				);

				(lex, lng)
			}
		}
	};
}
