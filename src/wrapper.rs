use super::ast::Ast;

pub trait Callback<'a, 'b, T> {
    fn wrap(self) -> Box<'b + Fn(&Ast<T>) -> Option<T>>;
}

impl<'a, 'b, T> Callback<'a, 'b, T> for Box<'b + Fn(&Ast<T>) -> Option<T>> {
    fn wrap(self) -> Box<'b + Fn(&Ast<T>) -> Option<T>> {
        self
    }
}

impl<'a, 'b, T> Callback<'a, 'b, T> for Box<'b + Fn(&Ast<T>) -> T>
where
    T: 'a,
    'a: 'b,
{
    fn wrap(self) -> Box<'b + Fn(&Ast<T>) -> Option<T>> {
        Box::new(move |ast: &Ast<T>| Some(self(ast)))
    }
}
