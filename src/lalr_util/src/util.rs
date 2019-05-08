use pretty::{Doc, *};

pub trait ToDoc {
    fn to_doc(&self) -> Doc<BoxDoc<()>>;
}

pub trait AsString {
    fn as_string(&self) -> String;
}

impl<T> AsString for T
where
    T: ToDoc,
{
    fn as_string(&self) -> String {
        let mut w = Vec::new();
        self.to_doc().render(128, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}
