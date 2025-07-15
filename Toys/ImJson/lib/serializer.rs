#[derive(PartialEq)]
pub enum Type {
    None,
    ObjectBegin,
    ObjectEnd,
    MemberKey,
    Comma,
    Integer,
}

pub struct Serializer<'a> {
    str_builder: Vec<&'a str>,
    current: Type,
}

impl<'a> Default for Serializer<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Serializer<'a> {
    pub fn new() -> Serializer<'a> {
        Serializer {
            str_builder: vec![],
            current: Type::None,
        }
    }

    pub fn begin_list(&mut self) {
        self.str_builder.push("[");
        self.current = Type::ObjectBegin;
    }

    pub fn end_list(&mut self) {
        self.str_builder.push("]");
        self.current = Type::ObjectEnd;
    }

    pub fn begin_object(&mut self) {
        self.str_builder.push("{");
        self.current = Type::ObjectBegin;
    }

    pub fn end_object(&mut self) {
        self.str_builder.push("}");
        self.current = Type::ObjectEnd;
    }

    /// TODO: generic key type?
    pub fn begin_member_key(&mut self, key: &'a str) {
        if self.current != Type::ObjectBegin {
            self.comma();
        }
        self.str_builder.push(key);
        self.str_builder.push(":");
        self.current = Type::MemberKey
    }

    pub fn num(&mut self, v: i64) {
        let s: &'a str = i64::to_string(&v).leak();

        if let Type::Integer = self.current {
            self.comma();
        }

        self.str_builder.push(s);
        self.current = Type::Integer;
    }

    fn comma(&mut self) {
        self.str_builder.push(",");
        self.current = Type::Comma
    }

    pub fn to_string_compact(&self) -> String {
        let capacity = self.str_builder.iter().map(|s| s.len()).sum();

        let mut ret = String::new();
        ret.reserve_exact(capacity);

        for s in self.str_builder.clone().into_iter() {
            ret.push_str(s);
        }
        ret
    }
}
