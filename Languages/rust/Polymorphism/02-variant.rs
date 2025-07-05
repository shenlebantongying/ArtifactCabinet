use crate::Base::{Variant1, Variant2};

enum Base {
    Variant1 { v: i64 },
    Variant2 { v2: i64, v3: i64 },
}

impl Base {
    fn visit(&self) -> i64 {
        match *self {
            Variant1 { v } => v,
            Variant2 { v2, v3 } => v2 + v3,
        }
    }
}

fn main() {
    let a: Vec<Base> = vec![Variant1 { v: 1 }, Variant2 { v2: 4, v3: 5 }];
    for b in a {
        println!("var -> {}", b.visit());
    }
}
