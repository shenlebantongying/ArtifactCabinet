struct D {
    a: String,
    b: String,
}

fn main() {
    let v: Box<D> = Box::new(D {
        a: String::from("asd"),
        b: String::from("asd"),
    });

    println!("{} + {}", v.a, v.b);
}
