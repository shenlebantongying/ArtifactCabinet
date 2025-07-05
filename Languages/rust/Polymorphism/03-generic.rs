use std::fmt::Display;

struct Wrapper<T> {
    v: T,
}

impl<T> Wrapper<T> {
    fn p(&self)
    where
        T: Display,
    {
        println!("Some v -> {}!", self.v)
    }
}

fn main() {
    let a: Wrapper<f64> = Wrapper { v: 1.1 };

    a.p();
}
