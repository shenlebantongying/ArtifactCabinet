//! Idea:
//! Share base level implementations to derived

trait Base {
    fn v(&self) -> f64;

    /// This is shared.
    fn double_v(&self) -> f64 {
        self.v() * 2.0
    }
}

struct Derived1 {
    some_v: f64,
}

struct Derived2 {
    another_v: f64,
}

impl Base for Derived1 {
    fn v(&self) -> f64 {
        self.some_v
    }
}

impl Base for Derived2 {
    fn v(&self) -> f64 {
        self.another_v
    }
}

fn main() {
    let ds: Vec<Box<dyn Base>> = vec![
        Box::new(Derived1 { some_v: 1.0 }),
        Box::new(Derived2 { another_v: 2.0 }),
    ];

    println!(
        "double_v total -> {}",
        ds.iter().fold(1.0, |acc, v| acc + v.double_v())
    );
}
