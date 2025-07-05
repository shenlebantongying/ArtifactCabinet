trait Base {
    fn val(&self) -> f64;
}

struct Data1 {
    v: f64,
}

struct Data2 {
    v2: f64,
}

impl Base for Data1 {
    fn val(&self) -> f64 {
        self.v
    }
}

impl Base for Data2 {
    fn val(&self) -> f64 {
        self.v2
    }
}

/// Dynamic Dispatch
fn plus_dyn(base1: &dyn Base, base2: &dyn Base) -> f64 {
    base1.val() + base2.val()
}

/// Static Dispatch
fn plus_static<T: Base, T2: Base>(base1: &T, base2: &T2) -> f64 {
    base1.val() + base2.val()
}

fn main() {
    let d1: Box<dyn Base> = Box::new(Data1 { v: 1.0 });
    println!("{} <- dyn", plus_dyn(d1.as_ref(), &Data2 { v2: 2.0 }));

    println!(
        "{} <- static",
        plus_static(&Data1 { v: 1.0 }, &Data2 { v2: 2.0 })
    );
}
