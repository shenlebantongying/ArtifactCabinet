use mini_queue::Miniq;

fn main() {
    let mut a: Miniq<i8> = Miniq::new();

    a.insert(10);
    a.insert(9);
    a.insert(11);

    println!("{a:?}");
}
