use std::fs;

type Int = i128;

fn step(n_1: Int, n_2: Int, k: Int) -> Int {
    n_1 + k * n_2
}

fn main() {
    let data = fs::read_to_string("./data/rosalind_fib.txt").expect("no file");
    let mut split = data.split_ascii_whitespace();
    let generations = split.next().unwrap().parse::<i128>().unwrap();
    let mul = split.next().unwrap().parse::<i128>().unwrap();

    let mut n_2 = 0;
    let mut n_1 = 1;

    for _ in 0..generations - 1 {
        let old_n = n_1;

        let n = step(n_1, n_2, mul);
        println!("{n} = {n_1} + 3 * {n_2}");

        n_1 = n;
        n_2 = old_n;
    }
}
