use std::fs;
use std::iter;
fn main() {
    let data = fs::read_to_string("./data/rosalind_hamm.txt").expect("no file");
    let mut ls = data.lines();
    let dna1 = ls.next().unwrap();
    let dna2 = ls.next().unwrap();

    println!(
        "{}",
        iter::zip(dna1.chars(), dna2.chars()).fold(0, |acc, (a, b)| if a == b {
            acc
        } else {
            acc + 1
        })
    );
}
