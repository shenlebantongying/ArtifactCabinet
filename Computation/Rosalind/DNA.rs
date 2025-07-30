use std::collections::HashMap;
use std::fs;

fn main() {
    let data = fs::read_to_string("./data/rosalind_dna.txt").expect("no file");

    let mut d: HashMap<char, i64> = HashMap::new();

    data.chars().for_each(|c| {
        d.entry(c).and_modify(|v| *v += 1).or_insert(1);
    });

    println!(
        "{}",
        ['A', 'C', 'G', 'T']
            .into_iter()
            .map(|c| d.get(&c).unwrap().to_string())
            .collect::<Vec<String>>()
            .join(" ")
    );
}
