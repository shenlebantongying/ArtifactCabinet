use std::fs;

fn main() {
    let dna = fs::read_to_string("./data/rosalind_rna.txt").expect("no file");

    let mut rna = String::with_capacity(dna.len());

    dna.chars().for_each(|c| {
        rna.push(match c {
            'T' => 'U',
            _ => c,
        })
    });

    println!("{rna}");
}
