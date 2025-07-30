use std::fs;

fn main() {
    let s = fs::read_to_string("./data/rosalind_revc.txt").expect("no file");

    println!(
        "{}",
        s.trim_ascii_end()
            .chars()
            .map(|c| match c {
                'A' => 'T',
                'T' => 'A',
                'C' => 'G',
                'G' => 'C',
                _ => panic!(),
            })
            .rev()
            .collect::<String>()
    );
}
