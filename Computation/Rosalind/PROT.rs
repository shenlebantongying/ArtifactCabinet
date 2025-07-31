use std::collections::BTreeMap;
use std::fs;

fn parse_rna_codon(s: &str) -> BTreeMap<&str, &str> {
    let chunks = s.split_ascii_whitespace();
    let mut source: &str = "";
    let mut map: BTreeMap<&str, &str> = BTreeMap::new();
    for (index, s) in chunks.enumerate() {
        if index % 2 == 0 {
            source = s;
        } else {
            map.insert(source, s);
        }
    }
    map
}

fn main() {
    let data = fs::read_to_string("./data/rosalind_prot.txt").expect("no file");
    let data_codon = fs::read_to_string("./codon_table_RNA.txt").expect("no file");
    let map = parse_rna_codon(&data_codon);

    // TODO: there are some nightly chunks in std, but not today
    let data_chunks: Vec<&str> = data
        .trim_end()
        .as_bytes()
        .chunks(3)
        .map(|c| unsafe { str::from_utf8_unchecked(c) })
        .collect();

    let mut result = String::with_capacity(data.len() / 3);
    for three in data_chunks {
        if let Some(target) = map.get(three) {
            if *target == "Stop" {
                break;
            }
            result.push_str(target);
        } else {
            panic!()
        }
    }
    println!("{result}");
}
