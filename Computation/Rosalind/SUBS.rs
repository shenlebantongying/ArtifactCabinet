use std::fs;

fn substr_find(s: &str, target: &str) -> Vec<usize> {
    let target_1st_char = target.chars().nth(0).unwrap();
    s.chars()
        .enumerate()
        .filter_map(|(i, c)| {
            if c == target_1st_char
                && ((s.len() - i) >= target.len())
                && (*target == s[i..i + target.len()])
            {
                Some(i)
            } else {
                None
            }
        })
        .collect()
}

fn main() {
    let data = fs::read_to_string("./data/rosalind_subs.txt").expect("no file");
    let mut lines = data.lines();
    let l1 = lines.next().unwrap().trim_end();
    let l2 = lines.next().unwrap().trim_end();

    for i in substr_find(l1, l2) {
        print!("{} ", i + 1);
    }
}
