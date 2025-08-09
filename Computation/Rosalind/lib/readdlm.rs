use std::fs;
use std::path::Path;

pub fn readdlm<P: AsRef<Path>, T: std::str::FromStr>(path: P) -> Vec<Vec<T>>
where
    <T as std::str::FromStr>::Err: std::fmt::Debug,
{
    let str = fs::read_to_string(path).expect("no file");
    let lines = str.lines();

    lines
        .map(|l| {
            l.split_ascii_whitespace()
                .map(|x| x.parse::<T>().unwrap())
                .collect::<Vec<T>>()
        })
        .collect::<Vec<Vec<T>>>()
}
