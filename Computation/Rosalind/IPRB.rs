use std::fs;

fn main() {
    let data = fs::read_to_string("./data/rosalind_iprb.txt").expect("no file");
    let mut split = data.split_ascii_whitespace();
    let k = split.next().unwrap().parse::<f64>().unwrap();
    let m = split.next().unwrap().parse::<f64>().unwrap();
    let n = split.next().unwrap().parse::<f64>().unwrap();

    let s = k + m + n;

    // Note that after picking one, the 2nd one's probability is affected
    println!(
        "{:.5}",
        k / s // 1st -> k
        + 0.5 * m/ s // 1st -> m-dom
        + 0.5*m/ s * ( k/(s -1.0) + 0.5*(m-1.0)/(s -1.0)) // 1st -> m-rec, 2st k or m-dom
        + n/ s * (k/(s -1.0) + 0.5*m/(s -1.0)) // 1st -> n, 2st k or m-dom
    );
}
