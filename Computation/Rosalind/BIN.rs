use ros::readdlm::readdlm;

fn main() {
    let f: Vec<Vec<i64>> = readdlm("./data/rosalind_bins.txt");
    let data = &f[2];
    let target = &f[3];

    let a: Vec<String> = target
        .iter()
        .map(|t| b_search(data, *t).to_string())
        .collect();
    println!("{}", a.join(" "))
}

fn b_search<T: PartialEq + PartialOrd>(seq: &[T], target: T) -> i64 {
    let (mut begin, mut end) = (0, seq.len() - 1);

    loop {
        let mid = (begin + end) / 2; //
        if mid == begin {
            return if seq[begin] == target {
                (begin + 1) as i64
            } else if seq[end] == target {
                (end + 1) as i64
            } else {
                -1
            };
        }

        if seq[mid] <= target {
            begin = mid
        } else {
            end = mid
        }
    }
}
