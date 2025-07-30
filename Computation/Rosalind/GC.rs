use std::collections::HashMap;
use std::{fs, ops};

#[derive(Copy, Clone)]
struct Stats {
    gc: i64,
    total: i64,
}

impl ops::Add<Stats> for &mut Stats {
    type Output = ();

    fn add(self, rhs: Stats) -> Self::Output {
        self.gc += rhs.gc;
        self.total += rhs.total;
    }
}

fn count_line(s: &str) -> Stats {
    let mut r = Stats {
        gc: 0,
        total: s.trim().len() as i64,
    };

    s.chars().for_each(|c| match c {
        'C' | 'G' => r.gc += 1,
        _ => {}
    });
    r
}

fn main() {
    let data = fs::read_to_string("./data/rosalind_gc.txt").expect("no file");
    let lines = data.lines();

    let mut records: HashMap<&str, Stats> = HashMap::new();
    {
        let mut current_record: &str = "";
        lines.for_each(|l| {
            if let Some(record_name) = l.strip_prefix('>') {
                current_record = record_name;
            } else {
                let line_stats = count_line(l);
                records
                    .entry(current_record)
                    .and_modify(|s| s + line_stats)
                    .or_insert(line_stats);
            }
        });
    }

    {
        let mut max_gc: f64 = 0.0;
        let mut best_record: &str = "";
        for (k, v) in records {
            let gc = (v.gc as f64 / v.total as f64) * 100.0;
            if gc > max_gc {
                max_gc = gc;
                best_record = k;
            }
        }
        println!("{best_record}\n{max_gc:.6}");
    }
}
