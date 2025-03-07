///
/// Goal: Load a csv file, then give a frequency table.
/// Sub goals:
/// + Use pivot table
///
/// TODO: not good
///

/// ordered by default.
use std::collections::BTreeMap;
use std::collections::BTreeSet;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct ColRow<'a> {
    col: &'a str,
    row: &'a str,
}

fn main() {
    let mut csv_lines = include_str!("d.csv").split("\n");

    // first line
    let labels = Vec::from_iter(csv_lines.next().unwrap().trim().split(","));

    let mut unpivot: Vec<(&str, &str)> = Vec::new();
    csv_lines.for_each(|l| {
        l.trim()
            .split(",")
            .into_iter()
            .enumerate()
            .into_iter()
            .for_each(|(pos, v)| unpivot.push((labels[pos], v)))
    });

    let mut count_map: BTreeMap<ColRow, i32> = BTreeMap::new();
    let mut cols: BTreeSet<&str> = BTreeSet::new();
    let mut rows: BTreeSet<&str> = BTreeSet::new();

    for (col, row) in unpivot {
        count_map
            .entry(ColRow { col, row })
            .and_modify(|c| *c += 1)
            .or_insert(1);
        cols.insert(col);
        rows.insert(row);
    }

    println!("   {}", Vec::from_iter(rows.iter().copied()).join(" "));

    for col_name in cols {
        print!("{} ", col_name);
        for (_, v) in count_map
            .iter()
            .filter(|&(col_row, _)| (col_row.col == col_name))
        {
            print!("{} ", v)
        }
        println!();
    }
}
