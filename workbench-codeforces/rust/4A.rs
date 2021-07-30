use std::io::{stdin, BufWriter, Write, stdout};

fn main() {
    let mut input = String::new();
    stdin().read_line(&mut input).expect("Failed");

    // NOTICE: inplace trim
    input.truncate(input.trim_end().len());

    let int_input=input.parse::<i32>().unwrap();

    let out = &mut BufWriter::new(stdout());

    if int_input > 2 && (input.parse::<i32>().unwrap())&1 != 1{
        write!(out,"YES\n").ok();
    } else {
        write!(out,"NO\n").ok();
    }
}