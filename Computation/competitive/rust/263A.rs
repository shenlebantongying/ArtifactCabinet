use std::io;
use std::io::BufRead;
use std::process::exit;

fn main() {
    let stdin = io::stdin();
    for (n,line) in stdin.lock().lines().enumerate(){
            let s=line.unwrap();
            let y = s.find("1");
            match y {
                Some(one_index)=> {
                    let row:i32= (n + 1) as i32;
                    let col:i32= (one_index / 2 + 1) as i32;
                    println!("{}",(row-3).abs()+(col-3).abs());
                    exit(0);
                }
                _=>{}
            }
    }
}
