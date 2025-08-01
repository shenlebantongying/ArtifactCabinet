use std::io;
use std::io::BufRead;
use std::process::exit;

// TODO: per-allocate a fixed vec?
// TODO: how to modify global vars?

fn main() {
    let stdin = io::stdin();
    for (n, line) in stdin.lock().lines().enumerate() {
        if n < 1 {
        } else {
            let nums = line
                .unwrap()
                .split_whitespace()
                .map(|s| s.trim())
                .filter(|s| !s.is_empty())
                .map(|s| s.parse::<i64>().unwrap())
                .collect::<Vec<i64>>();

            let mut flipper: i32 = 1;
            let mut acc:i64 = 0;
            let mut zeros:i64 =0;

            for n in nums {

                if n==0{
                    zeros+=1;
                } else
                if n>0 {
                    acc+=n-1;
                } if n<0 {
                    flipper*=-1;
                    acc+= -n-1;
                }
            }

            if flipper > 0 {
                println!("{}",acc+zeros);
            } else {
                if zeros==0{
                    println!("{}",acc+2);
                } else {
                    println!("{}",acc+zeros);
                }
            }

            exit(0);
        }
    }
}
