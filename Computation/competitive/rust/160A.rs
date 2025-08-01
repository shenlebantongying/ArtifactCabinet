use std::io;
use std::io::BufRead;
use std::process::exit;

fn main() {
    let stdin = io::stdin();
    let mut acc:u32=0;
    let mut res_counter:u32=0;
    for (n,line) in stdin.lock().lines().enumerate(){
        if n>=1 {

            let l=line.unwrap();
            let mut coins = l.split_whitespace()
                                      .map(|s| s.trim())
                                      .filter(|s| !s.is_empty())
                                      .map(|s| s.parse::<u32>().unwrap())
                                      .collect::<Vec<u32>>();
            coins.sort();
            coins.reverse();

            let half= coins.iter().sum::<u32>() /2;
            for i in coins{
                acc+=i;
                res_counter+=1;
                //println!("acc {}-res {}-counter {}-x {:?}",acc,res_counter,counter,half);
                if acc >half {
                    println!("{} ",res_counter);
                    exit(0);
                }
            }
        }
    }
}
