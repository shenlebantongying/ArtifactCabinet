use std::io;
use std::io::BufRead;

fn main() {
    let stdin = io::stdin();
    for (n,line) in stdin.lock().lines().enumerate(){
        //                            TODO: ^ what?
        if n>=1{
            let s=line.unwrap();
            let len = s.len();
            if len>10{
                println!("{}{}{}",s.chars().nth(0).unwrap(),
                                  len-2,
                                  s.chars().nth(len-1).unwrap());
            } else {
                println!("{}",s);
            }
        } // ignore first line
    }
}