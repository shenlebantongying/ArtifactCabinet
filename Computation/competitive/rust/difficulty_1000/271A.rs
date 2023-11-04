use std::io::{stdin};
use std::process::exit;


fn distinct_q(y: i32) -> bool {
    // i32 to digits
    let c:Vec<u32> = y
        .to_string()
        .chars()
        .map(|c| c.to_digit(10).unwrap())
        .collect();


    if c[0] == c[1] || c[0] == c[2] || c[0] == c[3] || c[1] == c[2] || c[1] == c[3] || c[2] == c[3]
    {
        return false;
    } else {
        return true;
    }

}

fn main() {
    let mut input = String::new();
    stdin().read_line(&mut input).expect("Failed");
    input.truncate(input.trim_end().len());

    let mut year = input.parse::<i32>().unwrap();
    loop{
        year+=1;
        if distinct_q(year){
            println!("{}",year);
            exit(0);
        }
    }

}
