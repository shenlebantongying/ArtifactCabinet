use std::env;
use std::process::Command;

fn main() {
    let args: Vec<String> = env::args().collect();

    let cmd = if args.len() >= 2 {
        args[1].to_string()
    } else {
        "pwd".to_string()
    };

    let output = Command::new(cmd).output();

    println!(
        "{}",
        match output {
            Ok(o) => {
                str::from_utf8(&o.stdout).unwrap().to_string()
            }
            Err(_) => {
                "err".to_string()
            }
        }
    )
}
