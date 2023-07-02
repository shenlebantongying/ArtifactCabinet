use std::sync::mpsc; // multiple producer, single consumer
use std::thread;
use std::time::Duration;

fn main() {
    let (tx, rx) = mpsc::channel();
    let tx2 = tx.clone();

    thread::spawn(move || {
        let thread_id = thread::current().id();
        for i in 1..5 {
            let r = tx.send(format!("From {thread_id:?} {i}"));
            if r.is_err() {
                print!("Nope");
            }
            thread::sleep(Duration::from_secs(1));
        }
    });

    thread::spawn(move || {
        let thread_id = thread::current().id();
        for i in 1..5 {
            let r = tx2.send(format!("From {thread_id:?} {i}"));
            if r.is_err() {
                print!("Nope");
            }
            thread::sleep(Duration::from_secs(1));
        }
    });

    thread::sleep(Duration::from_secs(1));

    for msg in rx.iter() {
        println!("-> {msg}");
    }
}
