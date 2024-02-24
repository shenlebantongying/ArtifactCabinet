use std::sync::Arc;
use std::thread;

// Arc -> read-only shared data

fn main() {
    let l = Arc::new(vec![1, 2, 3]);
    let mut thread_handles = Vec::new();
    for i in 0..3 {
        let temp_l = Arc::clone(&l);
        thread_handles.push(thread::spawn(move || {
            let id = thread::current().id();
            println!("{i} via {id:?} -> {temp_l:?}");
        }))
    }

    thread_handles.into_iter().for_each(|t| t.join().unwrap());
}
