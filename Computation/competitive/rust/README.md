# Codeforces Solutions written in Rust
Competitive programming in Rust. Solutions weight logic elegance slightly more than speed.

<https://codeforces.com>

# Super Easy Problems
| ID   | Notes|
|:----|:----|
|[4A - Watermelon](./4A.rs)| handle blank line |
|[71A - Way Too Long Words](./71A.rs)| better handling lines|
|[263A - Beautiful Matrix](./263A.rs)| tokenize numbers|
|[160A - A. Twins ](./160A.rs)||

# Lighting Fast Coder

## Tricks

```
let v: Vec<i32> = (0..10).map(|x| 2*x+1).collect();
```
## Copy-Pasta

#### CF style the first line as "indicator number"

```rust
fn main() {
    let stdin = io::stdin();
    let mut counter:u32=0;
    for (n,line) in stdin.lock().lines().enumerate(){
        if n<1{
            counter=line.unwrap().trim().parse::<u32>().unwrap();
        } else {
            // Do things
            // + counter -> first line single number
            // + n -> line number
            // + line -> line content
        }
    }
}
```

#### Idiomatic (CRAP) way to split a string to vec<u32>

```rust
l.split_whitespace()
    .map(|s| s.trim())
    .filter(|s| !s.is_empty())
    .map(|s| s.parse::<u32>().unwrap())
    .collect::<Vec<u32>>();
```
# TODO:

+ Line scanner?
+ Borrowing lifetime for nested structures
