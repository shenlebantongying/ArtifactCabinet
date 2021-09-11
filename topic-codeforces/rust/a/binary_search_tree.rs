use std::ops::Deref;

#[derive(PartialEq, Eq,Clone)]
struct Node<'a> {
    v: &'a i32,
    l: Option<Box<Node<'a>>>,
    r: Option<Box<Node<'a>>>,
}

// TODO: it seems the generic of Rust is still bad
// TODO: wait this to be fixed
// https://github.com/rust-lang/rfcs/issues/1621
// #[derive(PartialEq, Eq,Clone)]
// struct Node<'a,T:'a>{
//     v: &'a T,
//     l: Option<Box<Node<'a, T>>>,
//     r: Option<Box<Node<'a, T>>>,
// }

impl<'a> Node<'a> {
    pub fn new(root_val: &'a i32) -> Node<'a> {
        Node {
            v: &root_val,
            l: None,
            r: None,
        }
    }

    pub fn insert(&mut self, new_v: &'a i32) {
        if self.v == new_v {
            return;
        }

        let target_node = if new_v < self.v {
            &mut self.l
        } else {
            &mut self.r
        };

        match target_node {
            &mut Some(ref mut subnode) => subnode.insert(new_v),
            &mut None => {
                let new_node = Node {
                    v: new_v,
                    l: None,
                    r: None,
                };
                *target_node = Some(Box::new(new_node))
            }
        }
    }

    pub fn left_q(&self) -> bool {
        match &self.l {
            Some(_) => return true,
            None => return false,
        }
    }

    pub fn right_q(&self) -> bool {
        match &self.l {
            Some(_) => return true,
            None => return false,
        }
    }
}

fn print(a:&Node,n:i32) {
    println!("Level {} -> {}",n,a.v);
    match &a.l {
        Some(_node) => {
            print(_node.deref(),n+1);
        }
        None => {},
    }
    match &a.r {
        Some(_node) => {
            print(_node.deref(),n+1);
        }
        None => {},
    }
}

fn main() {
    let mut my_node = Node::new(&10);
    my_node.insert(&3);
    my_node.insert(&4);
    my_node.insert(&18);
    my_node.insert(&13);
    match my_node.left_q() {
        true => {
            println!("Has left")
        }
        false => {
            println!("Has right")
        }
    }
    print(&my_node,1);
}
