use std::cell::RefCell;
use std::cmp::max;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq)]
pub struct TreeNode {
    pub val: i32,
    pub left: Option<Rc<RefCell<TreeNode>>>,
    pub right: Option<Rc<RefCell<TreeNode>>>,
}

impl TreeNode {
    pub fn new(
        val: i32,
        left: Option<Rc<RefCell<TreeNode>>>,
        right: Option<Rc<RefCell<TreeNode>>>,
    ) -> Option<Rc<RefCell<TreeNode>>> {
        Some(Rc::new(RefCell::new(TreeNode { val, left, right })))
    }

    pub fn new_end(val: i32) -> Option<Rc<RefCell<TreeNode>>> {
        Self::new(val, None, None)
    }
}

fn largest_values(root_note: Option<Rc<RefCell<TreeNode>>>) -> Vec<i32> {
    let mut results = Vec::new();
    let mut level_nodes = Vec::from([root_note]);

    while !level_nodes.is_empty() {
        let mut temp_max = None;

        level_nodes = level_nodes.iter().fold(Vec::new(), |mut acc, temp_node| {
            if let Some(v) = temp_node {
                match temp_max {
                    Some(cur_max) => temp_max = Some(max(cur_max, v.borrow().val)),
                    None => temp_max = Some(v.borrow().val),
                }

                acc.push(v.borrow().left.clone());
                acc.push(v.borrow().right.clone());
            }
            acc
        });

        if let Some(temp_v) = temp_max {
            results.push(temp_v);
        }
    }
    results
}

fn main() {
    // find the biggest value each row
    //  1
    //  |\
    //  3 2
    //  |\  \
    //  5 3  9
    //  |
    //  7
    //  => [1,3,9,7]

    let tree = TreeNode::new(
        1,
        TreeNode::new(
            3,
            TreeNode::new(5, TreeNode::new_end(7), None),
            TreeNode::new_end(3),
        ),
        TreeNode::new(2, TreeNode::new_end(9), None),
    );

    println!("{:?}", largest_values(tree));
}
