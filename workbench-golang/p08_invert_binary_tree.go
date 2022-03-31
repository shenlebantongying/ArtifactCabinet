package main

import "fmt"

type TreeNode struct {
	Val   int
	Left  *TreeNode
	Right *TreeNode
}

func invertTree(root *TreeNode) *TreeNode {
	if root == nil {
		return nil
	}
	var left = invertTree(root.Left)
	var right = invertTree(root.Right)

	return &TreeNode{root.Val, right, left}
}

func printTree(t *TreeNode) {
	fmt.Print(t.Val, " ")
	if t.Left != nil {
		printTree(t.Left)
	}
	if t.Right != nil {
		printTree(t.Right)
	}
}

func main() {

	var inode = TreeNode{4,
		&TreeNode{2,
			&TreeNode{1, nil, nil},
			&TreeNode{3, nil, nil}},
		&TreeNode{7,
			&TreeNode{6, nil, nil},
			&TreeNode{9, nil, nil}}}

	printTree(&inode)
	fmt.Println()
	printTree(invertTree(&inode))
}
