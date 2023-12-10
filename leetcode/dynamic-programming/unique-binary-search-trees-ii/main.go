package main

import (
	"fmt"
)

type TreeNode struct {
	Val   int
	Left  *TreeNode
	Right *TreeNode
}

func copyTree(t *TreeNode, i int) *TreeNode {
	if t == nil {
		return nil
	}

	return &TreeNode{
		Val:   t.Val + i,
		Left:  copyTree(t.Left, i),
		Right: copyTree(t.Right, i),
	}
}

func generateTrees(n int) []*TreeNode {
	// We won't get this from the input program but we will on the
	// left/right side when we hit edges.
	if n == 0 {
		return []*TreeNode{nil}
	}

	// We are basically doing the same thing as version one, but we are
	// going to get back the actual list of trees on each side and
	// then do the cartesean joins.
	trees := []*TreeNode{}
	for i := 1; i <= n; i++ {
		left := generateTrees(i - 1)
		right := generateTrees(n - i)
		for _, l := range left {
			for _, r := range right {
				trees = append(trees,
					&TreeNode{
						Val:   i,
						Left:  copyTree(l, 0),
						Right: copyTree(r, i),
					},
				)
			}
		}
	}

	return trees
}

func printTrees(tt []*TreeNode) {
	for _, t := range tt {
		printTree(t)
		fmt.Println()
	}
}

func printTree(t *TreeNode) {
	if t == nil {
		fmt.Print("null ")
		return
	}
	fmt.Print(t.Val, " ")
	printTree(t.Left)
	printTree(t.Right)
}

func main() {
	printTrees(generateTrees(3))
	printTrees(generateTrees(1))
}
