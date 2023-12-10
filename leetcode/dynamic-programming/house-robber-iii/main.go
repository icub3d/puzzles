package main

import "fmt"

type TreeNode struct {
	Val   int
	Left  *TreeNode
	Right *TreeNode
}

type cache struct {
	root          *TreeNode
	parent_robbed bool
}

func rob(root *TreeNode) int {
	return rob_helper(make(map[cache]int), root, false)
}

func rob_helper(memo map[cache]int, root *TreeNode, parent_robbed bool) int {
	if root == nil {
		return 0
	} else if val, ok := memo[cache{root, parent_robbed}]; ok {
		return val
	}

	var val int
	if parent_robbed {
		val = rob_helper(memo, root.Left, false) + rob_helper(memo, root.Right, false)
	} else {
		val = max(rob_helper(memo, root.Left, true)+rob_helper(memo, root.Right, true)+root.Val, rob_helper(memo, root.Left, false)+rob_helper(memo, root.Right, false))
	}
	memo[cache{root, parent_robbed}] = val
	return val
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func main() {
	r := &TreeNode{
		Val: 3,
		Left: &TreeNode{
			Val:  2,
			Left: nil,
			Right: &TreeNode{
				Val: 3,
			},
		},
		Right: &TreeNode{
			Val:  3,
			Left: nil,
			Right: &TreeNode{
				Val: 1,
			},
		},
	}
	fmt.Println(rob(r))
	r = &TreeNode{
		Val: 3,
		Left: &TreeNode{
			Val: 4,
			Left: &TreeNode{
				Val: 1,
			},
			Right: &TreeNode{
				Val: 3,
			},
		},
		Right: &TreeNode{
			Val:  5,
			Left: nil,
			Right: &TreeNode{
				Val: 1,
			},
		},
	}
	fmt.Println(rob(r))
}
