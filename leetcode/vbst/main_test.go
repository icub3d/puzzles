package main

import "testing"

func TestIsValidBST(t *testing.T) {
	tree := &TreeNode{
		Val: 5,
		Left: &TreeNode{
			Val: 1,
		},
		Right: &TreeNode{
			Val: 4,
			Left: &TreeNode{
				Val: 3,
			},
			Right: &TreeNode{
				Val: 6,
			},
		},
	}
	if !isValidBST(tree) {
		t.Errorf("isValidBST(t) = false; want true")
	}
}
