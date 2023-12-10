package main

type Node struct {
	Val      int
	Children []*Node
}

func preorder(root *Node) []int {
	output := []int{}
	preorderHelper(root, &output)
	return output
}

func preorderHelper(root *Node, output *[]int) {
	if root == nil {
		return
	}
	*output = append(*output, root.Val)
	for _, child := range root.Children {
		preorderHelper(child, output)
	}
}
