package main

type TreeNode struct {
	Val   int
	Left  *TreeNode
	Right *TreeNode
}

func isValidBST(root *TreeNode) bool {
	ch := make(chan int)
	go func() {
		inOrder(root, ch)
		close(ch)
	}()

	cur, ok := <-ch
	if !ok {
		return true
	}
	for next := range ch {
		if cur >= next {
			return false
		}

		cur = next
	}
	return true
}

func inOrder(root *TreeNode, ch chan int) {
	if root == nil {
		return
	}
	inOrder(root.Left, ch)
	ch <- root.Val
	inOrder(root.Right, ch)
}
