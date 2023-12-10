package main

type TreeNode struct {
	Val   int
	Left  *TreeNode
	Right *TreeNode
}

func lowestCommonAncestor(root, p, q *TreeNode) *TreeNode {
	// If we found one, then it must be the common ancestor because it's below.
	if root.Val == p.Val || root.Val == q.Val {
		return root
	}

	// If they diverge, then this has to be the common ancestor.
	if (root.Val < p.Val && root.Val > q.Val) || (root.Val < q.Val && root.Val > p.Val) {
		return root
	}

	// Otherwise, they are both further down one side or the other.
	if p.Val < root.Val {
		return lowestCommonAncestor(root.Left, p, q)
	}

	return lowestCommonAncestor(root.Right, p, q)

}
