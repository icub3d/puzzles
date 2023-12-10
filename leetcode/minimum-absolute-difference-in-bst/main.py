from typing import Optional


# Definition for a binary tree node.
class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right


class Solution:
    def getMinimumDifference(self, root: Optional[TreeNode]) -> int:
        def inorder(root):
            if not root:
                return
            yield from inorder(root.left)
            yield root.val
            yield from inorder(root.right)

        min = float("inf")
        prev = None
        for v in inorder(root):
            if prev is None:
                prev = v
                continue
            if v - prev < min:
                min = v - prev
            prev = v
        return min


def test_getMinimumDifference():
    root = TreeNode(4, TreeNode(2, TreeNode(1), TreeNode(3)), TreeNode(6))
    assert Solution().getMinimumDifference(root) == 1
    root = TreeNode(1, TreeNode(0), TreeNode(48, TreeNode(12), TreeNode(49)))
    assert Solution().getMinimumDifference(root) == 1
