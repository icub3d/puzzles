package main

type ListNode struct {
	Val  int
	Next *ListNode
}

func detectCycle(head *ListNode) *ListNode {
	m := map[*ListNode]bool{}
	for head != nil {
		if _, ok := m[head]; ok {
			return head
		}
		m[head] = true
		head = head.Next
	}
	return nil
}

func main() {

}
