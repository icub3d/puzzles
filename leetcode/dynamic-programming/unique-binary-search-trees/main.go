package main

// The key insight here is that for each value in [1, n], all numbers
// less will be on the left hand side and all numbers greater will be
// on the right hand side. We can simply calculate the number of
// sub-trees for each side recursively and then multiply them to get
// this values count. We multiply because it's the cartesean product
// of both sides. That is, if there are 3 ways to do the left and 2
// ways to do the right, we'll have 6 ways to do them in total.
func numTrees(n int) int {
	if n == 0 {
		return 1
	}
	var res int
	for i := 1; i <= n; i++ {
		res += numTrees(i-1) * numTrees(n-i)
	}
	return res
}

func main() {
	println(numTrees(3))
	println(numTrees(4))
	println(numTrees(1))
}
