import unittest

class Solution:
    def subarraysWithKDistinct(self, nums: list[int], k: int) -> int:
        # The question asks for the exact number. So we use the sliding window
        # to find sub arrays with at most k distinct and then subtract it from 
        # the count of k-1 to get the count of exact. This is simpler (though not 
        # most performance) and illustrates the sliding window solution.
        return self.countAtMostK(nums, k) - self.countAtMostK(nums, k-1)

    def countAtMostK(self, nums: list[int], k: int) -> int:
        count = 0 
        integers = {}
        left = 0 
        for right in range(0, len(nums)):
            num = nums[right]
            integers[num] = integers.get(num, 0) + 1
            while left <= right and len(integers) > k:
                num_left = nums[left]
                integers[num_left] -= 1
                if integers[num_left] == 0:
                    del integers[num_left]
                left += 1
            count += right-left+1
        return count



class TestSolution(unittest.TestCase):

    def test_solution(self):
        solution = Solution()
        self.assertEqual(solution.subarraysWithKDistinct([1,2,1,2,3], 2), 7)
        self.assertEqual(solution.subarraysWithKDistinct([1,2,1,3,4], 3), 3)

if __name__ == "__main__":
    unittest.main()
