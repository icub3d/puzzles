import unittest 

class Solution:
    def containsNearbyDuplicate(self, nums: list[int], k: int) -> bool:
        if len(nums) < 2 or k < 1: return False
        left = 0
        current_window = {nums[0]}
        for right in range(1, len(nums)):
            if nums[right] in current_window:
                return True
            current_window.add(nums[right])
            if len(current_window) > k:
                current_window.remove(nums[left])
                left += 1

        return False
    

class TestSolution(unittest.TestCase):
    def test_solution(self):
        solution = Solution()
        self.assertEqual(solution.containsNearbyDuplicate([1,0,1,1], 1), True)
        self.assertEqual(solution.containsNearbyDuplicate([1,2,3,1], 0), False)
        self.assertEqual(solution.containsNearbyDuplicate([1,2,3,1], 3), True)
        self.assertEqual(solution.containsNearbyDuplicate([1,2,3,1,2,3], 2), False)

if __name__ == '__main__':
    unittest.main()
