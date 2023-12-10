import unittest
from collections import Counter

class Solution:
    def characterReplacement(self, s: str, k: int) -> int:
        chars_in_window = Counter()
        left = 0
        most_frequent = 0
        longest = 0 
        for right in range(len(s)):
            cur = s[right]
            chars_in_window.update(cur)
            most_frequent = max(most_frequent, chars_in_window[cur])
            while right - left + 1 - most_frequent > k:
                chars_in_window.subtract(s[left])
                left += 1

            longest = max(longest, right-left+1)
        return longest

class TestSolution(unittest.TestCase):
    def test_solution(self):
        solution = Solution()
        self.assertEqual(solution.characterReplacement("ABAB", 2), 4)
        self.assertEqual(solution.characterReplacement("AABABBA", 1), 4)

if __name__ == '__main__':
    unittest.main()
