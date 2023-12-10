import unittest

class Solution:
    def lengthOfLongestSubstring(self, s: str) -> int:
        longest = 0
        left = 0
        characters = set()
        for right in range(0, len(s)):
            while left <= right and s[right] in characters:
                characters.remove(s[left])
                left += 1
            characters.add(s[right])
            longest = max(longest, right-left+1)
        return longest

class SolutionTest(unittest.TestCase):
    def test_solution(self):
        solution = Solution()
        self.assertEqual(solution.lengthOfLongestSubstring("abcabcbb"), 3)
        self.assertEqual(solution.lengthOfLongestSubstring("bbbbb"), 1)
        self.assertEqual(solution.lengthOfLongestSubstring("pwwkew"), 3)

if __name__ == "__main__":
    unittest.main()
