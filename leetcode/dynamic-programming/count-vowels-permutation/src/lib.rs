pub struct Solution;

impl Solution {
    pub fn count_vowel_permutation(n: i32) -> i32 {
        // The dynamic programming intuition is that we can calculate
        // the numbers of strings of length n by knowing the numbers
        // of strings of length n-1. Thus if we start at length 1, we
        // can build up to length n.
        //
        // For each vowel, we just need to know which vowels could
        // come before it. For example, If our current letter is 'a',
        // then it could have been preceded by 'e', 'i', or 'u'. So
        // the count for 'a' is the sum of those letters counts.

        let mut cur = vec![1; 5];
        let mut next = vec![0; 5];
        let m = 1_000_000_007_usize;
        for _ in 1..n as usize {
            // a could come after e, i, u
            next[0] = (cur[1] + cur[2] + cur[4]) % m;

            // e could come after a, i
            next[1] = (cur[0] + cur[2]) % m;

            // i could come after e, o
            next[2] = (cur[1] + cur[3]) % m;

            // o could come after i
            next[3] = cur[2];

            // u could come after i, o
            next[4] = (cur[2] + cur[3]) % m;

            // swap cur and next
            cur = next;
            next = vec![0; 5];
        }
        cur.iter().fold(0, |acc, x| (acc + x) % m) as i32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::count_vowel_permutation(1), 5);
        assert_eq!(Solution::count_vowel_permutation(2), 10);
        assert_eq!(Solution::count_vowel_permutation(5), 68);
        assert_eq!(Solution::count_vowel_permutation(144), 18208803);
    }
}
