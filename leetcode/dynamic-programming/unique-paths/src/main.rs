struct Solution;

use std::collections::HashMap;

impl Solution {
    pub fn unique_paths_tab(m: i32, n: i32) -> i32 {
        let m = m as usize;
        let n = n as usize;

        // We can use bottom up tabulation to solve this problem. We
        // start at the end and work our way back to the beginning.
        let mut dp = vec![vec![0; n]; m];
        for i in (0..m).rev() {
            for j in (0..n).rev() {
                // The last item is one.
                if i == m - 1 && j == n - 1 {
                    dp[i][j] = 1;
                    continue;
                }

                // Otherwise, we can sum up the right and down values
                // if we aren't at the edges..
                let mut result = 0;
                if i < m - 1 {
                    result += dp[i + 1][j];
                }
                if j < n - 1 {
                    result += dp[i][j + 1];
                }
                dp[i][j] = result;
            }
        }

        // Our final solution will be the result at the top left.
        dp[0][0]
    }

    pub fn unique_paths(m: i32, n: i32) -> i32 {
        let mut memo: HashMap<(i32, i32), i32> = HashMap::new();
        memo.insert((1, 1), 1);
        memo.insert((1, 2), 1);
        memo.insert((2, 1), 1);
        memo.insert((2, 2), 2);
        Solution::unique_paths_helper(m, n, &mut memo)
    }

    pub fn unique_paths_helper(m: i32, n: i32, memo: &mut HashMap<(i32, i32), i32>) -> i32 {
        if memo.contains_key(&(m, n)) {
            return memo[&(m, n)];
        } else {
            let mut result = 0;
            if m > 1 {
                result += Solution::unique_paths_helper(m - 1, n, memo);
            }
            if n > 1 {
                result += Solution::unique_paths_helper(m, n - 1, memo);
            }
            memo.insert((m, n), result);
            return result;
        }
    }
}

fn main() {
    assert_eq!(Solution::unique_paths(3, 2), 3);
    assert_eq!(Solution::unique_paths(3, 7), 28);
    assert_eq!(Solution::unique_paths_tab(3, 2), 3);
    assert_eq!(Solution::unique_paths_tab(3, 7), 28);
}
