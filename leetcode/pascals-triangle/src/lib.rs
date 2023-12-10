pub struct Solution;

impl Solution {
    pub fn generate(num_rows: i32) -> Vec<Vec<i32>> {
        let num_rows = num_rows as usize;
        let mut s = vec![vec![1]];
        let mut cur = vec![1];

        for j in 1..num_rows {
            let mut next = vec![1; j + 1];
            for i in 1..cur.len() {
                next[i] = cur[i - 1] + cur[i];
            }
            s.push(next.clone());
            cur = next;
        }

        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::generate(5),
            vec![
                vec![1],
                vec![1, 1],
                vec![1, 2, 1],
                vec![1, 3, 3, 1],
                vec![1, 4, 6, 4, 1]
            ]
        );
        assert_eq!(Solution::generate(1), vec![vec![1]]);
    }
}
