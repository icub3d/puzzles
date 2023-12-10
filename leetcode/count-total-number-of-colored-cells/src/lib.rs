pub struct Solution;

impl Solution {
    pub fn colored_cells(n: i32) -> i64 {
        let mut result = 0;
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::colored_cells(1), 4);
        assert_eq!(Solution::colored_cells(2), 5);
    }
}
