pub struct Solution;

impl Solution {
    pub fn angle_clock(hour: i32, minutes: i32) -> f64 {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::angle_clock(12, 30), 165.0);
        assert_eq!(Solution::angle_clock(3, 30), 75.0);
        assert_eq!(Solution::angle_clock(3, 15), 7.5);
        assert_eq!(Solution::angle_clock(4, 50), 155.0);
        assert_eq!(Solution::angle_clock(12, 0), 0.0);
    }
}
