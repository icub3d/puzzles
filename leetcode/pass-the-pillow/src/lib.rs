pub struct Solution;

impl Solution {
    pub fn pass_the_pillow(n: i32, time: i32) -> i32 {
        let mut result = 0;
        let mut current = 0;
        let mut time = time;
        while time > 0 {
            current += 1;
            if current == n {
                current = 0;
            }
            time -= 1;
        }
        current
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::pass_the_pillow(3, 2), 3);
        assert_eq!(Solution::pass_the_pillow(4, 5), 2);
    }
}
