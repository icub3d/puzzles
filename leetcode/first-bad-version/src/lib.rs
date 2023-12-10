pub struct Solution {
    bad: i32,
}

impl Solution {
    pub fn new(bad: i32) -> Self {
        Self { bad }
    }

    pub fn isBadVersion(&self, n: i32) -> bool {
        n >= self.bad
    }

    pub fn first_bad_version(&self, n: i32) -> i32 {
        self.helper(1, n)
    }

    fn helper(&self, left: i32, right: i32) -> i32 {
        let mid = left + (right - left) / 2;
        match self.isBadVersion(mid) {
            true => match self.isBadVersion(mid - 1) {
                false => mid,
                true => self.helper(left, mid - 1),
            },
            false => self.helper(mid + 1, right),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::new(4).first_bad_version(5), 4);
        assert_eq!(Solution::new(1).first_bad_version(1), 1);
        assert_eq!(
            Solution::new(1702766719).first_bad_version(2126753390),
            1702766719
        );
    }
}
