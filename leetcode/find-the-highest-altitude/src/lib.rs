pub struct Solution;

impl Solution {
    pub fn largest_altitude(gain: Vec<i32>) -> i32 {
        let mut current_altitude = 0;
        let mut max_altitude = 0;
        for i in gain {
            current_altitude += i;
            if current_altitude > max_altitude {
                max_altitude = current_altitude;
            }
        }
        max_altitude
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::largest_altitude(vec![-5, 1, 5, 0, -7]), 1);
        assert_eq!(Solution::largest_altitude(vec![-4, -3, -2, -1, 4, 3, 2]), 0);
    }
}
