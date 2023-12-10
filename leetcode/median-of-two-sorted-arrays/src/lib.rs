pub struct Solution;

impl Solution {
    pub fn find_median_sorted_arrays(nums1: Vec<i32>, nums2: Vec<i32>) -> f64 {
        let total = nums1.len() + nums2.len();
        let mid = total / 2;

        // If we only have one between the two, we have our answer.
        if total == 1 && nums1.len() == 1 {
            return nums1[0] as f64;
        } else if total == 1 && nums2.len() == 1 {
            return nums2[0] as f64;
        }

        // Find the mid point of the two arrays.
        let mut i = 0;
        let mut j = 0;
        let mut prev = 0;
        let mut cur = 0;
        while i + j <= mid {
            dbg!(i, j, mid);
            prev = cur;
            if i < nums1.len() && j < nums2.len() {
                if nums1[i] < nums2[j] {
                    cur = nums1[i];
                    i += 1;
                } else {
                    cur = nums2[j];
                    j += 1;
                }
            } else if i < nums1.len() {
                cur = nums1[i];
                i += 1;
            } else {
                cur = nums2[j];
                j += 1;
            }
        }
        if total % 2 == 0 {
            (prev + cur) as f64 / 2.0
        } else {
            cur as f64
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::find_median_sorted_arrays(vec![2], vec![]), 2.0);
        assert_eq!(
            Solution::find_median_sorted_arrays(vec![1, 3], vec![2]),
            2.0
        );
        assert_eq!(
            Solution::find_median_sorted_arrays(vec![1, 2], vec![3, 4]),
            2.5
        );
        assert_eq!(
            Solution::find_median_sorted_arrays(vec![1, 2, 3], vec![4, 5, 6]),
            3.5
        );
        assert_eq!(
            Solution::find_median_sorted_arrays(vec![1], vec![2, 3, 4, 5, 6]),
            3.5
        );
    }
}
