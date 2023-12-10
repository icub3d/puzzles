pub struct Solution;

pub enum Slot {
    Idle,
    Busy(char),
}

impl Solution {
    pub fn least_interval(tasks: Vec<char>, n: i32) -> i32 {
        let mut ff = vec![0; 26];
        for task in tasks.iter() {
            ff[*task as usize - 'A' as usize] += 1;
        }
        ff.sort();

        let max = ff.pop().unwrap();
        let mut idle = (max - 1) * n;
        while ff.len() > 0 && idle > 0 {
            idle -= std::cmp::min(max - 1, ff.pop().unwrap());
        }
        tasks.len() as i32 + idle
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::least_interval(vec!['A', 'A', 'A', 'B', 'B', 'B'], 2),
            8
        );
        assert_eq!(
            Solution::least_interval(vec!['A', 'A', 'A', 'B', 'B', 'B'], 0),
            6
        );
        assert_eq!(
            Solution::least_interval(
                vec!['A', 'A', 'A', 'A', 'A', 'A', 'B', 'C', 'D', 'E', 'F', 'G'],
                2
            ),
            16
        );
    }
}
