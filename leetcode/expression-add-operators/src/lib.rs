pub struct Solution;

impl Solution {
    pub fn add_operators(num: String, target: i32) -> Vec<String> {
		let mut result = vec![];
		let mut path = vec![];
		let mut num = num.chars().collect::<Vec<char>>();
		Self::dfs(&mut result, &mut path, &mut num, target, 0, 0, 0);
		result
    }

	fn dfs(
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
		assert_eq!(
			Solution::add_operators("123".to_string(), 6),
			vec!["1+2+3", "1*2*3"]
		);
		assert_eq!(
			Solution::add_operators("232".to_string(), 8),
			vec!["2*3+2", "2+3*2"]
		);
		assert_eq!(
			Solution::add_operators("105".to_string(), 5),
			vec!["1*0+5", "10-5"]
		);
		assert_eq!(
			Solution::add_operators("00".to_string(), 0),
			vec!["0+0", "0-0", "0*0"]
		);
		assert_eq!(
			Solution::add_operators("3456237490".to_string(), 9191),
			vec![]
		);
    }
}
