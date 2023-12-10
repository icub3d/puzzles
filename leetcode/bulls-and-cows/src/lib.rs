pub struct Solution;

impl Solution {
    pub fn get_hint(secret: String, guess: String) -> String {
        use std::collections::HashMap;

        let mut bulls = 0;
        let mut secret_left = HashMap::new();
        let mut guess_left = HashMap::new();

        for (s, g) in secret.chars().zip(guess.chars()) {
            // If it's an exact match, we just increase the bull count.
            if s == g {
                bulls += 1;
                continue;
            }

            secret_left.entry(s).and_modify(|v| *v += 1).or_insert(1);
            guess_left.entry(g).and_modify(|v| *v += 1).or_insert(1);
        }

        let cows: usize = guess_left
            .iter()
            .map(|(k, v)| match secret_left.get(k) {
                Some(s) => std::cmp::min(*s, *v),
                _ => 0,
            })
            .sum();

        format!("{}A{}B", bulls, cows)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::get_hint("1807".into(), "7810".into()),
            "1A3B".to_string()
        );
        assert_eq!(
            Solution::get_hint("1123".into(), "0111".into()),
            "1A1B".to_string()
        );
    }
}
