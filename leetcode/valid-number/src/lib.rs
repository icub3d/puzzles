pub struct Solution;
pub enum State {
    Begin,
    Sign,
    DotNoBefore,
    BeforeDot,
    Dot,
    AfterDot,
    E,
    ESign,
    AfterE,
}
impl Solution {
    pub fn is_number(s: String) -> bool {
        let mut state = State::Begin;
        for c in s.chars() {
            match state {
                State::Begin => match c {
                    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                        state = State::BeforeDot
                    }
                    '.' => state = State::DotNoBefore,
                    '+' | '-' => state = State::Sign,
                    _ => return false,
                },
                State::Sign => match c {
                    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                        state = State::BeforeDot
                    }
                    '.' => state = State::DotNoBefore,
                    _ => return false,
                },
                State::DotNoBefore => match c {
                    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                        state = State::AfterDot
                    }
                    _ => return false,
                },
                State::BeforeDot => match c {
                    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                        state = State::BeforeDot
                    }
                    '.' => state = State::Dot,
                    'e' | 'E' => state = State::E,
                    _ => return false,
                },
                State::Dot | State::AfterDot => match c {
                    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                        state = State::AfterDot
                    }
                    'e' | 'E' => state = State::E,
                    _ => return false,
                },
                State::E => match c {
                    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                        state = State::AfterE
                    }
                    '+' | '-' => state = State::ESign,
                    _ => return false,
                },
                State::ESign | State::AfterE => match c {
                    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                        state = State::AfterE
                    }
                    _ => return false,
                },
            }
        }
        matches!(
            state,
            State::BeforeDot | State::Dot | State::AfterDot | State::AfterE
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::is_number("0".into()), true);
        assert_eq!(Solution::is_number("e".into()), false);
        assert_eq!(Solution::is_number(".".into()), false);

        assert_eq!(Solution::is_number("2".into()), true);
        assert_eq!(Solution::is_number("0089".into()), true);
        assert_eq!(Solution::is_number("-0.1".into()), true);
        assert_eq!(Solution::is_number("+3.14".into()), true);
        assert_eq!(Solution::is_number("4.".into()), true);
        assert_eq!(Solution::is_number("-.9".into()), true);
        assert_eq!(Solution::is_number("2e10".into()), true);
        assert_eq!(Solution::is_number("-90E3".into()), true);
        assert_eq!(Solution::is_number("3e+7".into()), true);
        assert_eq!(Solution::is_number("+6e-1".into()), true);
        assert_eq!(Solution::is_number("53.5e93".into()), true);
        assert_eq!(Solution::is_number("-123.456e789".into()), true);

        assert_eq!(Solution::is_number("abc".into()), false);
        assert_eq!(Solution::is_number("1a".into()), false);
        assert_eq!(Solution::is_number("1e".into()), false);
        assert_eq!(Solution::is_number("e3".into()), false);
        assert_eq!(Solution::is_number("99e2.5".into()), false);
        assert_eq!(Solution::is_number("--6".into()), false);
        assert_eq!(Solution::is_number("-+3".into()), false);
        assert_eq!(Solution::is_number("95a54e53".into()), false);
    }
}
