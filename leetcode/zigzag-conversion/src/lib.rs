pub struct Solution;

impl Solution {
    // 2 // 0
    // 2, 0 -> 0 2 4 6 8 10 12
    // 0, 2 -> 1 3 5 7 9 11 13

    // 3 // 1
    // 4, 0 -> 0 4 8 12
    // 2, 2 -> 1 3 5 7 9 11 13
    // 0, 4 -> 2 6 10

    // 4 // 2
    // 6, 0 -> 0 6 12
    // 4, 2 -> 1 5 7 11 13
    // 2, 4 -> 2 4 8 10
    // 0, 6 -> 3 9

    // 5 // 3
    // 8, 0 -> 0 8
    // 6, 2 -> 1 7 9
    // 4, 4 -> 2 6 10
    // 2, 6 -> 3 5 11 13
    // 0, 8 -> 4 12

    // There appears to be a pattern where the distance from the
    // initial item to the next alternates. The total distance
    // between the current and the next is n + n-2. There are
    // actually two values. The seconds starting at zero. After
    // we've collected the firrst row, we subtract 2 from the
    // first value and add to to the next. We then alternate
    // between the two.
    pub fn convert(s: String, num_rows: i32) -> String {
        let n = num_rows as usize;
        if n == 1 {
            return s;
        }
        let cc = s.chars().collect::<Vec<char>>();

        let mut r = vec![];
        let mut j = [2 * n - 2, 0];

        for i in 0..n {
            let mut k = i;
            let mut it = j.iter().cycle();
            while k < cc.len() {
                r.push(cc[k]);
                k += match it.next().unwrap() {
                    0 => it.next().unwrap(),
                    n => n,
                };
            }
            if i == n - 1 {
                break;
            }
            j[0] -= 2;
            j[1] += 2;
        }

        r.iter().collect::<String>()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::convert("PAYPALISHIRING".to_string(), 3),
            "PAHNAPLSIIGYIR"
        );
        assert_eq!(
            Solution::convert("PAYPALISHIRING".to_string(), 4),
            "PINALSIGYAHRPI"
        );
    }
}
