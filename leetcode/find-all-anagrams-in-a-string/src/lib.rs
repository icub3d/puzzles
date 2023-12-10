pub struct Solution;

impl Solution {
    pub fn find_anagrams(s: String, p: String) -> Vec<i32> {
        let s = s.chars().collect::<Vec<_>>();
        let mut l = 0;
        let pp = p.chars().fold(vec![0; 26], |mut v, w| {
            v[w as usize - 'a' as usize] += 1;
            v
        });
        let mut ss = vec![0; 26];
        let mut aa = vec![];

        for r in 0..s.len() {
            ss[s[r] as usize - 'a' as usize] += 1;

            if ss.iter().sum::<i32>() < pp.iter().sum::<i32>() {
                continue;
            }
            while ss.iter().sum::<i32>() > pp.iter().sum::<i32>() {
                ss[s[l] as usize - 'a' as usize] -= 1;
                l += 1;
            }
            if ss == pp {
                aa.push(l as i32);
            }
        }
        aa
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::find_anagrams("cbaebabacd".into(), "abc".into()),
            vec![0, 6]
        );
        assert_eq!(
            Solution::find_anagrams("abab".into(), "ab".into()),
            vec![0, 1, 2]
        );
    }
}
