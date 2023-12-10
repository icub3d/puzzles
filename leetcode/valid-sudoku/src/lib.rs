pub struct Solution;

impl Solution {
    pub fn is_valid_sudoku(board: Vec<Vec<char>>) -> bool {
        let boxes = vec![
            (0, 0),
            (0, 3),
            (0, 6),
            (3, 0),
            (3, 3),
            (3, 6),
            (6, 0),
            (6, 3),
            (6, 6),
        ];
        for i in 0..9 {
            if !Solution::valid(&board[i].iter().filter(|v| **v != '.').cloned().collect()) {
                return false;
            } else if !Solution::valid(&board.iter().map(|v| v[i]).filter(|v| *v != '.').collect())
            {
                return false;
            }
            let mut vv = vec!['.'; 9];
            for x in 0..3 {
                for y in 0..3 {
                    vv[x * 3 + y] = board[boxes[i].0 + x][boxes[i].1 + y];
                }
            }
            if !Solution::valid(&vv.iter().filter(|v| **v != '.').cloned().collect()) {
                return false;
            }
        }
        true
    }

    pub fn valid(nn: &Vec<char>) -> bool {
        dbg!(nn);
        use std::collections::HashSet;
        let mut s = HashSet::new();
        for n in nn {
            match s.insert(n) {
                false => return false,
                _ => (),
            };
        }
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::is_valid_sudoku(vec![
                vec!['5', '3', '.', '.', '7', '.', '.', '.', '.'],
                vec!['6', '.', '.', '1', '9', '5', '.', '.', '.'],
                vec!['.', '9', '8', '.', '.', '.', '.', '6', '.'],
                vec!['8', '.', '.', '.', '6', '.', '.', '.', '3'],
                vec!['4', '.', '.', '8', '.', '3', '.', '.', '1'],
                vec!['7', '.', '.', '.', '2', '.', '.', '.', '6'],
                vec!['.', '6', '.', '.', '.', '.', '2', '8', '.'],
                vec!['.', '.', '.', '4', '1', '9', '.', '.', '5'],
                vec!['.', '.', '.', '.', '8', '.', '.', '7', '9']
            ]),
            true
        );
        assert_eq!(
            Solution::is_valid_sudoku(vec![
                vec!['8', '3', '.', '.', '7', '.', '.', '.', '.'],
                vec!['6', '.', '.', '1', '9', '5', '.', '.', '.'],
                vec!['.', '9', '8', '.', '.', '.', '.', '6', '.'],
                vec!['8', '.', '.', '.', '6', '.', '.', '.', '3'],
                vec!['4', '.', '.', '8', '.', '3', '.', '.', '1'],
                vec!['7', '.', '.', '.', '2', '.', '.', '.', '6'],
                vec!['.', '6', '.', '.', '.', '.', '2', '8', '.'],
                vec!['.', '.', '.', '4', '1', '9', '.', '.', '5'],
                vec!['.', '.', '.', '.', '8', '.', '.', '7', '9'],
            ]),
            false
        );
    }
}
