pub struct Solution;

pub enum Direction {
    Right,
    Down,
    Left,
    Up,
}

impl Solution {
    pub fn spiral_order(matrix: Vec<Vec<i32>>) -> Vec<i32> {
        let mut output = vec![];
        let (mut x, mut y) = (0, 0);
        let mut direction = Direction::Right;
        let (mut end_x, mut end_y) = (matrix[0].len() - 1, 0);
        loop {
            output.push(matrix[y][x]);
            // I was trying to calculate the end point originally, but realized that
            // I'm actually done, if my output len has all the elements.
            if output.len() == matrix[0].len() * matrix.len() {
                break;
            }
            if (x, y) == (end_x, end_y) {
                // We've reached the end, so we need to change direction.
                direction = match direction {
                    Direction::Right => Direction::Down,
                    Direction::Down => Direction::Left,
                    Direction::Left => Direction::Up,
                    Direction::Up => Direction::Right,
                };

                // Determine new direction (this took a bit of looking at various sizes).
                end_x = match direction {
                    Direction::Right => matrix[0].len() - y - 1,
                    Direction::Down => x,
                    Direction::Left => matrix.len() - y - 1,
                    Direction::Up => x,
                };
                end_y = match direction {
                    Direction::Right => y,
                    Direction::Down => matrix.len() - (matrix[0].len() - x),
                    Direction::Left => y,
                    Direction::Up => x + 1,
                };
            }

            x = match direction {
                Direction::Right => x + 1,
                Direction::Down => x,
                Direction::Left => x - 1,
                Direction::Up => x,
            };
            y = match direction {
                Direction::Right => y,
                Direction::Down => y + 1,
                Direction::Left => y,
                Direction::Up => y - 1,
            };
        }
        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::spiral_order(vec![
                vec![1, 2, 3, 4],
                vec![5, 6, 7, 8],
                vec![9, 10, 11, 12],
                vec![13, 14, 15, 16]
            ]),
            vec![1, 2, 3, 4, 8, 12, 16, 15, 14, 13, 9, 5, 6, 7, 11, 10]
        );
        assert_eq!(
            Solution::spiral_order(vec![vec![2, 5, 8], vec![4, 0, -1]]),
            vec![2, 5, 8, -1, 0, 4]
        );
        assert_eq!(
            Solution::spiral_order(vec![vec![2, 5], vec![8, 4], vec![0, -1]]),
            vec![2, 5, 4, -1, 0, 8]
        );
        assert_eq!(
            Solution::spiral_order(vec![vec![1, 2], vec![3, 4]]),
            vec![1, 2, 4, 3]
        );
        assert_eq!(
            Solution::spiral_order(vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]]),
            vec![1, 2, 3, 6, 9, 8, 7, 4, 5]
        );
        assert_eq!(
            Solution::spiral_order(vec![
                vec![1, 2, 3, 4],
                vec![5, 6, 7, 8],
                vec![9, 10, 11, 12]
            ]),
            vec![1, 2, 3, 4, 8, 12, 11, 10, 9, 5, 6, 7]
        );
        assert_eq!(Solution::spiral_order(vec![vec![1]]), vec![1]);
        assert_eq!(Solution::spiral_order(vec![vec![6, 9, 7]]), vec![6, 9, 7]);
        assert_eq!(
            Solution::spiral_order(vec![vec![6], vec![9], vec![7]]),
            vec![6, 9, 7]
        );
    }
}
