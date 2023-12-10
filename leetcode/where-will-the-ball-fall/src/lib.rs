enum BallPosition {
    Above,
    Below,
}

pub struct Solution;

impl Solution {
    pub fn find_ball(grid: Vec<Vec<i32>>) -> Vec<i32> {
        let max_x = grid[0].len() - 1;
        let max_y = grid.len() - 1;
        let mut solution = vec![];
        for mut x in 0..=max_x {
            let mut pos = BallPosition::Above;
            let mut y = 0;
            loop {
                match (pos, grid[y][x]) {
                    // Ball above the line and we are moving right and we can move right.
                    (BallPosition::Above, 1) if x < max_x => match grid[y][x + 1] {
                        1 => {
                            pos = BallPosition::Below;
                            x += 1;
                        }
                        _ => {
                            solution.push(-1);
                            break;
                        }
                    },
                    // Ball is above the line and we are moving left and we can move left.
                    (BallPosition::Above, -1) if x > 0 => match grid[y][x - 1] {
                        1 => {
                            solution.push(-1);
                            break;
                        }
                        _ => {
                            pos = BallPosition::Below;
                            x -= 1;
                        }
                    },
                    // If we are above but the two before didn't catch it, then we are stuck.
                    (BallPosition::Above, _) => {
                        solution.push(-1);
                        break;
                    }
                    // If we are below the line and we've hit the bottom, we found the solution.
                    (BallPosition::Below, _) if y == max_y => {
                        solution.push(x as i32);
                        break;
                    }
                    // Otherwise, move down and put ourselves above the line.
                    _ => {
                        y += 1;
                        pos = BallPosition::Above;
                    }
                }
            }
        }
        solution
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::find_ball(vec![
                vec![1, 1, 1, -1, -1],
                vec![1, 1, 1, -1, -1],
                vec![-1, -1, -1, 1, 1],
                vec![1, 1, 1, 1, -1],
                vec![-1, -1, -1, -1, -1]
            ]),
            vec![1, -1, -1, -1, -1]
        );
        assert_eq!(Solution::find_ball(vec![vec![-1]]), vec![-1]);
        assert_eq!(
            Solution::find_ball(vec![
                vec![1, 1, 1, 1, 1, 1],
                vec![-1, -1, -1, -1, -1, -1],
                vec![1, 1, 1, 1, 1, 1],
                vec![-1, -1, -1, -1, -1, -1]
            ]),
            vec![0, 1, 2, 3, 4, -1]
        );
    }
}
