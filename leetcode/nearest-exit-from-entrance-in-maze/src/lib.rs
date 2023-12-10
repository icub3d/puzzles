pub struct Solution;

impl Solution {
    pub fn nearest_exit(maze: Vec<Vec<char>>, entrance: Vec<i32>) -> i32 {
        let mut maze = maze;
        let mut queue = vec![];
        let mut steps = 0;
        let mut result = 0;
        let mut visited = vec![vec![false; maze[0].len()]; maze.len()];
        let mut directions = vec![vec![0, 1], vec![0, -1], vec![1, 0], vec![-1, 0]];
        queue.push(entrance);
        visited[entrance[0] as usize][entrance[1] as usize] = true;
        while !queue.is_empty() {
            let mut size = queue.len();
            while size > 0 {
                let current = queue.remove(0);
                if current[0] == 0
                    || current[0] == maze.len() as i32 - 1
                    || current[1] == 0
                    || current[1] == maze[0].len() as i32 - 1
                {
                    if current != entrance {
                        return steps;
                    }
                }
                for direction in &directions {
                    let mut next = vec![0; 2];
                    next[0] = current[0] + direction[0];
                    next[1] = current[1] + direction[1];
                    if next[0] >= 0
                        && next[0] < maze.len() as i32
                        && next[1] >= 0
                        && next[1] < maze[0].len() as i32
                        && maze[next[0] as usize][next[1] as usize] == '.'
                        && !visited[next[0] as usize][next[1] as usize]
                    {
                        visited[next[0] as usize][next[1] as usize] = true;
                        queue.push(next);
                    }
                }
                size -= 1;
            }
            steps += 1;
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::nearest_exit(
                vec![
                    vec!['+', '+', '+', '+', '+', '+'],
                    vec!['+', '.', '.', '.', '.', '+'],
                    vec!['+', '.', '+', '+', '.', '+'],
                    vec!['+', '.', '.', '.', '.', '+'],
                    vec!['+', '+', '+', '+', '+', '+']
                ],
                vec![1, 2]
            ),
            1
        );
        assert_eq!(
            Solution::nearest_exit(
                vec![
                    vec!['+', '+', '+', '+', '+', '+'],
                    vec!['+', '.', '.', '.', '.', '+'],
                    vec!['+', '.', '+', '+', '.', '+'],
                    vec!['+', '.', '.', '.', '.', '+'],
                    vec!['+', '+', '+', '+', '+', '+']
                ],
                vec![1, 0]
            ),
            2
        );
    }
}
