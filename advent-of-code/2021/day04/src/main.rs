use std::fs;

#[derive(Debug, Clone)]
struct Board {
    grid: Vec<(usize, bool)>,
}

const WINNING_BOARDS: [[usize; 5]; 12] = [
    [0, 1, 2, 3, 4],
    [5, 6, 7, 8, 9],
    [10, 11, 12, 13, 14],
    [15, 16, 17, 18, 19],
    [20, 21, 22, 23, 24],
    [0, 5, 10, 15, 20],
    [1, 6, 11, 16, 21],
    [2, 7, 12, 17, 22],
    [3, 8, 13, 18, 23],
    [4, 9, 14, 19, 24],
    [1, 6, 12, 18, 24],
    [4, 8, 12, 16, 20],
];

impl Board {
    fn new(lines: &[&str]) -> Board {
        let mut grid = Vec::new();
        lines.iter().for_each(|line| {
            line.split(' ')
                .filter(|part| !part.is_empty())
                .map(|n| n.parse::<usize>().unwrap())
                .for_each(|i| grid.push((i, false)));
        });
        Board { grid }
    }

    fn find(&mut self, n: usize) {
        for (i, v) in self.grid.iter().enumerate() {
            if n == v.0 {
                self.grid[i].1 = true;
                return;
            }
        }
    }

    fn win(&self) -> bool {
        for winning_board in WINNING_BOARDS {
            let mut found = true;
            for i in winning_board.iter() {
                if !self.grid[*i].1 {
                    found = false;
                    break;
                }
            }
            if found {
                return true;
            }
        }
        false
    }

    fn score(&self, n: usize) -> usize {
        let mut sum = 0;
        for (i, found) in self.grid.iter() {
            if !*found {
                sum += i
            }
        }
        sum * n
    }
}

fn main() {
    let lines = fs::read_to_string("input").unwrap();
    let lines: Vec<&str> = lines.lines().collect();
    let numbers: Vec<usize> = lines
        .get(0)
        .unwrap()
        .split(',')
        .map(|x| x.parse::<usize>().unwrap())
        .collect();
    let mut boards = vec![];
    lines.iter().skip(2).fold(vec![], |mut acc, line| {
        if line.is_empty() {
            boards.push(Board::new(&acc));
            return vec![];
        }
        acc.push(line);
        acc
    });

    let mut bb: Vec<Board> = boards.to_vec();
    'winning: for n in numbers.clone() {
        for board in bb.iter_mut() {
            board.find(n);
            if board.win() {
                println!("winning board: {}", board.score(n));
                break 'winning;
            }
        }
    }
    let mut bb: Vec<Board> = boards.to_vec();
    'losing: for n in numbers {
        for board in bb.iter_mut() {
            board.find(n);
        }
        if bb.len() > 1 {
            bb = bb.iter().filter(|b| !b.win()).cloned().collect();
        }
        if bb.len() == 1 && bb[0].win() {
            println!("losing board: {}", bb[0].score(n));
            break 'losing;
        }
    }
}
