use std::{
    collections::{HashMap, HashSet},
    fs,
    ops::{AddAssign, SubAssign},
};

#[derive(Eq, PartialEq, Copy, PartialOrd, Ord, Clone, Hash, Debug)]
struct Point {
    x: isize,
    y: isize,
}

impl Point {
    fn intersects(&self, s: &HashSet<Point>) -> bool {
        // (0, 8) are x bounds, and 0 is y floor.
        if self.x <= 0 || self.x >= 8 || self.y <= 0 {
            return true;
        }
        s.contains(self)
    }
}

impl AddAssign<&Point> for Point {
    fn add_assign(&mut self, rhs: &Point) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}

impl SubAssign<&Point> for Point {
    fn sub_assign(&mut self, rhs: &Point) {
        self.x -= rhs.x;
        self.y -= rhs.y;
    }
}

#[derive(Clone)]
struct Block {
    points: Vec<Point>,
}

impl Block {
    fn new(points: Vec<Point>) -> Self {
        Self { points }
    }

    fn intersects(&self, s: &HashSet<Point>) -> bool {
        for p in &self.points {
            if p.intersects(s) {
                return true;
            }
        }
        false
    }
}

impl AddAssign<&Point> for Block {
    fn add_assign(&mut self, rhs: &Point) {
        for p in self.points.iter_mut() {
            *p += rhs;
        }
    }
}

impl SubAssign<&Point> for Block {
    fn sub_assign(&mut self, rhs: &Point) {
        for p in self.points.iter_mut() {
            *p -= rhs;
        }
    }
}

struct Game {
    board: HashSet<Point>,
    seen: HashMap<(usize, usize, Vec<Point>), (usize, isize)>,
    blocks: Vec<Block>,
    movements: Vec<char>,
    added: usize,
    mm: usize,
    iteration: usize,
}

impl Game {
    fn new(blocks: Vec<Block>, movements: String) -> Self {
        Self {
            board: HashSet::new(),
            seen: HashMap::new(),
            blocks,
            movements: movements.chars().collect(),
            added: 0,
            mm: 0,
            iteration: 0,
        }
    }

    fn max_y(&self) -> isize {
        self.board.iter().map(|p| p.y).max().unwrap_or(0)
    }

    fn next_block(&self) -> Block {
        self.blocks[self.iteration % self.blocks.len()].clone()
    }

    fn next_movement(&mut self) -> Point {
        if self.mm >= self.movements.len() {
            self.mm = 0;
        }
        let p = match self.movements[self.mm] {
            '<' => Point { x: -1, y: 0 },
            _ => Point { x: 1, y: 0 },
        };
        self.mm += 1;
        p
    }

    // Get top 30 and see if we can find a match.
    fn top(&self) -> Vec<Point> {
        let y = self.max_y();
        let mut top: Vec<Point> = self
            .board
            .iter()
            // This was largely arbitrary. I started at 10 and it didn't work.
            .filter(|p| y - p.y <= 30)
            .map(|p| Point { x: p.x, y: y - p.y })
            .collect();
        top.sort();
        top
    }

    fn iterate(&mut self) {
        let down = Point { x: 0, y: -1 };
        let pos = Point {
            x: 3,
            y: self.max_y() + 4,
        };
        let mut block = self.next_block();
        block += &pos;
        loop {
            // Move left or right. If we can't move back.
            let movement = self.next_movement();
            block += &movement;
            if block.intersects(&self.board) {
                block -= &movement;
            }

            // Move down.
            block += &down;
            if block.intersects(&self.board) {
                // We can't move down, so go back and add this block to the board.
                block -= &down;
                for p in block.points {
                    self.board.insert(p);
                }

                // Do the work for part 2. We maintain a map of game states we've seen.
                let cur = (self.iteration % 5, self.mm, self.top());
                // If we have seen this game state before and we've gotten past part 1, we should update the amount we added to the p2 total.
                if self.seen.contains_key(&cur) && self.iteration >= 2022 {
                    // Get the previous seen values.
                    let (old_iteration, old_y) = *self.seen.get(&cur).unwrap();

                    // We want to increment our iterations and the number of lines added by figuring out how many times we'd see this in the rest of the iterations.
                    let dy = self.max_y() - old_y;
                    let di = self.iteration - old_iteration;
                    let a = (1_000_000_000_000 - self.iteration) / di;
                    self.added += a * dy as usize;
                    self.iteration += a * di;
                }
                self.seen.insert(cur, (self.iteration, self.max_y()));

                self.iteration += 1;
                return;
            }
        }
    }
}

fn main() {
    let horizontal = Block::new(vec![
        Point { x: 0, y: 0 },
        Point { x: 1, y: 0 },
        Point { x: 2, y: 0 },
        Point { x: 3, y: 0 },
    ]);
    let cross = Block::new(vec![
        Point { x: 1, y: 0 },
        Point { x: 0, y: 1 },
        Point { x: 1, y: 1 },
        Point { x: 2, y: 1 },
        Point { x: 1, y: 2 },
    ]);
    let l = Block::new(vec![
        Point { x: 2, y: 2 },
        Point { x: 2, y: 1 },
        Point { x: 0, y: 0 },
        Point { x: 1, y: 0 },
        Point { x: 2, y: 0 },
    ]);
    let vertical = Block::new(vec![
        Point { x: 0, y: 0 },
        Point { x: 0, y: 1 },
        Point { x: 0, y: 2 },
        Point { x: 0, y: 3 },
    ]);
    let square = Block::new(vec![
        Point { x: 0, y: 0 },
        Point { x: 1, y: 0 },
        Point { x: 0, y: 1 },
        Point { x: 1, y: 1 },
    ]);
    let rotation: Vec<Block> = vec![horizontal, cross, l, vertical, square];
    let lines = fs::read_to_string("input").unwrap();

    let mut game = Game::new(rotation, lines);

    // For part 2 we'll adjust iterations based on if we find cycles.
    while game.iteration < 1_000_000_000_000 {
        game.iterate();

        // Iterations 2022 is for part 1.
        if game.iteration == 2022 {
            // The top would be the largest X.
            println!("p1: {}", game.max_y());
        }
    }
    println!("p2: {}", game.max_y() as usize + game.added);
}
