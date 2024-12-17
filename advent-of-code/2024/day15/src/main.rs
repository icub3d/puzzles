use itertools::Itertools;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Instruction {
    Up,
    Down,
    Left,
    Right,
}

impl From<char> for Instruction {
    fn from(c: char) -> Self {
        match c {
            '^' => Instruction::Up,
            'v' => Instruction::Down,
            '<' => Instruction::Left,
            '>' => Instruction::Right,
            _ => unreachable!(),
        }
    }
}

impl Instruction {
    fn apply(&self, x: usize, y: usize) -> (usize, usize) {
        match self {
            Instruction::Up => (x, y - 1),
            Instruction::Down => (x, y + 1),
            Instruction::Left => (x - 1, y),
            Instruction::Right => (x + 1, y),
        }
    }
}

#[derive(Debug)]
struct Map {
    map: Vec<Vec<char>>,
    robot: (usize, usize),
}

impl Map {
    fn new(input: &str) -> Self {
        // For the normal sized map in p1, we simply push values and find the
        // robot.
        let mut map = vec![];
        let mut robot = (0, 0);
        for (y, line) in input.lines().enumerate() {
            map.push(vec![]);
            for (x, c) in line.chars().enumerate() {
                map[y].push(c);
                if c == '@' {
                    robot = (x, y);
                }
            }
        }
        Map { map, robot }
    }

    fn new_wide(input: &str) -> Self {
        // For the wide map in p2, we'll be pushing two characters per cell.
        let mut map = vec![];
        let mut robot = (0, 0);
        for (y, line) in input.lines().enumerate() {
            map.push(vec![]);
            for (x, c) in line.chars().enumerate() {
                if c == 'O' {
                    map[y].push('[');
                    map[y].push(']');
                } else if c == '@' {
                    // Note the x position of the robot is doubled because we
                    // have two characters per cell.
                    map[y].push('@');
                    map[y].push('.');
                    robot = (x * 2, y);
                } else {
                    map[y].push(c);
                    map[y].push(c);
                }
            }
        }
        Map { map, robot }
    }

    fn apply_all(&mut self, instructions: &[Instruction]) {
        instructions.iter().for_each(|instruction| {
            self.apply(instruction);
        });
    }

    fn apply(&mut self, instruction: &Instruction) {
        // Get the new position of the robot. and our next position.
        let (x, y) = self.robot;
        let (new_x, new_y) = instruction.apply(x, y);

        // Do some base case checks.
        if self.map[new_y][new_x] == '#' {
            // We reached a wall.
            return;
        } else if self.map[new_y][new_x] == '.' {
            // we can simply move there.
            self.map[y][x] = '.';
            self.map[new_y][new_x] = '@';
            self.robot = (new_x, new_y);
            return;
        }

        // Try to recursively shift the boxes.
        self.shift(new_x, new_y, instruction);

        // See if the shift worked and we can now move.
        if self.map[new_y][new_x] == '.' {
            self.map[y][x] = '.';
            self.map[new_y][new_x] = '@';
            self.robot = (new_x, new_y);
        }
    }

    fn shift(&mut self, x: usize, y: usize, instruction: &Instruction) {
        // We are at a box now. Let's see if we can move it and all other boxes
        // in that can move with it.
        if let Some(moves) = self.can_move(x, y, instruction) {
            let moves = moves.into_iter().unique().collect::<Vec<_>>();
            for (x, y) in moves {
                let (new_x, new_y) = instruction.apply(x, y);
                (self.map[y][x], self.map[new_y][new_x]) = (self.map[new_y][new_x], self.map[y][x]);
            }
        }
    }

    fn can_move(
        &self,
        x: usize,
        y: usize,
        instruction: &Instruction,
    ) -> Option<Vec<(usize, usize)>> {
        // Get the position of the place I need to move.
        let (new_x, new_y) = instruction.apply(x, y);

        // Check for another part of the box (p2).
        let other = match (self.map[y][x], instruction) {
            ('[', Instruction::Up | Instruction::Down) => Some((x + 1, y)),
            (']', Instruction::Up | Instruction::Down) => Some((x - 1, y)),
            _ => None,
        };
        let other_new = other.map(|(x, y)| instruction.apply(x, y));

        // Do some base case checks.
        match (self.map[new_y][new_x], other, other_new) {
            // Do we hit a wall?
            ('#', _, _) => return None,
            (_, _, Some((x, y))) if self.map[y][x] == '#' => return None,

            // Are we free to move?
            ('.', _, None) => return Some(vec![(x, y)]),
            ('.', Some((other_x, other_y)), Some((other_new_x, other_new_y)))
                if self.map[other_new_y][other_new_x] == '.' =>
            {
                return Some(vec![(x, y), (other_x, other_y)])
            }

            _ => (),
        }

        // At this point, we know we need to move at least one box that's in the way.
        // Let's see if we can move it (recursion bruh).
        let mut all_moves = vec![];
        if self.map[new_y][new_x] != '.' {
            all_moves.extend(self.can_move(new_x, new_y, instruction)?);
        }
        if let Some((other_new_x, other_new_y)) = other_new {
            if self.map[other_new_y][other_new_x] != '.' {
                all_moves.extend(self.can_move(other_new_x, other_new_y, instruction)?);
            }
        }

        // We need to include ourself as well as our partner (if we have one).
        all_moves.push((x, y));
        if let Some((other_x, other_y)) = other {
            all_moves.push((other_x, other_y));
        }
        Some(all_moves)
    }

    fn gps(&self) -> usize {
        self.map
            .iter()
            .enumerate()
            .flat_map(|(y, line)| {
                line.iter().enumerate().map(move |(x, c)| {
                    // For p1, we are looking for 'O', for p2 we are looking for
                    // the left side of the box '['.
                    if *c == '[' || *c == 'O' {
                        y * 100 + x
                    } else {
                        0
                    }
                })
            })
            .sum()
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = include_str!("input.txt");
    let (map, instructions) = input.split_once("\r\n\r\n").unwrap();

    // Parse maps.
    let mut wide_map = Map::new_wide(map);
    let mut map = Map::new(map);

    // Parse instructions.
    let instructions: Vec<Instruction> = instructions
        .chars()
        .filter(|c| !c.is_whitespace())
        .map(Instruction::from)
        .collect();

    // Part 1
    map.apply_all(&instructions);
    println!("p1: {}", map.gps());

    // Part 2
    wide_map.apply_all(&instructions);
    println!("p2: {}", wide_map.gps());

    Ok(())
}
