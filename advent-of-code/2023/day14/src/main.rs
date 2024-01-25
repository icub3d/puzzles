use std::{collections::HashSet, hash::Hasher};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Grid {
    grid: Vec<Vec<char>>,
}

impl Grid {
    fn parse(input: &str) -> Self {
        let grid = input
            .lines()
            .map(|line| line.chars().collect::<Vec<_>>())
            .collect::<Vec<_>>();
        Self { grid }
    }

    #[allow(dead_code)]
    fn print(&self) {
        for row in &self.grid {
            println!("{}", row.iter().collect::<String>());
        }
    }

    fn total_load(&self) -> usize {
        // Calculate the total load of the grid. Basically we are going
        // through the grid and looking for 'O'. It's load it the number
        // of rows minus the row it's on.
        self.grid
            .iter()
            .enumerate()
            .map(|(y, row)| {
                row.iter()
                    .enumerate()
                    .map(|(_, c)| if *c == 'O' { self.grid.len() - y } else { 0 })
                    .sum::<usize>()
            })
            .sum()
    }

    fn roll_north_faster(&mut self) {
        // See p1_faster for an explanation of this.
        for x in 0..self.grid[0].len() {
            let mut next = 0;
            for y in 0..self.grid.len() {
                match self.grid[y][x] {
                    'O' => {
                        (self.grid[next][x], self.grid[y][x]) =
                            (self.grid[y][x], self.grid[next][x]);
                        next += 1;
                        while next < y && self.grid[next][x] != '.' {
                            next += 1;
                        }
                    }
                    '#' => next = y + 1,
                    _ => {}
                }
            }
        }
    }

    fn roll_south_faster(&mut self) {
        // See p1_faster for an explanation of this.
        for x in 0..self.grid[0].len() {
            let mut next = self.grid.len() - 1;
            for y in (0..self.grid.len()).rev() {
                match self.grid[y][x] {
                    'O' => {
                        (self.grid[next][x], self.grid[y][x]) =
                            (self.grid[y][x], self.grid[next][x]);
                        next = next.saturating_sub(1);
                        while next > y && self.grid[next][x] != '.' {
                            next = next.saturating_sub(1);
                        }
                    }
                    '#' => next = y.saturating_sub(1),
                    _ => {}
                }
            }
        }
    }

    fn roll_west_faster(&mut self) {
        // See p1_faster for an explanation of this.
        for y in 0..self.grid.len() {
            let mut next = 0;
            for x in 0..self.grid[0].len() {
                match self.grid[y][x] {
                    'O' => {
                        (self.grid[y][next], self.grid[y][x]) =
                            (self.grid[y][x], self.grid[y][next]);
                        next += 1;
                        while next < x && self.grid[y][next] != '.' {
                            next += 1;
                        }
                    }
                    '#' => next = x + 1,
                    _ => {}
                }
            }
        }
    }

    fn roll_east_faster(&mut self) {
        // See p1_faster for an explanation of this.
        for y in 0..self.grid.len() {
            let mut next = self.grid[0].len() - 1;
            for x in (0..self.grid[0].len()).rev() {
                match self.grid[y][x] {
                    'O' => {
                        (self.grid[y][next], self.grid[y][x]) =
                            (self.grid[y][x], self.grid[y][next]);
                        next = next.saturating_sub(1);
                        while next > x && self.grid[y][next] != '.' {
                            next = next.saturating_sub(1);
                        }
                    }
                    '#' => next = x.saturating_sub(1),
                    _ => {}
                }
            }
        }
    }

    fn roll_all_faster(&mut self) {
        self.roll_north_faster();
        self.roll_west_faster();
        self.roll_south_faster();
        self.roll_east_faster();
    }

    fn roll_north(&mut self, x: usize, y: usize) {
        // Note use of rev() here because we want to start at our
        // current positing and work backwards. We start at 1 because
        // we are checking the rock above us and we don't want to go
        // out of bounds.
        for y in (1..=y).rev() {
            // We want to stop if we hit another rock. Otherwise, we
            // can move our rock up.
            match self.grid[y - 1][x] {
                '#' | 'O' => break,
                _ => {
                    self.grid[y - 1][x] = 'O';
                    self.grid[y][x] = '.';
                }
            }
        }
    }

    fn roll_east(&mut self, x: usize, y: usize) {
        // Same as roll_north, but we are moving right now so our
        // bounds and direction change.
        for x in x..self.grid[y].len() - 1 {
            match self.grid[y][x + 1] {
                '#' | 'O' => break,
                _ => {
                    self.grid[y][x + 1] = 'O';
                    self.grid[y][x] = '.';
                }
            }
        }
    }

    fn roll_south(&mut self, x: usize, y: usize) {
        // Same ...
        for y in y..self.grid.len() - 1 {
            match self.grid[y + 1][x] {
                '#' | 'O' => break,
                _ => {
                    self.grid[y + 1][x] = 'O';
                    self.grid[y][x] = '.';
                }
            }
        }
    }

    fn roll_west(&mut self, x: usize, y: usize) {
        // Same ... note rev again because we are moving backwards.
        for x in (1..=x).rev() {
            match self.grid[y][x - 1] {
                '#' | 'O' => break,
                _ => {
                    self.grid[y][x - 1] = 'O';
                    self.grid[y][x] = '.';
                }
            }
        }
    }

    fn roll_all(&mut self) {
        // North and west need to scan from top left to bottom right
        // moving forward.
        for f in &[Self::roll_north, Self::roll_west] {
            for y in 0..self.grid.len() {
                for x in 0..self.grid[y].len() {
                    if self.grid[y][x] == 'O' {
                        f(self, x, y);
                    }
                }
            }
        }

        // For south and east, we need to scan from bottom right to
        // top left moving backwards.
        for f in &[Self::roll_south, Self::roll_east] {
            for y in (0..self.grid.len()).rev() {
                for x in (0..self.grid[y].len()).rev() {
                    if self.grid[y][x] == 'O' {
                        f(self, x, y);
                    }
                }
            }
        }
    }
}

// We'll use this to track the grids we've seen and the step we saw it
// in.
#[derive(Debug, Eq)]
struct State {
    step: usize,
    grid: Grid,
}

impl State {
    fn new(step: usize, grid: Grid) -> Self {
        Self { step, grid }
    }
}

// For PartialEq and Hash, we only care about the grid. The grid is
// the "node" in our cycle detection. The step is the position we saw
// it in last so we can get a cycle count.
impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.grid == other.grid
    }
}

impl std::hash::Hash for State {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.grid.hash(state);
    }
}

fn p2<F>(grid: &mut Grid, roll_fn: F) -> usize
where
    F: Fn(&mut Grid),
{
    // We want to track grid state and see if we've seen this state
    // before. If we have, we are in a cycle.
    let mut seen = HashSet::new();

    // We shouldn't need to go all the steps, because we hope to have
    // found a cycle.
    for i in 0..1_000_000_000 {
        // Add our current state.
        seen.insert(State::new(i, grid.clone()));

        // perform the rolls.
        roll_fn(grid);

        // See if we've seen this state before.
        if let Some(state) = seen.get(&State::new(0, grid.to_owned())) {
            // If we have, we are in a cycle. We can calculate the
            // cycle length and then fast forward to the end.
            let cycle_len = i + 1 - state.step;
            let remaining = 1_000_000_000 - i - 1;
            let remaining = remaining % cycle_len;

            // Remaining is the number of steps we need to take to
            // from where we are at to get to the same position that
            // 1_000_000_000 steps would have taken us.
            for _ in 0..remaining {
                roll_fn(grid);
            }
            return grid.total_load();
        }
    }
    panic!("why you wait so long?");
}

fn p1_roll_north(grid: &mut [Vec<char>], x: usize, y: usize) -> usize {
    // We'll track our y position because it will be used to calculate
    // the load of this rounded rock.
    let mut pos = y;

    // We want to start at the current position and roll north until
    // we get to the top or we run into another rock.
    for y in (1..=y).rev() {
        match grid[y - 1][x] {
            '#' | 'O' => break, // stop if we hit another rock.
            _ => {
                // Otherwise, we want to move it up and decrement our
                // position.
                grid[y - 1][x] = 'O';
                grid[y][x] = '.';
                pos -= 1;
            }
        }
    }

    // The load for this rock is the number of rows minus the y
    // position.
    grid.len() - pos
}

fn p1(grid: &mut [Vec<char>]) -> usize {
    // We can track the total as we go.
    let mut total = 0;

    // We just need to loop through the entire grid and find all the
    // rounded rocks and roll them north.
    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            // If we find a rounded rock, roll it north and add the
            // total.
            if grid[y][x] == 'O' {
                total += p1_roll_north(grid, x, y);
            }
        }
    }
    total
}

fn p1_faster(grid: &mut [Vec<char>]) -> usize {
    // We do a sort of tortoise and hare thing here. The tortoise will
    // be the next position to where we could roll a rock. The hare
    // will be looking for a rock to roll. When we find a rock, we
    // swap and then have the tortoise move to the next valid
    // position.
    let mut total = 0;
    for x in 0..grid[0].len() {
        let mut next = 0;
        for y in 0..grid.len() {
            match grid[y][x] {
                'O' => {
                    // We can just swap the values here. If next == y,
                    // this just happens to be a nop.
                    (grid[next][x], grid[y][x]) = (grid[y][x], grid[next][x]);
                    // Increment our total.
                    total += grid.len() - next;

                    // Move next to the next valid position (possibly y).
                    next += 1;
                    while next < y && grid[next][x] != '.' {
                        next += 1;
                    }
                }

                // If we find a cube-shaped rock, we "reset" the
                // tortoise to after it.
                '#' => next = y + 1,
                _ => {}
            }
        }
    }
    total
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let grid = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let mut g1 = grid.clone();
    let now = std::time::Instant::now();
    let load = p1(&mut g1);
    println!("p1:        {} ({:?})", load, now.elapsed());

    let mut g2 = grid.clone();
    let now = std::time::Instant::now();
    let load = p1_faster(&mut g2);
    println!("p1-faster: {} ({:?})", load, now.elapsed());

    let mut g2 = Grid::parse(&input);
    let now = std::time::Instant::now();
    let load = p2(&mut g2, Grid::roll_all);
    println!("p2:        {} ({:?})", load, now.elapsed());

    let mut g2 = Grid::parse(&input);
    let now = std::time::Instant::now();
    let load = p2(&mut g2, Grid::roll_all_faster);
    println!("p2-faster: {} ({:?})", load, now.elapsed());
}
