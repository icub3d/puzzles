use std::{
    collections::{HashMap, HashSet, VecDeque},
    ops::Add,
};

// Standard point in the garden.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Point {
    x: i32,
    y: i32,
}

// We'll do some arithmatic on points to calculate next valid points.
impl Add for Point {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl Point {
    fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }

    // Find all neighbors of this point. We'll filter it later.
    fn neighbors(&self) -> Vec<Self> {
        vec![
            Self::new(self.x - 1, self.y),
            Self::new(self.x + 1, self.y),
            Self::new(self.x, self.y - 1),
            Self::new(self.x, self.y + 1),
        ]
    }
}

struct Garden {
    rocks: HashSet<Point>,
    start: Point,
    min_x: i32,
    min_y: i32,
    max_x: i32,
    max_y: i32,
}
impl Garden {
    fn new(input: &str) -> Self {
        let max_y = input.lines().count() as i32;
        let max_x = input.lines().next().unwrap().len() as i32;
        let min_x = 0;
        let min_y = 0;

        // Go through the input and look for rocks and the start.
        let mut start = Point::new(0, 0);
        let mut rocks = HashSet::new();
        for (y, line) in input.lines().enumerate() {
            for (x, c) in line.chars().enumerate() {
                let p = Point::new(x as i32, y as i32);
                if c == 'S' {
                    start = p;
                } else if c == '#' {
                    rocks.insert(p);
                }
            }
        }

        Self {
            rocks,
            start,
            min_x,
            min_y,
            max_x,
            max_y,
        }
    }

    // Print is a helper function I used a couple of places to pring
    // out the garden.
    #[allow(dead_code)]
    fn print(&self, points: &HashSet<Point>) {
        for y in self.min_y..self.max_y {
            for x in self.min_x..self.max_x {
                let p = Point::new(x, y);
                if self.rocks.contains(&p) {
                    print!("#");
                } else if points.contains(&p) {
                    print!("O");
                } else if self.start == p {
                    print!("S");
                } else {
                    print!(".");
                }
            }
            println!();
        }
    }

    // Find all valid neighbors of this point. Valid here means it's
    // in the garden and not a rock.
    fn neighbors(&self, p: &Point) -> Vec<Point> {
        p.neighbors()
            .into_iter()
            .filter(|p| {
                p.x >= self.min_x
                    && p.x < self.max_x
                    && p.y >= self.min_y
                    && p.y < self.max_y
                    && !self.rocks.contains(p)
            })
            .collect()
    }

    #[allow(dead_code)]
    fn naive(&self, steps: i32) -> HashSet<Point> {
        // We just use a helper function to recursively find all
        // points.
        let mut points = HashSet::new();
        self.naive_helper(&mut points, steps, &self.start);
        points
    }

    #[allow(dead_code)]
    fn naive_helper(&self, points: &mut HashSet<Point>, steps: i32, cur: &Point) {
        if steps == 0 {
            return;
        }
        for neighbor in self.neighbors(cur) {
            if steps == 1 {
                points.insert(neighbor);
            }
            self.naive_helper(points, steps - 1, &neighbor);
        }
    }

    fn alternating(&self, steps: i32) -> HashSet<Point> {
        let mut cur = HashSet::new();
        cur.insert(self.start);

        let mut next = HashSet::new();

        for _ in 0..steps {
            for p in cur.iter() {
                for d in self.neighbors(p) {
                    next.insert(d);
                }
            }
            std::mem::swap(&mut cur, &mut next);
            next.clear();
        }
        cur
    }

    #[allow(dead_code)]
    fn expand(&self) -> Self {
        let mut new_rocks = HashSet::new();
        for p in &self.rocks {
            new_rocks.insert(*p);
            new_rocks.insert(p.add(Point::new(0, self.max_y)));
            new_rocks.insert(p.add(Point::new(0, -self.max_y)));
            new_rocks.insert(p.add(Point::new(self.max_x, 0)));
            new_rocks.insert(p.add(Point::new(-self.max_x, 0)));
            new_rocks.insert(p.add(Point::new(self.max_x, self.max_y)));
            new_rocks.insert(p.add(Point::new(-self.max_x, self.max_y)));
            new_rocks.insert(p.add(Point::new(self.max_x, -self.max_y)));
            new_rocks.insert(p.add(Point::new(-self.max_x, -self.max_y)));
        }

        Self {
            rocks: new_rocks,
            start: self.start,
            min_x: self.min_x - self.max_x,
            min_y: self.min_y - self.max_y,
            max_x: self.max_x + self.max_x,
            max_y: self.max_y + self.max_y,
        }
    }

    fn calculate_distances(&self) -> HashMap<Point, i32> {
        let mut distances = HashMap::new();
        let mut frontier = VecDeque::new();
        frontier.push_back((self.start, 0));

        while let Some((p, dist)) = frontier.pop_front() {
            if distances.contains_key(&p) {
                continue;
            }

            distances.insert(p, dist);

            for neighbor in self.neighbors(&p) {
                frontier.push_back((neighbor, dist + 1));
            }
        }

        distances
    }
}

fn main() {
    // Build the garden.
    let input = std::fs::read_to_string("input").unwrap();
    let garden = Garden::new(&input);

    // Naive approach - I used this to print out the garden a few
    // times to see what it would look like in the test example.
    // for i in 0..10 {
    //     let points = garden.naive(i);
    //     garden.print(&points);
    // }

    // Alternating approach - track previous states to generate new
    // states.
    let now = std::time::Instant::now();
    let p1 = garden.alternating(64);
    println!("p1: {} ({:?})", p1.len(), now.elapsed());

    // Expand the garden - I used this during the analysis of part 2.
    // let bigger_garden = garden.expand();
    // let bigger_points = bigger_garden.alternating(65 * 3 + 1);
    // bigger_garden.print(&bigger_points);

    // Calculate the distances from each point to the start.
    let now = std::time::Instant::now();
    let distances = garden.calculate_distances();

    // In our final expanded garden, we'll have some odd and even
    // completely filled in blocks. We'll also have some edges of odds
    // we'll need to exclude and some edges of evens we'll need to
    // include. Start by getting a count of all those.
    let (odd, even, odd_edges, even_edges) = distances.iter().fold(
        (0_usize, 0_usize, 0_usize, 0_usize),
        |(odd, even, odd_edges, even_edges), (_, v)| {
            if *v % 2 == 1 && *v > 65 {
                (odd + 1, even, odd_edges + 1, even_edges)
            } else if *v % 2 == 1 {
                (odd + 1, even, odd_edges, even_edges)
            } else if *v % 2 == 0 && *v > 65 {
                (odd, even + 1, odd_edges, even_edges + 1)
            } else {
                (odd, even + 1, odd_edges, even_edges)
            }
        },
    );

    // Where do we get this value?
    // number of steps =  26501365
    // garden size = 131
    //   26501365 / 131 = 202300.496183 -- didn't work.
    //   26501365 % 131 = 65 -- it is a multiple, try ignoring center.
    //   (26501365 - 65) / 131 = 202300
    let count = 202300; // Magic number from analysis above.

    // The total odd and even are squares of count + 1 and count
    // respectively.
    let total_odd = odd * (count + 1) * (count + 1);
    let total_even = even * (count * count);

    // The odd edges is count + 1 and the even edges is count.
    let total_odd_edges = odd_edges * (count + 1);
    let total_even_edges = count * even_edges;

    let p2 = total_odd + total_even - total_odd_edges + total_even_edges;

    println!("p2: {} ({:?})", p2, now.elapsed());
}

// My Process to get to the solutions.
//
// - Naive definitely not going to work, 4^64 checks (i think).
//
// - Use it though to print out some grids to do some pattern
// analysis.
//
// - Try odd/even, where we are tracking odd and even to help
// reduce size, still not enough.
//
// - Try dijkstra on each non-rock node. That does work and gives
// me answer to p1 (3574). Takes several seconds
// though. Obviously, not going to work for p2.
//
// - Go back to odd even. Alternating *does* work, just not how I
// implemented it with odd/even. Instead, track previous values to
// generate new values, then swap, cleanup, and repeat.
//
// - Visualize p1 solution, it's a diamond shape.
//
// - Try one level of expansion and see what it looks like. Another
// diamond.
// - 65         = 3574
// - 65 * 2     = 14648
// - 65 * 3 + 1 = 33190
//
// - Look at some numbers:
//   26501365 (number of steps)
//   3574 (number of points for 64)
//   2719 (number of points for 65, hitting edge with diamond)
//   131 grid size
//   26501365 / 131 = 202300.496183
//   26501365 % 131 = 65
//   (26501365 - 65) / 131 = 202300
//
// - What are those numbers telling me? Excluding the center grid,
// we'll expand out 202300 times. There must be some smart math here.
//
// - The bigger_rocks grid could be thought of as a really zoomed out
// version. Check numbers for lengths of the diamond. Pythagorean
// theorem for the length of the diagonals (286095). We know we'll
// have that many of each diagnol. Then we'll one of each of the
// corners of the diamond. Finally, how many will we have inside?
//
// - This won't actually work because the grids are going to alternate
// between odd/even, LOL!
//
// - Maybe we figure out how many odd/even internal grids and then
// calculate the corners and edges based on whether they'd be
// odd/even.
