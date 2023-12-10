use std::{
    collections::HashMap,
    fmt, fs,
    ops::{Add, AddAssign},
};

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
struct Point {
    x: isize,
    y: isize,
}

impl Point {
    fn new(s: &str) -> Point {
        let p = s
            .split(',')
            .map(|p| p.parse::<isize>().unwrap())
            .collect::<Vec<isize>>();
        Point { x: p[0], y: p[1] }
    }

    fn delta(&self, other: &Point) -> Point {
        match self.x == other.x {
            true => match self.y > other.y {
                true => Point { x: 0, y: -1 },
                false => Point { x: 0, y: 1 },
            },
            false => match self.x > other.x {
                true => Point { x: -1, y: 0 },
                false => Point { x: 1, y: 0 },
            },
        }
    }
}

impl AddAssign<&Point> for Point {
    fn add_assign(&mut self, other: &Point) {
        *self = Self {
            x: self.x + other.x,
            y: self.y + other.y,
        };
    }
}

impl Add for Point {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

struct Grid {
    grid: HashMap<Point, char>,
    min: Point,
    max: Point,
    floor: isize,
}

impl Grid {
    fn new(s: &str) -> Grid {
        let mut g = Grid {
            grid: HashMap::new(),
            min: Point {
                x: isize::MAX,
                y: isize::MAX,
            },
            max: Point {
                x: isize::MIN,
                y: isize::MIN,
            },
            floor: 0,
        };

        for line in s.lines() {
            let points = line.split(" -> ").collect::<Vec<&str>>();
            let mut prev = Point::new(points[0]);
            g.grid.insert(prev.clone(), '#');

            for i in 1..points.len() {
                let cur = Point::new(points[i]);
                let delta = prev.delta(&cur);
                g.update_min_max(&prev);

                while prev != cur {
                    prev += &delta;
                    g.grid.insert(prev.clone(), '#');
                    g.update_min_max(&prev);
                }
                g.grid.insert(prev.clone(), '#');
            }
        }
        g.min.y = 0;
        g.floor = g.max.y + 2;
        g
    }

    fn update_min_max(&mut self, p: &Point) {
        if p.x < self.min.x {
            self.min.x = p.x;
        }
        if p.x > self.max.x {
            self.max.x = p.x;
        }
        if p.y > self.max.y {
            self.max.y = p.y;
        }
    }

    fn get(&self, p: &Point, floor: bool) -> Option<&char> {
        if floor && p.y == self.floor {
            Some(&'#')
        } else {
            self.grid.get(&p)
        }
    }

    fn rest(&mut self, p: &mut Point, floor: bool) -> bool {
        if self.grid.get(&p).is_some() {
            return false;
        }
        loop {
            // If we are past the bottom, we are done with p1.
            if !floor && p.y >= self.max.y {
                return false;
            }
            // See if something is below us.
            let mut next = p.clone() + Point { x: 0, y: 1 };
            match self.get(&next, floor) {
                None => {
                    *p = next;
                    continue;
                }
                Some(_) => (),
            };

            // See if we can go to the left.
            next += &Point { x: -1, y: 0 };
            match self.get(&next, floor) {
                None => {
                    *p = next;
                    continue;
                }
                Some(_) => (),
            };

            // See if we can go to the right.
            next += &Point { x: 2, y: 0 };
            match self.get(&next, floor) {
                None => {
                    *p = next;
                    continue;
                }
                Some(_) => {
                    self.grid.insert(p.clone(), 'o');
                    return true;
                }
            };
        }
    }
}

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // write!(f, "{:?} {:?}\n", self.min, self.max)?;
        for y in self.min.y..=self.max.y {
            for x in self.min.x..=self.max.x {
                match self.grid.get(&Point { x, y }) {
                    Some(x) => write!(f, "{x}")?,
                    None => write!(f, ".")?,
                };
            }
            write!(f, "\n")?;
        }
        write!(f, "\n")
    }
}

fn main() {
    let lines = fs::read_to_string("input").unwrap();

    let mut grid = Grid::new(&lines);
    let mut sand = 0;

    loop {
        let mut pos = Point { x: 500, y: 0 };
        match grid.rest(&mut pos, false) {
            true => (),
            false => break,
        }
        sand += 1;
    }

    println!("p1: {sand}");

    let mut grid = Grid::new(&lines);
    let mut sand = 0;

    loop {
        let mut pos = Point { x: 500, y: 0 };
        match grid.rest(&mut pos, true) {
            true => (),
            false => break,
        }
        sand += 1;
    }

    println!("p2: {sand}");
}
