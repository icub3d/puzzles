use std::str::FromStr;

struct Point {
    q: i32,
    r: i32,
    s: i32,
}

enum Direction {
    N,
    NE,
    SE,
    S,
    SW,
    NW,
}

impl FromStr for Direction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "n" => Ok(Direction::N),
            "ne" => Ok(Direction::NE),
            "se" => Ok(Direction::SE),
            "s" => Ok(Direction::S),
            "sw" => Ok(Direction::SW),
            "nw" => Ok(Direction::NW),
            _ => Err(()),
        }
    }
}

impl Point {
    fn new(q: i32, r: i32, s: i32) -> Point {
        assert_eq!(q + r + s, 0);
        Point { q, r, s }
    }

    fn move_direction(&self, direction: Direction) -> Point {
        match direction {
            Direction::N => Point::new(self.q, self.r + 1, self.s - 1),
            Direction::NE => Point::new(self.q + 1, self.r, self.s - 1),
            Direction::SE => Point::new(self.q + 1, self.r - 1, self.s),
            Direction::S => Point::new(self.q, self.r - 1, self.s + 1),
            Direction::SW => Point::new(self.q - 1, self.r, self.s + 1),
            Direction::NW => Point::new(self.q - 1, self.r + 1, self.s),
        }
    }

    fn distance(&self, other: &Point) -> i32 {
        let Point {
            q: q1,
            r: r1,
            s: s1,
        } = self;
        let Point {
            q: q2,
            r: r2,
            s: s2,
        } = other;
        (*q1 - *q2)
            .abs()
            .max((*r1 - *r2).abs())
            .max((*s1 - *s2).abs())
    }
}

fn main() {
    let input = include_str!("../input");
    let directions: Vec<Direction> = input
        .trim()
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect();
    let mut point = Point::new(0, 0, 0);
    let mut max_distance = 0;

    for direction in directions {
        point = point.move_direction(direction);
        max_distance = max_distance.max(point.distance(&Point::new(0, 0, 0)));
    }

    println!("p1: {}", point.distance(&Point::new(0, 0, 0)));
    println!("p2: {}", max_distance);
}
