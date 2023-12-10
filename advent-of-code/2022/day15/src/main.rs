use std::{
    collections::HashSet,
    fmt, fs,
    ops::{Add, AddAssign},
};

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref RE: Regex =
        Regex::new(r"Sensor at x=([^,]*), y=([^,]*): closest beacon is at x=([^,]*), y=([^,]*)")
            .unwrap();
}

#[derive(PartialEq, Hash, Eq, Debug, Clone, Copy)]
struct Point {
    x: isize,
    y: isize,
}

impl Point {
    fn new(x: &str, y: &str) -> Self {
        Self {
            x: x.parse::<isize>().unwrap(),
            y: y.parse::<isize>().unwrap(),
        }
    }

    fn manhattan_distance(&self, other: &Point) -> isize {
        (self.x - other.x).abs() + (self.y - other.y).abs()
    }
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl AddAssign for Point {
    fn add_assign(&mut self, other: Self) {
        *self = Self {
            x: self.x + other.x,
            y: self.y + other.y,
        };
    }
}

impl Add<&Point> for Point {
    type Output = Point;
    fn add(self, other: &Point) -> <Self as Add<&Point>>::Output {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct Sensor {
    location: Point,
    nearest_beacon: Point,
    dist: isize,
}

impl Sensor {
    fn new(s: &str) -> Self {
        let caps = RE.captures(s).unwrap();
        Self {
            location: Point::new(&caps[1], &caps[2]),
            nearest_beacon: Point::new(&caps[3], &caps[4]),
            dist: Point::new(&caps[1], &caps[2])
                .manhattan_distance(&Point::new(&caps[3], &caps[4])),
        }
    }
}

fn covered(sensors: &[Sensor], y: isize, min: isize, max: isize) -> usize {
    let mut found: HashSet<isize> = HashSet::new();
    for sensor in sensors {
        // Get manhattan distance.
        let md = sensor.dist;

        // Determine if we'd mark any spots with this sensor.
        if (sensor.location.y < y && sensor.location.y + md >= y)
            || (sensor.location.y > y && sensor.location.y - md <= y)
        {
            // Find out how many marks we'd need for the given location
            let md = ((sensor.location.y - y).abs() - md).abs();
            let loc = sensor.location.x;
            if loc >= min && loc <= max {
                found.insert(loc);
            }

            // Go right
            let mut cur = loc + 1;
            while cur <= loc + md && cur <= max {
                found.insert(cur);
                cur += 1;
            }

            // Go left
            let mut cur = loc + 1;
            while cur >= loc - md && cur >= min {
                found.insert(cur);
                cur -= 1;
            }
        }
    }

    // We need to remove any sensors or beacons from Y.
    for sensor in sensors {
        if sensor.location.y == y {
            found.remove(&sensor.location.x);
        }
        if sensor.nearest_beacon.y == y {
            found.remove(&sensor.nearest_beacon.x);
        }
    }
    found.len()
}

fn main() {
    let lines = fs::read_to_string("input").unwrap();
    let sensors = lines.lines().map(Sensor::new).collect::<Vec<Sensor>>();
    let p1 = covered(&sensors, 2_000_000, isize::MIN, isize::MAX);
    println!("p1: {}", p1);

    let mut p2x = 0;
    let mut p2y = 0;
    for cur in 0..=4_000_000 {
        // Create line segments along the the given y axis similar to how covered worked above.
        let mut segs = vec![];
        for sensor in &sensors {
            let md = sensor.dist - (cur - sensor.location.y).abs();
            if md >= 0 {
                segs.push((
                    0.max(sensor.location.x - md),
                    4_000_000.min(sensor.location.x + md),
                ))
            }
        }
        segs.sort();

        // For each of the line segments, make sure the end of the previous overlaps with the start of the next. If it doesn't, we found the y lines that's not completely covered.
        let mut end = segs[0].1;
        for (next_start, next_end) in segs.iter().skip(1) {
            if *next_start > end {
                p2y = cur;
                p2x = next_start - 1;
                break;
            }
            end = end.max(*next_end);
        }
    }
    println!("p2: {} {} {}", p2x, p2y, p2x * 4_000_000 + p2y);
}
