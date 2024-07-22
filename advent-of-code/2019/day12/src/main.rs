use std::ops::Add;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Point {
    x: isize,
    y: isize,
    z: isize,
}

impl Point {
    fn new(x: isize, y: isize, z: isize) -> Point {
        Point { x, y, z }
    }

    fn energy(&self) -> isize {
        self.x.abs() + self.y.abs() + self.z.abs()
    }
}

impl Add for Point {
    type Output = Point;

    fn add(self, other: Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Moon {
    position: Point,
    velocity: Point,
}

impl Moon {
    fn new(x: isize, y: isize, z: isize) -> Moon {
        Moon {
            position: Point::new(x, y, z),
            velocity: Point::new(0, 0, 0),
        }
    }

    fn gravitational_pull(&self, other: &Moon) -> Point {
        Point {
            x: match self.position.x.cmp(&other.position.x) {
                std::cmp::Ordering::Less => 1,
                std::cmp::Ordering::Equal => 0,
                std::cmp::Ordering::Greater => -1,
            },
            y: match self.position.y.cmp(&other.position.y) {
                std::cmp::Ordering::Less => 1,
                std::cmp::Ordering::Equal => 0,
                std::cmp::Ordering::Greater => -1,
            },
            z: match self.position.z.cmp(&other.position.z) {
                std::cmp::Ordering::Less => 1,
                std::cmp::Ordering::Equal => 0,
                std::cmp::Ordering::Greater => -1,
            },
        }
    }

    fn apply_gravity(&mut self, delta: Point) {
        self.velocity = self.velocity + delta;
    }

    fn apply_velocity(&mut self) {
        self.position = self.position + self.velocity;
    }

    fn energy(&self) -> isize {
        self.position.energy() * self.velocity.energy()
    }
}

fn main() {
    let mut moons = [
        Moon::new(-13, -13, -13),
        Moon::new(5, -8, 3),
        Moon::new(-6, -10, -3),
        Moon::new(0, 5, -5),
    ];

    for _ in 0..1000 {
        let deltas: Vec<Point> = moons
            .iter()
            .map(|moon| {
                moons
                    .iter()
                    .map(|other| moon.gravitational_pull(other))
                    .fold(Point::new(0, 0, 0), |acc, x| acc + x)
            })
            .collect();

        for (moon, delta) in moons.iter_mut().zip(deltas) {
            moon.apply_gravity(delta);
        }

        for moon in moons.iter_mut() {
            moon.apply_velocity();
        }
    }

    let total_energy: isize = moons.iter().map(|moon| moon.energy()).sum();
    println!("p1: {}", total_energy);

    let moons = [
        Moon::new(-13, -13, -13),
        Moon::new(5, -8, 3),
        Moon::new(-6, -10, -3),
        Moon::new(0, 5, -5),
    ];

    let x = find_cylce(&moons.iter().map(|moon| moon.position.x).collect::<Vec<_>>());
    let y = find_cylce(&moons.iter().map(|moon| moon.position.y).collect::<Vec<_>>());
    let z = find_cylce(&moons.iter().map(|moon| moon.position.z).collect::<Vec<_>>());
    println!("p2: {}", lcm3(x as isize, y as isize, z as isize))
}

fn find_cylce(positions: &[isize]) -> usize {
    let mut velocities = vec![0; positions.len()];
    let mut positions = positions.to_vec();
    let initial_positions = positions.to_vec();
    let initial_velocities = velocities.clone();

    let mut steps = 0;
    loop {
        for i in 0..positions.len() {
            for j in 0..positions.len() {
                if i == j {
                    continue;
                }
                match positions[i].cmp(&positions[j]) {
                    std::cmp::Ordering::Less => velocities[i] += 1,
                    std::cmp::Ordering::Equal => (),
                    std::cmp::Ordering::Greater => velocities[i] -= 1,
                }
            }
        }

        for i in 0..positions.len() {
            positions[i] += velocities[i];
        }

        steps += 1;

        if positions == initial_positions && velocities == initial_velocities {
            return steps;
        }
    }
}

fn gcd(mut a: isize, mut b: isize) -> isize {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

fn lcm(a: isize, b: isize) -> isize {
    a * b / gcd(a, b)
}

fn lcm3(a: isize, b: isize, c: isize) -> isize {
    lcm(lcm(a, b), c)
}
