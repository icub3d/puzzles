use std::{ops::Sub, str::FromStr};

use nalgebra::{Matrix6, Matrix6x1, RowVector6};
use num::Num;

// Represents a point in 3D space.
#[derive(Debug, Copy, Clone)]
struct Point<N> {
    x: N,
    y: N,
    z: N,
}

impl<N> From<&str> for Point<N>
where
    N: Num + Copy + std::str::FromStr + std::fmt::Debug,
    <N as FromStr>::Err: std::fmt::Debug,
{
    fn from(input: &str) -> Self {
        // We just split by comma, but we also trim because of extra
        // whitespace in the input.
        let mut split = input.split(", ");
        let x = split.next().unwrap().trim().parse().unwrap();
        let y = split.next().unwrap().trim().parse().unwrap();
        let z = split.next().unwrap().trim().parse().unwrap();
        Self { x, y, z }
    }
}

// Impl sub for points so we can do vector math.
impl<N> Sub for Point<N>
where
    N: Num + Copy,
{
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
        }
    }
}

// Represents a vector in 3D space.
#[derive(Debug, Clone)]
struct Vector<N> {
    position: Point<N>,
    velocity: Point<N>,
}

impl<N> From<&str> for Vector<N>
where
    N: Num + Copy + std::str::FromStr + std::fmt::Debug,
    <N as FromStr>::Err: std::fmt::Debug,
{
    fn from(input: &str) -> Self {
        // We can just split by the @ symbol.
        let mut split = input.split(" @ ");
        let position = split.next().unwrap().into();
        let velocity = split.next().unwrap().into();
        Self { position, velocity }
    }
}

// Our intersect_xy will return this. It's possible they are the same
// line and as such they intersect at all points.
enum Intersection<N> {
    Point(Point<N>),
    All,
}

impl<N> Vector<N>
where
    N: Num + Copy + std::fmt::Debug + num::Zero + std::cmp::PartialOrd,
{
    fn intersection_xy(&self, other: &Vector<N>) -> Option<Intersection<N>> {
        // This is all based off of solving y = mx + b for two
        // lines. We first need to find the lines and then solve for
        // where they intersect.

        // Calculate the slope and intercept for each vector.
        let slope_self = self.velocity.y / self.velocity.x;
        let slope_other = other.velocity.y / other.velocity.x;
        let intercept_self = self.position.y - slope_self * self.position.x;
        let intercept_other = other.position.y - slope_other * other.position.x;

        // If they are the same line, then they always
        // intersect. Otherwise, if the slopes are the same, then they
        // never intersect because they are parallel.
        if slope_self == slope_other && intercept_self == intercept_other {
            return Some(Intersection::All);
        } else if slope_self == slope_other {
            return None;
        }

        // Solve for x and y.
        let x = (intercept_other - intercept_self) / (slope_self - slope_other);
        let y = slope_self * x + intercept_self;
        Some(Intersection::Point(Point { x, y, z: N::zero() }))
    }

    fn in_past_xy(&self, point: &Point<N>) -> bool {
        // We want to check if the point of intersection would be in
        // the past. The simplest way to do this is to figure out the
        // number of steps to get to the point and make sure it's not
        // negative.
        let x = point.x - self.position.x;
        let y = point.y - self.position.y;

        let x = x / self.velocity.x;
        let y = y / self.velocity.y;

        x < N::zero() && y < N::zero()
    }
}

// For the real input:
const MIN: f64 = 200_000_000_000_000.0;
const MAX: f64 = 400_000_000_000_000.0;

// For the test input:
// const MIN: f64 = 7.0;
// const MAX: f64 = 27.0;

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let vectors: Vec<Vector<f64>> = input.lines().map(|l| l.into()).collect();

    let mut count = 0;
    for i in 0..vectors.len() {
        for j in i + 1..vectors.len() {
            // Check to see if it intersects.
            if let Some(point) = vectors[i].intersection_xy(&vectors[j]) {
                match point {
                    // If it's all points, then it would definitely be
                    // in the grid.
                    Intersection::All => {
                        count += 1;
                    }

                    Intersection::Point(point) => {
                        // Otherwise, we need to check we are in the
                        // grid and not in the past.
                        if point.x >= MIN
                            && point.x <= MAX
                            && point.y >= MIN
                            && point.y <= MAX
                            && !vectors[i].in_past_xy(&point)
                            && !vectors[j].in_past_xy(&point)
                        {
                            count += 1;
                        }
                    }
                }
            }
        }
    }
    println!("p1: {}", count);
    p2_nalgrebra(&vectors);

    let vectors: Vec<Vector<i64>> = input.lines().map(|l| l.into()).collect();
    p2_z3(&vectors);
}

fn p2_z3(vectors: &[Vector<i64>]) {
    use z3::ast::{Ast, Int};
    use z3::{Config, Context, SatResult, Solver};

    let now = std::time::Instant::now();

    // Setup our z3 context and solver.
    let cfg = Config::new();
    let ctx = Context::new(&cfg);
    let solver = Solver::new(&ctx);

    // These are the rocks position and velocity for which we are
    // interested in solving.
    let x = Int::new_const(&ctx, "x");
    let y = Int::new_const(&ctx, "y");
    let z = Int::new_const(&ctx, "z");
    let vx = Int::new_const(&ctx, "vx");
    let vy = Int::new_const(&ctx, "vy");
    let vz = Int::new_const(&ctx, "vz");

    // // These are our time variables for all the hailstones.
    // let tt = (0..vectors.len())
    //     .map(|i| Int::new_const(&ctx, format!("t{}", i)))
    //     .take(3)
    //     .collect::<Vec<_>>();

    // Add our hailstone constraints. Again, we should only need 3
    // linearly independent hailstones, which for my input the first 3
    // are.
    for (i, v) in vectors.iter().enumerate().take(3) {
        let t = Int::new_const(&ctx, format!("t{}", i));
        let hx = Int::from_i64(&ctx, v.position.x);
        let hy = Int::from_i64(&ctx, v.position.y);
        let hz = Int::from_i64(&ctx, v.position.z);
        let hvx = Int::from_i64(&ctx, v.velocity.x);
        let hvy = Int::from_i64(&ctx, v.velocity.y);
        let hvz = Int::from_i64(&ctx, v.velocity.z);

        solver.assert(&(&x + &vx * &t)._eq(&(hx + &hvx * &t)));
        solver.assert(&(&y + &vy * &t)._eq(&(hy + &hvy * &t)));
        solver.assert(&(&z + &vz * &t)._eq(&(hz + &hvz * &t)));
    }

    // Check that the solver we created is satisfiable.
    match solver.check() {
        // We expect to get this. For other input sets, there may be
        // no way to solve it.
        SatResult::Sat => {
            // Get the model from the check().
            let model = solver.get_model().unwrap();

            // Use the model to get the get the values of x, y, and z
            // and add them together.
            let solution = model.eval(&(x + y + z), true).unwrap();
            println!("p2-z3:       {:?} ({:?})", solution, now.elapsed());
        }
        _ => {
            println!("p2-z3:       no solution ({:?})", now.elapsed());
        }
    }
}

fn p2_nalgrebra(vectors: &[Vector<f64>]) {
    let now = std::time::Instant::now();

    let p0 = &vectors[0].position;
    let p1 = &vectors[1].position;
    let p2 = &vectors[2].position;
    let v0 = &vectors[0].velocity;
    let v1 = &vectors[1].velocity;
    let v2 = &vectors[2].velocity;

    let b = Matrix6x1::from_row_slice(&[
        ((p0.y as i128 * v0.x as i128 - p1.y as i128 * v1.x as i128)
            - (p0.x as i128 * v0.y as i128 - p1.x as i128 * v1.y as i128)) as f64,
        ((p0.y as i128 * v0.x as i128 - p2.y as i128 * v2.x as i128)
            - (p0.x as i128 * v0.y as i128 - p2.x as i128 * v2.y as i128)) as f64,
        ((p0.z as i128 * v0.x as i128 - p1.z as i128 * v1.x as i128)
            - (p0.x as i128 * v0.z as i128 - p1.x as i128 * v1.z as i128)) as f64,
        ((p0.z as i128 * v0.x as i128 - p2.z as i128 * v2.x as i128)
            - (p0.x as i128 * v0.z as i128 - p2.x as i128 * v2.z as i128)) as f64,
        ((p0.z as i128 * v0.y as i128 - p1.z as i128 * v1.y as i128)
            - (p0.y as i128 * v0.z as i128 - p1.y as i128 * v1.z as i128)) as f64,
        ((p0.z as i128 * v0.y as i128 - p2.z as i128 * v2.y as i128)
            - (p0.y as i128 * v0.z as i128 - p2.y as i128 * v2.z as i128)) as f64,
    ]);

    let a = Matrix6::from_rows(&[
        RowVector6::new(v1.y - v0.y, v0.x - v1.x, 0.0, p0.y - p1.y, p1.x - p0.x, 0.0),
        RowVector6::new(v2.y - v0.y, v0.x - v2.x, 0.0, p0.y - p2.y, p2.x - p0.x, 0.0),
        RowVector6::new(v1.z - v0.z, 0.0, v0.x - v1.x, p0.z - p1.z, 0.0, p1.x - p0.x),
        RowVector6::new(v2.z - v0.z, 0.0, v0.x - v2.x, p0.z - p2.z, 0.0, p2.x - p0.x),
        RowVector6::new(0.0, v1.z - v0.z, v0.y - v1.y, 0.0, p0.z - p1.z, p1.y - p0.y),
        RowVector6::new(0.0, v2.z - v0.z, v0.y - v2.y, 0.0, p0.z - p2.z, p2.y - p0.y),
    ]);

    let r = a.lu().solve(&b).unwrap();
    println!(
        "p2-nalgebra: {:?} ({:?})",
        (r[0] + r[1] + r[2]).round() as i128,
        now.elapsed()
    );
}
