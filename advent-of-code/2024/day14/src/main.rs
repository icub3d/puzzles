use nom::{
    bytes::complete::tag,
    character::complete::{char, digit1},
    combinator::{map, opt},
    multi::separated_list1,
    sequence::tuple,
    IResult,
};

#[derive(Debug, Copy, Clone, PartialEq)]
struct Point {
    x: isize,
    y: isize,
}

// Parse a signed integer
fn parse_signed_integer(input: &str) -> IResult<&str, isize> {
    map(
        tuple((
            opt(char::<&str, nom::error::Error<&str>>('-')), // Optional minus sign
            digit1,                                          // One or more digits
        )),
        |(sign, digits)| {
            let num: isize = digits.parse().unwrap();

            // Apply sign if negative
            match sign {
                Some(_) => -num,
                None => num,
            }
        },
    )(input)
}

impl Point {
    fn new(x: isize, y: isize) -> Point {
        Point { x, y }
    }

    fn parse(input: &str) -> IResult<&str, Point> {
        let (input, x) = parse_signed_integer(input)?;
        let (input, _) = char(',')(input)?;
        let (input, y) = parse_signed_integer(input)?;
        Ok((input, Point::new(x, y)))
    }
}

#[derive(Debug, Copy, Clone)]
struct Robot {
    position: Point,
    velocity: Point,
}

impl Robot {
    fn new(position: Point, velocity: Point) -> Robot {
        Robot { position, velocity }
    }

    fn parse(input: &str) -> IResult<&str, Robot> {
        let (input, _) = tag("p=")(input)?;
        let (input, position) = Point::parse(input)?;
        let (input, _) = tag(" v=")(input)?;
        let (input, velocity) = Point::parse(input)?;
        Ok((input, Robot::new(position, velocity)))
    }

    fn parse_all(input: &str) -> IResult<&str, Vec<Robot>> {
        let (input, robots) = separated_list1(tag("\r\n"), Robot::parse)(input)?;
        Ok((input, robots))
    }

    fn step_n(&mut self, n: isize, max_x: isize, max_y: isize) {
        self.position.x = ((self.position.x + self.velocity.x * n) % max_x + max_x) % max_x;
        self.position.y = ((self.position.y + self.velocity.y * n) % max_y + max_y) % max_y;
    }

    fn quadrant(&self, max_x: isize, max_y: isize) -> Option<usize> {
        let x = self.position.x;
        let y = self.position.y;
        if x < max_x / 2 && y < max_y / 2 {
            Some(0)
        } else if x > max_x / 2 && y < max_y / 2 {
            Some(1)
        } else if x < max_x / 2 && y > max_y / 2 {
            Some(2)
        } else if x > max_x / 2 && y > max_y / 2 {
            Some(3)
        } else {
            None
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // let input = include_str!("test.txt");
    // static MAX_X: isize = 11;
    // static MAX_Y: isize = 7;
    let input = include_str!("input.txt");
    static MAX_X: isize = 101;
    static MAX_Y: isize = 103;
    let (_, robots) = Robot::parse_all(input)?;

    // For part 1, we simply simulate 100 steps for each robot and then count
    // how many robots are in each quadrant and multiply the counts.
    let now = std::time::Instant::now();
    let p1 = robots
        .iter()
        .cloned()
        .filter_map(|mut robot| {
            robot.step_n(100, MAX_X, MAX_Y);
            robot.quadrant(MAX_X, MAX_Y)
        })
        .fold([0; 4], |mut acc, x| {
            acc[x] += 1;
            acc
        })
        .iter()
        .product::<usize>();
    println!("p1: {} ({:?})", p1, now.elapsed());

    let now = std::time::Instant::now();

    // How many robots?
    println!("count: {}", robots.len());

    // When does each robot end up back where it started?
    let cycles = robots
        .iter()
        .map(|robot| {
            let mut r = robot.clone();
            let mut n = 0;
            loop {
                r.step_n(1, MAX_X, MAX_Y);
                n += 1;
                if r.position == robot.position {
                    break n;
                }
            }
        })
        .collect::<Vec<usize>>();

    // We can find the least common multiple of all cycles to find the maximum
    // number of steps we'll need to pre-compute.
    let lcm = cycles
        .iter()
        .fold(cycles[0], |acc, &x| lcm(acc as usize, x as usize));
    println!("lcm: {}", lcm);

    // Create all the maps for all iterations.
    let maps = (0..lcm)
        .map(|n| {
            let robots = robots
                .iter()
                .map(|robot| {
                    let mut r = robot.clone();
                    r.step_n(n as isize, MAX_X, MAX_Y);
                    r.position
                })
                .collect::<Vec<Point>>();
            let mut map = vec![vec![0; MAX_X as usize]; MAX_Y as usize];
            for robot in robots {
                map[robot.y as usize][robot.x as usize] += 1;
            }
            map
        })
        .collect::<Vec<Vec<Vec<usize>>>>();

    // Test each map to see how "central" the values are. Presumably the map
    // with the robots closest to the middle will have the tree.
    let centrality_values = maps.iter().map(|map| centrality(map)).collect::<Vec<f64>>();
    let (max_centrality, _) = centrality_values
        .iter()
        .enumerate()
        .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap())
        .unwrap();
    println!("p2: {} ({:?})", max_centrality, now.elapsed());

    // Print the map with the highest centrality.
    // let map = &maps[0];
    // for row in map {
    //     for count in row {
    //         print!("{}", if *count > 0 { '#' } else { '.' });
    //     }
    //     println!();
    // }

    // This also apparently works too after checking reddit. The smallest safety
    // factor is the answer.
    let now = std::time::Instant::now();
    let min_safety_factor = (0..lcm)
        .map(|n| {
            robots
                .iter()
                .filter_map(|robot| {
                    let mut r = robot.clone();
                    r.step_n(n as isize, MAX_X, MAX_Y);
                    r.quadrant(MAX_X, MAX_Y)
                })
                .fold([0; 4], |mut acc, x| {
                    acc[x] += 1;
                    acc
                })
                .iter()
                .product::<usize>()
        })
        .enumerate()
        .min_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap())
        .unwrap()
        .0;
    println!("p2: {} ({:?})", min_safety_factor, now.elapsed());

    Ok(())
}

fn gcd(a: usize, b: usize) -> usize {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

fn lcm(a: usize, b: usize) -> usize {
    a / gcd(a, b) * b
}

fn centrality(map: &[Vec<usize>]) -> f64 {
    let rows = map.len();
    let cols = map[0].len();

    // Find center of map
    let center_row = rows as f64 / 2.0;
    let center_col = cols as f64 / 2.0;

    let (score, count) = map
        .iter()
        .enumerate()
        .fold((0.0, 0), |(score, count), (r, row)| {
            row.iter()
                .enumerate()
                .fold((score, count), |(score, count), (c, &value)| {
                    // Skip places where there are no robots.
                    if value == 0 {
                        return (score, count);
                    }

                    // Calculate distance from center.
                    let row_dist = (r as f64 - center_row).abs();
                    let col_dist = (c as f64 - center_col).abs();
                    let distance = (row_dist * row_dist + col_dist * col_dist).sqrt();

                    // We want to do inverse of distance because closer should be better (higher).
                    (score + 1.0 / (distance + 1.0), count + value)
                })
        });

    // Average the score over the number of robots.
    score / count as f64
}
