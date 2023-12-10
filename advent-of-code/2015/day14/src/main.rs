#[derive(Debug)]
struct Reindeer {
    name: String,
    speed: i32,
    duration: i32,
    rest: i32,
}

impl From<&str> for Reindeer {
    fn from(s: &str) -> Self {
        let parts = s.split(" ").collect::<Vec<_>>();
        let name = parts[0];
        let speed = parts[3].parse::<i32>().unwrap();
        let duration = parts[6].parse::<i32>().unwrap();
        let rest = parts[13].parse::<i32>().unwrap();
        Reindeer {
            name: name.to_string(),
            speed,
            duration,
            rest,
        }
    }
}

impl Reindeer {
    fn distance(&self, time: i32) -> i32 {
        let cycle = self.duration + self.rest;
        let cycles = time / cycle;
        let remainder = time % cycle;
        let mut distance = cycles * self.speed * self.duration;
        if remainder > self.duration {
            distance += self.speed * self.duration;
        } else {
            distance += self.speed * remainder;
        }
        distance
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let rr = input.lines().map(|l| Reindeer::from(l)).collect::<Vec<_>>();
    let mut max = 0;
    for r in rr.iter() {
        let d = r.distance(2503);
        if d > max {
            max = d;
        }
    }
    println!("Part 1: {}", max);

    let mut points = vec![0; rr.len()];
    for t in 1..=2503 {
        let mut max = 0;
        let mut max_i = vec![];
        for (i, r) in rr.iter().enumerate() {
            let d = r.distance(t);
            if d == max {
                max_i.push(i);
            } else if d > max {
                max = d;
                max_i = vec![i];
            }
        }
        for i in max_i {
            points[i] += 1;
        }
    }
    println!("Part 2: {}", points.iter().max().unwrap());
}
