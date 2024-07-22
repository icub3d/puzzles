struct Disc {
    positions: usize,
    current_position: usize,
}

impl Disc {
    fn new(positions: usize, current_position: usize) -> Disc {
        Disc {
            positions,
            current_position,
        }
    }

    fn from_string(input: &str) -> Disc {
        let parts: Vec<&str> = input.split_whitespace().collect();
        let positions = parts[3].parse().unwrap();
        let current_position = parts[11].trim_end_matches('.').parse().unwrap();
        Disc::new(positions, current_position)
    }

    fn is_open(&self, t: usize) -> bool {
        (self.current_position + t) % self.positions == 0
    }
}

fn main() {
    let input = include_str!("../input");
    let mut discs: Vec<Disc> = input.lines().map(Disc::from_string).collect();
    println!("p1: {}", run(&discs));

    discs.push(Disc::new(11, 0));
    println!("p2: {}", run(&discs));
}

fn run(discs: &[Disc]) -> usize {
    let mut t = 0;
    loop {
        if discs
            .iter()
            .enumerate()
            .all(|(i, disc)| disc.is_open(t + i + 1))
        {
            break;
        }
        t += 1;
    }
    t
}
