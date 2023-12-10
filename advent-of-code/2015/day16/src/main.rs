#[derive(Debug)]
struct Analysis {
    children: i32,
    cats: i32,
    samoyeds: i32,
    pomeranians: i32,
    akitas: i32,
    vizslas: i32,
    goldfish: i32,
    trees: i32,
    cars: i32,
    perfumes: i32,
}

impl Analysis {
    fn new() -> Self {
        Analysis {
            children: -1,
            cats: -1,
            samoyeds: -1,
            pomeranians: -1,
            akitas: -1,
            vizslas: -1,
            goldfish: -1,
            trees: -1,
            cars: -1,
            perfumes: -1,
        }
    }

    fn p1(&self, other: &Self) -> bool {
        if self.children != -1 && self.children != other.children {
            return false;
        }
        if self.cats != -1 && self.cats != other.cats {
            return false;
        }
        if self.samoyeds != -1 && self.samoyeds != other.samoyeds {
            return false;
        }
        if self.pomeranians != -1 && self.pomeranians != other.pomeranians {
            return false;
        }
        if self.akitas != -1 && self.akitas != other.akitas {
            return false;
        }
        if self.vizslas != -1 && self.vizslas != other.vizslas {
            return false;
        }
        if self.goldfish != -1 && self.goldfish != other.goldfish {
            return false;
        }
        if self.trees != -1 && self.trees != other.trees {
            return false;
        }
        if self.cars != -1 && self.cars != other.cars {
            return false;
        }
        if self.perfumes != -1 && self.perfumes != other.perfumes {
            return false;
        }
        true
    }

    fn p2(&self, other: &Self) -> bool {
        if self.children != -1 && self.children != other.children {
            return false;
        }
        if self.cats != -1 && self.cats <= other.cats {
            return false;
        }
        if self.samoyeds != -1 && self.samoyeds != other.samoyeds {
            return false;
        }
        if self.pomeranians != -1 && self.pomeranians >= other.pomeranians {
            return false;
        }
        if self.akitas != -1 && self.akitas != other.akitas {
            return false;
        }
        if self.vizslas != -1 && self.vizslas != other.vizslas {
            return false;
        }
        if self.goldfish != -1 && self.goldfish >= other.goldfish {
            return false;
        }
        if self.trees != -1 && self.trees <= other.trees {
            return false;
        }
        if self.cars != -1 && self.cars != other.cars {
            return false;
        }
        if self.perfumes != -1 && self.perfumes != other.perfumes {
            return false;
        }
        true
    }
}

impl From<&str> for Analysis {
    fn from(s: &str) -> Self {
        let mut analysis = Analysis::new();
        let parts: Vec<_> = s
            .split(" ")
            .map(|s| s.trim_end_matches(":").trim_end_matches(","))
            .collect::<_>();

        for i in (2..parts.len()).step_by(2) {
            let value = parts[i + 1].parse().unwrap();
            match parts[i] {
                "children" => analysis.children = value,
                "cats" => analysis.cats = value,
                "samoyeds" => analysis.samoyeds = value,
                "pomeranians" => analysis.pomeranians = value,
                "akitas" => analysis.akitas = value,
                "vizslas" => analysis.vizslas = value,
                "goldfish" => analysis.goldfish = value,
                "trees" => analysis.trees = value,
                "cars" => analysis.cars = value,
                "perfumes" => analysis.perfumes = value,
                _ => panic!("Unknown property"),
            }
        }

        analysis
    }
}

fn main() {
    let expected = Analysis {
        children: 3,
        cats: 7,
        samoyeds: 2,
        pomeranians: 3,
        akitas: 0,
        vizslas: 0,
        goldfish: 5,
        trees: 3,
        cars: 2,
        perfumes: 1,
    };

    let input = std::fs::read_to_string("input").unwrap();
    let sues = input.lines().map(Analysis::from).collect::<Vec<_>>();

    for (i, s) in sues.iter().enumerate() {
        if s.p1(&expected) {
            println!("p1: {}", i + 1);
            break;
        }
    }

    for (i, s) in sues.iter().enumerate() {
        if s.p2(&expected) {
            println!("p2: {}", i + 1);
            break;
        }
    }
}
