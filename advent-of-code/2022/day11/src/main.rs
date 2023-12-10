use std::fs;

#[derive(Debug, Clone)]
struct Monkey {
    items: Vec<usize>,
    op: Vec<String>,
    test_divisible: usize,
    test_true: usize,
    test_false: usize,
}

impl Monkey {
    fn from(s: &str) -> Self {
        let lines = s.lines().collect::<Vec<_>>();
        Self {
            items: lines[1]
                .split(": ")
                .nth(1)
                .unwrap()
                .split(", ")
                .map(|s| s.parse::<usize>().unwrap())
                .collect(),
            op: lines[2].split(" ").map(|s| String::from(s)).collect(),
            test_divisible: lines[3]
                .split(' ')
                .nth(5)
                .unwrap()
                .parse::<usize>()
                .unwrap(),
            test_true: lines[4]
                .split(' ')
                .nth(9)
                .unwrap()
                .parse::<usize>()
                .unwrap(),
            test_false: lines[5]
                .split(' ')
                .nth(9)
                .unwrap()
                .parse::<usize>()
                .unwrap(),
        }
    }

    fn test(&self, item: usize) -> bool {
        item % self.test_divisible == 0
    }

    fn op(&self, old: usize) -> usize {
        let rhs = match self.op[7].as_str() {
            "old" => old,
            _ => self.op[7].parse::<usize>().unwrap(),
        };

        match self.op[6].as_str() {
            "+" => old + rhs,
            "/" => old / rhs,
            _ => old * rhs,
        }
    }
}

fn main() {
    let input = fs::read_to_string("input").unwrap();
    let mut monkeys = input
        .split("\n\n")
        .map(|s| Monkey::from(s))
        .collect::<Vec<_>>();
    let mut inspections = vec![0; monkeys.len()];
    for _ in 0..20 {
        for m in 0..monkeys.len() {
            let monkey = monkeys[m].clone();
            inspections[m] += monkey.items.len();
            for item in &monkey.items {
                let new = monkey.op(*item) / 3;
                if monkey.test(new) {
                    monkeys[monkey.test_true].items.push(new);
                } else {
                    monkeys[monkey.test_false].items.push(new);
                }
            }
            monkeys[m].items = vec![];
        }
    }
    inspections.sort();
    inspections.reverse();
    println!("p1: {}", inspections[0] * inspections[1]);

    let mut monkeys = input
        .split("\n\n")
        .map(|s| Monkey::from(s))
        .collect::<Vec<_>>();
    let mut inspections = vec![0; monkeys.len()];
    // We can do modulo arithmetic here to keep the size low. In essence, no number need to be larger than the product of all the divisors. Before we throw to the new monkey, we can downsize the value.
    let p = monkeys.iter().map(|m| m.test_divisible).product::<usize>();
    for _ in 0..10000 {
        for m in 0..monkeys.len() {
            let monkey = monkeys[m].clone();
            inspections[m] += monkey.items.len();
            for item in &monkey.items {
                let new = monkey.op(*item);
                if monkey.test(new) {
                    monkeys[monkey.test_true].items.push(new % p);
                } else {
                    monkeys[monkey.test_false].items.push(new % p);
                }
            }
            monkeys[m].items = vec![];
        }
    }
    inspections.sort();
    inspections.reverse();
    println!("p2: {}", inspections[0] * inspections[1]);
}
