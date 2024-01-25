use std::collections::{HashMap, VecDeque};

#[derive(Debug, Copy, Clone)]
enum Give {
    Bot(usize),
    Output(usize),
}

impl From<&[&str]> for Give {
    fn from(s: &[&str]) -> Self {
        let value = s[1].parse().unwrap();
        match s[0] {
            "bot" => Give::Bot(value),
            "output" => Give::Output(value),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
struct Bot {
    id: usize,
    chips: Vec<usize>,
    give_low: Give,
    give_high: Give,
}

impl Bot {
    fn receive(&mut self, value: usize) -> Option<Vec<(Give, usize)>> {
        self.chips.push(value);
        self.chips.sort();
        if self.chips.len() == 2 {
            let low = self.chips[0];
            let high = self.chips[1];
            if low == 17 && high == 61 {
                println!("p1: {}", self.id);
            }
            self.chips.clear();
            Some(vec![(self.give_low, low), (self.give_high, high)])
        } else {
            None
        }
    }
}

impl From<&[&str]> for Bot {
    fn from(s: &[&str]) -> Self {
        let id = s[1].parse().unwrap();
        let give_low = Give::from(&s[5..7]);
        let give_high = Give::from(&s[10..12]);
        Bot {
            id,
            chips: Vec::new(),
            give_low,
            give_high,
        }
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let mut bots = HashMap::new();
    let mut gives = Vec::new();
    for line in input.lines() {
        let words = line.split_whitespace().collect::<Vec<_>>();
        match words[0] {
            "value" => {
                let value: usize = words[1].parse().unwrap();
                let give = Give::from(&words[4..6]);
                gives.push((give, value));
            }
            "bot" => {
                let bot: Bot = words.as_slice().into();
                bots.insert(bot.id, bot);
            }
            _ => unreachable!(),
        }
    }

    let mut outputs = HashMap::new();
    let mut queue = VecDeque::new();
    gives.into_iter().for_each(|(give, value)| {
        queue.push_back((give, value));
    });
    while let Some((give, value)) = queue.pop_front() {
        match give {
            Give::Bot(id) => {
                let bot = bots.get_mut(&id).unwrap();
                if let Some(gives) = bot.receive(value) {
                    gives.into_iter().for_each(|(give, value)| {
                        queue.push_back((give, value));
                    });
                }
            }
            Give::Output(id) => {
                outputs.entry(id).or_insert(Vec::new()).push(value);
            }
        }
    }

    println!("p2: {}", outputs[&0][0] * outputs[&1][0] * outputs[&2][0]);
}
