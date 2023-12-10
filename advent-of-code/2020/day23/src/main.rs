// Look, ma! I have a type system and OO! :)
use std::collections::HashMap;
use std::fs;

type Cup = usize;
type Cups = Vec<Cup>;

#[derive(Debug)]
struct Game {
    pos: usize,
    cups: Cups,
}

impl Game {
    fn new(start: &str) -> Game {
        Game {
            pos: 0,
            cups: start
                .chars()
                .map(|b| b.to_digit(10).unwrap() as usize)
                .collect::<Cups>(),
        }
    }

    fn round(&mut self, n: usize) {
        let cur = self.cups[self.pos];
        let taken = self.take(3);
        let next = self.next(cur, &taken);
        self.insert(next, taken);
        self.pos = (self.cups.iter().position(|c| c == &cur).unwrap() + 1) % self.cups.len();
    }

    fn take(&mut self, n: usize) -> Cups {
        let mut taken = Cups::new();
        for x in 0..n {
            taken.push(self.cups[(self.pos + x + 1) % self.cups.len()]);
        }
        for took in taken.iter() {
            let pos = self.cups.iter().position(|c| c == took).unwrap();
            self.cups.remove(pos);
        }
        taken
    }

    fn next(&self, cur: usize, taken: &Cups) -> Cup {
        let mut next = match cur {
            1 => self.cups.len() + taken.len(),
            _ => cur - 1,
        };
        while taken.iter().any(|&c| c == next) {
            next = match next {
                1 => self.cups.len() + taken.len(),
                _ => next - 1,
            };
        }
        next
    }

    fn solution(&mut self) -> String {
        let pos = self.cups.iter().position(|&c| c == 1).unwrap();
        self.cups.rotate_left(pos);
        self.cups[1..]
            .iter()
            .map(|c| c.to_string())
            .collect::<String>()
    }

    fn insert(&mut self, next: usize, taken: Cups) {
        let pos = self.cups.iter().position(|&c| c == next).unwrap();
        for took in taken.iter().rev() {
            self.cups.insert(pos + 1, *took);
        }
    }
}

fn main() {
    let input = fs::read_to_string("input").unwrap();
    let mut game = Game::new(&input);
    for x in 0..100 {
        game.round(x);
    }
    println!("{}", game.solution());

    // Use a map to store positions this time, so we don't have to
    // work through the list of 1 million entries 10 million times.
    let cups = input
        .chars()
        .map(|b| b.to_digit(10).unwrap() as usize)
        .collect::<Cups>();
    let mut lookup: HashMap<usize, usize> = HashMap::new();
    for cup in 0..1_000_000 {
        if cup < cups.len() - 1 {
            lookup.insert(cups[cup], cups[cup + 1]);
        } else if cup == cups.len() - 1 {
            lookup.insert(cups[8], 10);
        } else {
            lookup.insert(cup + 1, cup + 2);
        }
    }
    lookup.insert(1_000_000, cups[0]);

    let mut cur = cups[0];
    for _ in 0..10_000_000 {
        // take 3
        let t1 = lookup[&cur];
        let t2 = lookup[&t1];
        let t3 = lookup[&t2];
        lookup.insert(cur, lookup[&t3]);

        // determine next.
        let mut next = match cur == 1 {
            true => 1_000_000,
            false => cur - 1,
        };
        while next == t1 || next == t2 || next == t3 {
            next = match next == 1 {
                true => 1_000_000,
                false => next - 1,
            }
        }

        // add back in and set new cur
        lookup.insert(t3, lookup[&next]);
        lookup.insert(next, t1);
        cur = lookup[&cur];
    }
    println!("{}", lookup[&1] * lookup[&lookup[&1]]);
}
