use std::{cmp::Ordering, fs, str::FromStr};

#[derive(Eq, Debug, Clone)]
enum Packet {
    Integer(usize),
    List(Vec<Packet>),
}

impl PartialEq for Packet {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Packet::Integer(l), Packet::Integer(r)) => l.cmp(r),
            (Packet::List(l), Packet::List(r)) => l.cmp(r),
            (Packet::Integer(l), Packet::List(r)) => {
                Packet::List(vec![Packet::Integer(*l)]).cmp(&Packet::List(r.clone()))
            }
            (Packet::List(l), Packet::Integer(r)) => {
                Packet::List(l.clone()).cmp(&Packet::List(vec![Packet::Integer(*r)]))
            }
        }
    }
}

impl FromStr for Packet {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Trim list edges.
        let s = &s[1..s.len() - 1];

        // We'll use stack to store depth of lists.
        let mut stack = vec![];
        let mut integer = vec![];
        let mut list = vec![];

        for c in s.chars() {
            match c {
                '0'..='9' => integer.push(c),
                ',' => {
                    if integer.len() > 0 {
                        list.push(Packet::Integer(
                            integer.iter().collect::<String>().parse::<usize>().unwrap(),
                        ));
                        integer = vec![];
                    }
                }
                '[' => {
                    stack.push((integer, list));
                    integer = vec![];
                    list = vec![];
                }
                ']' => {
                    if integer.len() > 0 {
                        list.push(Packet::Integer(
                            integer.iter().collect::<String>().parse::<usize>().unwrap(),
                        ));
                    }
                    let p = Packet::List(list);
                    (integer, list) = stack.pop().unwrap();
                    list.push(p);
                }
                _ => panic!("unexpected char: {}", c),
            }
        }

        if integer.len() > 0 {
            list.push(Packet::Integer(
                integer.iter().collect::<String>().parse::<usize>().unwrap(),
            ))
        }
        Ok(Packet::List(list))
    }
}

fn main() {
    let input = fs::read_to_string("input").unwrap();
    let sets = input
        .split("\n\n")
        .map(|s| {
            s.split('\n')
                .map(|l| Packet::from_str(l).unwrap())
                .collect()
        })
        .collect::<Vec<Vec<Packet>>>();
    let mut p1 = 0;
    for (i, set) in sets.iter().enumerate() {
        if set[0].cmp(&set[1]) == Ordering::Less {
            p1 += i + 1;
        }
    }
    println!("p1: {p1}");

    let mut all = sets.iter().flat_map(|s| s.iter()).collect::<Vec<&Packet>>();
    let two = Packet::from_str("[[2]]").unwrap();
    let six = Packet::from_str("[[6]]").unwrap();
    all.push(&two);
    all.push(&six);
    all.sort();

    let mut p2 = 1;
    for (i, p) in all.iter().enumerate() {
        if **p == two {
            p2 *= i + 1;
        } else if **p == six {
            p2 *= i + 1;
        }
    }
    println!("p2: {p2}");
}
