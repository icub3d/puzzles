use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, digit1},
    combinator::map,
    multi::separated_list1,
    sequence::preceded,
    IResult,
};

#[derive(Debug)]
enum Operation {
    Remove,
    Add(usize),
}

#[derive(Debug)]
struct Step<'a> {
    label: &'a str,
    operation: Operation,
}

impl<'a> Step<'a> {
    // rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
    fn parse(input: &'a str) -> IResult<&str, Self> {
        // Start with the label.
        let (input, label) = alpha1(input)?;
        let (input, operation) = alt((
            // We'll either get a '-' or a '=' followed by a number.
            map(tag("-"), |_| Operation::Remove),
            map(preceded(tag("="), digit1), |s: &str| {
                Operation::Add(s.parse().unwrap())
            }),
        ))(input)?;
        Ok((input, Self::new(label, operation)))
    }

    fn parse_all(input: &'a str) -> Vec<Self> {
        separated_list1(tag(","), Step::parse)(input).unwrap().1
    }

    fn new(label: &'a str, operation: Operation) -> Self {
        Self { label, operation }
    }

    fn hash(&self) -> usize {
        // We can reuse the hash function from part 1.
        let mut h = 0;
        for c in self.label.bytes() {
            h += c as usize;
            h *= 17;
            h %= 256;
        }
        h
    }
}

#[derive(Debug, Clone)]
struct Lens<'a> {
    label: &'a str,
    focal_length: usize,
}

impl<'a> Lens<'a> {
    fn new(label: &'a str, focal_length: usize) -> Self {
        Self {
            label,
            focal_length,
        }
    }
}

struct Boxes<'a> {
    boxes: Vec<Vec<Lens<'a>>>,
}

impl<'a> Boxes<'a> {
    fn new() -> Self {
        Self {
            boxes: vec![vec![]; 256],
        }
    }

    fn perform_steps(&mut self, steps: &'a [Step<'a>]) {
        for step in steps {
            self.perform_step(step);
        }
    }

    fn perform_step(&mut self, step: &'a Step) {
        match step.operation {
            Operation::Remove => self.remove(step.hash(), step.label),
            Operation::Add(focal_length) => self.add(step.hash(), step.label, focal_length),
        }
    }

    fn add(&mut self, hash: usize, label: &'a str, focal_length: usize) {
        // Look to see if it already exists.
        for l in self.boxes[hash].iter_mut() {
            if l.label == label {
                // If we find it, we just need to update the focal
                // length.
                l.focal_length = focal_length;
                return;
            }
        }

        // Otherwise, we need to add it.
        let lens = Lens::new(label, focal_length);
        self.boxes[hash].push(lens);
    }

    fn remove(&mut self, hash: usize, label: &'a str) {
        // Try to find it.
        for (i, l) in self.boxes[hash].iter().enumerate() {
            if l.label == label {
                // If we find it, we need to remove it.
                self.boxes[hash].remove(i);
                return;
            }
        }
    }

    fn focusing_power(&self) -> usize {
        let mut power = 0;
        // in rust: box is a keyword.
        for (box_number, bx) in self.boxes.iter().enumerate() {
            for (slot, lens) in bx.iter().enumerate() {
                power += (box_number + 1) * (slot + 1) * lens.focal_length;
            }
        }
        power
    }
}

fn hash(s: &str) -> usize {
    // Just kind of following instructions here. Note that we are
    // using usize here to prevent overflow.
    let mut h = 0;
    for c in s.bytes() {
        h += c as usize;
        h *= 17;
        h %= 256;
    }
    h
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let p1 = input.split(',').map(hash).sum::<usize>();
    println!("p1: {}", p1);

    let steps = Step::parse_all(&input);
    let mut boxes = Boxes::new();
    boxes.perform_steps(&steps);
    let p2 = boxes.focusing_power();
    println!("p2: {:?}", p2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hash() {
        // I just did some testing here to make sure my hash was
        // working.
        assert_eq!(hash("HASH"), 52);
        assert_eq!(hash("rn"), 0);
        assert_eq!(hash("cm"), 0);
    }
}
