use std::collections::HashSet;

use nom::{
    bytes::complete::tag, character::complete::alpha1, multi::separated_list1, sequence::tuple,
    IResult,
};

#[derive(Debug)]
struct Replacement<'a> {
    from: &'a str,
    to: &'a str,
}

impl<'a> Replacement<'a> {
    fn new(from: &'a str, to: &'a str) -> Self {
        Self { from, to }
    }

    fn parse(input: &'a str) -> IResult<&str, Self> {
        let (input, (from, _, to)) = tuple((alpha1, tag(" => "), alpha1))(input)?;
        Ok((input, Self::new(from, to)))
    }
}

#[derive(Debug)]
struct Input<'a> {
    replacements: Vec<Replacement<'a>>,
    molecule: &'a str,
}

impl<'a> Input<'a> {
    fn new(replacements: Vec<Replacement<'a>>, molecule: &'a str) -> Self {
        Self {
            replacements,
            molecule,
        }
    }

    fn parse(input: &'a str) -> IResult<&str, Self> {
        let (input, replacements) = separated_list1(tag("\n"), Replacement::parse)(input)?;
        let (input, _) = tag("\n\n")(input)?;
        let (input, molecule) = alpha1(input)?;
        Ok((input, Self::new(replacements, molecule)))
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let input = Input::parse(&input).unwrap().1;
    let mut molecules = HashSet::new();
    for replacement in &input.replacements {
        for (i, _) in input.molecule.match_indices(replacement.from) {
            let mut molecule = input.molecule.to_string();
            molecule.replace_range(i..i + replacement.from.len(), replacement.to);
            molecules.insert(molecule);
        }
    }
    println!("p1: {}", molecules.len());

    // The trick here was to look at the replacements an realize they form a tree.
    let mut steps = 0;
    let mut molecule = input.molecule.to_string();
    while molecule != "e" {
        for replacement in &input.replacements {
            if let Some(i) = molecule.find(replacement.to) {
                molecule.replace_range(i..i + replacement.to.len(), replacement.from);
                steps += 1;
            }
        }
    }
    println!("p2: {}", steps);
}
