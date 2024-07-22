use std::collections::HashMap;

use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, digit1},
    combinator::opt,
    multi::separated_list1,
    sequence::tuple,
    IResult,
};

fn parse_name(input: &str) -> IResult<&str, String> {
    let (input, name) = alpha1(input)?;
    Ok((input, name.to_string()))
}

fn parse_weight(input: &str) -> IResult<&str, usize> {
    let (input, (_, weight, _)) = tuple((tag("("), digit1, tag(")")))(input)?;
    Ok((input, weight.parse().unwrap()))
}

fn parse_children(input: &str) -> IResult<&str, Vec<String>> {
    let (input, children) = separated_list1(tag(", "), parse_name)(input)?;
    Ok((input, children))
}

fn parse_line(input: &str) -> IResult<&str, (String, usize, Vec<String>)> {
    let (input, (name, _, weight, children)) = tuple((
        parse_name,
        tag(" "),
        parse_weight,
        opt(tuple((tag(" -> "), parse_children))),
    ))(input)?;
    let children = children.map(|(_, children)| children).unwrap_or_default();
    Ok((input, (name, weight, children)))
}

fn main() {
    let input = include_str!("../input");
    let programs = input
        .lines()
        .map(|line| {
            let (_, (name, weight, children)) = parse_line(line).unwrap();
            (name, (weight, children))
        })
        .collect::<HashMap<_, _>>();

    let mut names = programs.keys().collect::<Vec<_>>();

    for (_, children) in programs.values() {
        for child in children {
            names.retain(|&n| n != child);
        }
    }
    println!("p1: {:?}", names[0]);

    let mut weights = HashMap::new();
    get_weights(&mut weights, &programs, names[0]);
    println!("p2: {}", find_weight_to_fix(&programs, names[0], &weights));
}

fn find_weight_to_fix(
    programs: &HashMap<String, (usize, Vec<String>)>,
    parent: &str,
    weights: &HashMap<String, usize>,
) -> usize {
    // If there are no children, we are done.
    if programs.get(parent).unwrap().1.is_empty() {
        return 0;
    }

    // Count all the different weights among the children.
    let mut weight_counts: HashMap<usize, Vec<String>> = HashMap::new();
    for child in &programs.get(parent).unwrap().1 {
        let weight = weights.get(child).unwrap();
        (*weight_counts.entry(*weight).or_default()).push(child.to_string());
    }

    // If all the weights are the same, we are done.
    if weight_counts.len() == 1 {
        return 0;
    }

    // Find the weight that is different
    let (wrong_weight, wrong_names) = weight_counts
        .iter()
        .find(|(_, names)| names.len() == 1)
        .unwrap();
    let wrong_name = wrong_names[0].clone();

    // Check the children weights to see if the wrong weight is a child.
    let mut children_weight_counts: HashMap<usize, Vec<String>> = HashMap::new();
    for child in &programs.get(&wrong_name).unwrap().1 {
        let weight = weights.get(child).unwrap();
        (*children_weight_counts.entry(*weight).or_default()).push(child.to_string());
    }
    if children_weight_counts.len() == 1 {
        // If all the children have the same weight, the wrong weight is the parent.
        let correct_weight = weight_counts.keys().find(|w| *w != wrong_weight).unwrap();
        *correct_weight
            - children_weight_counts
                .iter()
                .map(|(w, v)| w * v.len())
                .sum::<usize>()
    } else {
        // If the children have different weights, the wrong weight is a child.
        find_weight_to_fix(programs, &wrong_name, weights)
    }
}

#[allow(dead_code)]
fn print_tree(
    programs: &HashMap<String, (usize, Vec<String>)>,
    name: &str,
    indent: usize,
    weights: &HashMap<String, usize>,
) {
    let (weight, children) = programs.get(name).unwrap();
    let total_weight = weights.get(name).unwrap();
    println!(
        "{:indent$}{} ({}) -> {}",
        "",
        name,
        total_weight,
        weight,
        indent = indent
    );
    for child in children {
        print_tree(programs, child, indent + 2, weights);
    }
}

fn get_weights(
    weights: &mut HashMap<String, usize>,
    programs: &HashMap<String, (usize, Vec<String>)>,
    name: &str,
) -> usize {
    let (weight, children) = programs.get(name).unwrap();
    let mut total_weight = *weight;
    for child in children {
        total_weight += get_weights(weights, programs, child);
    }
    weights.insert(name.to_string(), total_weight);
    total_weight
}
