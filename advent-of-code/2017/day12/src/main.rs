use std::collections::{HashMap, HashSet};

use nom::{bytes::complete::tag, character::complete::digit1, multi::separated_list1, IResult};

fn parse_connection(input: &str) -> IResult<&str, (usize, Vec<usize>)> {
    let (input, id) = digit1(input)?;
    let (input, _) = tag(" <-> ")(input)?;
    let (input, connections) = separated_list1(tag(", "), digit1)(input)?;
    Ok((
        input,
        (
            id.parse().unwrap(),
            connections.iter().map(|x| x.parse().unwrap()).collect(),
        ),
    ))
}

fn main() {
    let input = include_str!("../input");
    let input = input
        .lines()
        .map(|x| parse_connection(x).unwrap().1)
        .collect::<Vec<_>>();

    let mut connections: HashMap<usize, HashSet<usize>> = HashMap::new();
    for (id, connected) in input {
        for c in connected {
            connections.entry(id).or_default().insert(c);
            connections.entry(c).or_default().insert(id);
        }
    }

    let mut visited = HashSet::new();
    visit(&connections, &mut visited, 0);
    println!("p1: {}", visited.len());

    let mut found = connections.keys().cloned().collect::<HashSet<_>>();
    let mut groups = 0;
    while !found.is_empty() {
        let id = *found.iter().next().unwrap();
        let mut visited = HashSet::new();
        visit(&connections, &mut visited, id);
        found = found.difference(&visited).cloned().collect();
        groups += 1;
    }
    println!("p2: {}", groups);
}

fn visit(connections: &HashMap<usize, HashSet<usize>>, visited: &mut HashSet<usize>, id: usize) {
    visited.insert(id);
    for c in connections.get(&id).unwrap() {
        if !visited.contains(c) {
            visit(connections, visited, *c);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_connection() {
        assert_eq!(
            parse_connection("0 <-> 2, 3, 4"),
            Ok(("", (0, vec![2, 3, 4])))
        );
    }

    #[test]
    fn test_visit() {
        let mut connections = HashMap::new();
        connections.insert(0, [2].iter().cloned().collect());
        connections.insert(1, [9].iter().cloned().collect());
        connections.insert(2, [0, 3, 4].iter().cloned().collect());
        connections.insert(3, [2, 4].iter().cloned().collect());
        connections.insert(4, [2, 3, 6].iter().cloned().collect());
        connections.insert(5, [6].iter().cloned().collect());
        connections.insert(6, [4, 5].iter().cloned().collect());

        let mut visited = HashSet::new();
        visit(&connections, &mut visited, 0);
        assert_eq!(visited.len(), 6);
    }
}
