use std::collections::HashSet;

use nom::bytes::complete::tag;
use nom::character::complete::space1;
use nom::multi::separated_list0;
use nom::sequence::{separated_pair, tuple};
use nom::IResult;

#[derive(Debug)]
struct Card {
    winners: HashSet<usize>,
    ours: HashSet<usize>,
}

impl Card {
    fn wins(&self) -> usize {
        self.winners.intersection(&self.ours).count()
    }

    fn points(&self) -> usize {
        let wins = self.wins();
        if wins == 0 {
            0
        } else {
            1 << (wins - 1)
        }
    }

    fn parse_set(input: &str) -> IResult<&str, HashSet<usize>> {
        let (input, nums) = separated_list0(space1, nom::character::complete::u64)(input)?;
        Ok((input, nums.into_iter().map(|n| n as usize).collect()))
    }

    fn parse_all(input: &str) -> Vec<Self> {
        separated_list0(tag("\n"), Card::parse)(input).unwrap().1
    }

    fn parse(input: &str) -> IResult<&str, Self> {
        let (input, (_, _, _, _, _)) = tuple((
            tag("Card"),
            space1,
            nom::character::complete::u64,
            tag(":"),
            space1,
        ))(input)?;
        let (input, (winners, ours)) =
            separated_pair(Card::parse_set, Card::parse_pipe, Card::parse_set)(input)?;
        Ok((input, Self { winners, ours }))
    }

    fn parse_pipe(input: &str) -> IResult<&str, ()> {
        tuple(((space1), tag("|"), space1))(input).map(|(i, _)| (i, ()))
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let cards = Card::parse_all(&input);
    let p1 = cards.iter().map(|c| c.points()).sum::<usize>();
    println!("p1: {:?}", p1);

    let mut counts = vec![1; cards.len()];
    for i in 0..cards.len() {
        for j in 0..cards[i].wins() {
            counts[i + j + 1] += counts[i];
        }
    }
    println!("p2: {:?}", counts.iter().sum::<usize>());
}
