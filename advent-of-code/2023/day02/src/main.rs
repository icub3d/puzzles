use std::arch::x86_64::_mm256_mask_packs_epi16;

use nom::bytes::complete::tag;
use nom::character::complete::{alpha0, space0};
use nom::multi::separated_list0;
use nom::sequence::{delimited, separated_pair, tuple};
use nom::IResult;

#[derive(Debug)]
struct Round {
    red: usize,
    green: usize,
    blue: usize,
}

impl Round {
    fn is_valid(&self) -> bool {
        self.red <= 12 && self.green <= 13 && self.blue <= 14
    }

    fn color(input: &str) -> IResult<&str, (u64, &str)> {
        separated_pair(nom::character::complete::u64, tag(" "), alpha0)(input)
    }

    fn parse(input: &str) -> IResult<&str, Round> {
        let (input, colors) = separated_list0(tag(", "), Round::color)(input)?;
        let mut round = Round {
            red: 0,
            green: 0,
            blue: 0,
        };
        for (value, color) in colors {
            match color {
                "red" => round.red = value as usize,
                "green" => round.green = value as usize,
                "blue" => round.blue = value as usize,
                _ => panic!("unknown color"),
            }
        }
        Ok((input, round))
    }
}

#[derive(Debug)]
struct Game {
    id: usize,
    rounds: Vec<Round>,
}

impl Game {
    fn power(&self) -> usize {
        let red = self.rounds.iter().map(|r| r.red).max();
        let green = self.rounds.iter().map(|r| r.green).max();
        let blue = self.rounds.iter().map(|r| r.blue).max();
        red.unwrap() * green.unwrap() * blue.unwrap()
    }

    fn parse(input: &str) -> IResult<&str, Game> {
        let (input, id) = tuple((
            tag("Game "),
            space0,
            nom::character::complete::u64,
            tag(": "),
        ))(input)?;

        let (input, rounds) = separated_list0(tag("; "), Round::parse)(input)?;
        Ok((
            input,
            Game {
                id: id as usize,
                rounds,
            },
        ))
    }

    fn parse_all(input: &str) -> Vec<Game> {
        separated_list0(tag("\n"), Game::parse)(input).unwrap().1
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let games = Game::parse_all(&input);

    let sum = games
        .iter()
        .filter(|g| g.rounds.iter().all(|r| r.is_valid()))
        .map(|g| g.id)
        .sum::<usize>();
    println!("p1: {}", sum);

    let sum_power = games.iter().map(|g| g.power()).sum::<usize>();
    println!("p2: {}", sum_power);
}
