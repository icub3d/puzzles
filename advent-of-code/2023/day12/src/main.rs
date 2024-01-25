use std::{
    collections::HashMap,
    sync::{Mutex, OnceLock},
};

use nom::{
    bytes::complete::tag,
    character::complete::{digit1, newline, one_of, space1},
    multi::{many1, separated_list1},
    IResult,
};

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
enum Spring {
    Operational,
    Damaged,
    Unknown,
}

impl From<char> for Spring {
    fn from(c: char) -> Self {
        match c {
            '.' => Spring::Operational,
            '#' => Spring::Damaged,
            _ => Spring::Unknown,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
struct Record {
    springs: Vec<Spring>,
    groups: Vec<usize>,
}

impl Record {
    fn new(springs: Vec<Spring>, groups: Vec<usize>) -> Self {
        Self { springs, groups }
    }

    fn parse(input: &str) -> IResult<&str, Record> {
        let (input, springs) = many1(one_of(".#?"))(input)?;
        let (input, _) = space1(input)?;
        let (input, groups) = separated_list1(tag(","), digit1)(input)?;
        Ok((
            input,
            Record {
                springs: springs.into_iter().map(|c| c.into()).collect(),
                groups: groups.into_iter().map(|g| g.parse().unwrap()).collect(),
            },
        ))
    }

    fn parse_all(input: &str) -> Vec<Record> {
        separated_list1(newline, Record::parse)(input).unwrap().1
    }

    fn expand(&self) -> Record {
        let springs = self
            .springs
            .iter()
            .cloned()
            .chain([Spring::Unknown].iter().cloned())
            .cycle()
            .take(self.springs.len() * 5 + 4)
            .collect();
        let groups = self
            .groups
            .iter()
            .cloned()
            .cycle()
            .take(self.groups.len() * 5)
            .collect();
        Record::new(springs, groups)
    }
}

fn memo_hits() -> &'static Mutex<usize> {
    static HITS: OnceLock<Mutex<usize>> = OnceLock::new();
    HITS.get_or_init(|| Mutex::new(0))
}

fn possible_solutions(memo: &mut HashMap<Record, usize>, record: &Record) -> usize {
    // Check to see if we have already calculated this record.
    if let Some(&v) = memo.get(record) {
        *memo_hits().lock().unwrap() += 1;
        return v;
    }

    // If we have no groups left we have a solution if there are no
    // other damaged springs in our list. Otherwise, it can't be valid.
    if record.groups.is_empty() {
        let v = match record.springs.iter().any(|c| *c == Spring::Damaged) {
            true => 0,
            false => 1,
        };
        memo.insert(record.clone(), v);
        return v;
    }

    // At this point, we have some number of groups left, so make sure
    // we we have enough springs left to fill them.
    if record.springs.len() < record.groups.iter().sum::<usize>() + record.groups.len() - 1 {
        memo.insert(record.clone(), 0);
        return 0;
    }

    // We can't do anything with operational springs. So just skip it.
    if record.springs[0] == Spring::Operational {
        let solutions = possible_solutions(
            memo,
            &Record::new(record.springs[1..].to_vec(), record.groups.clone()),
        );
        memo.insert(record.clone(), solutions);
        return solutions;
    }

    // At this point, we know we are at the beginning of a possible
    // position for the current group. We can check if that's possible
    // and if it is, we can see how many valid solutions we'd get if
    // we did.
    let mut solutions = 0;
    let cur = record.groups[0];
    let all_non_operational = record.springs[0..cur]
        .iter()
        .all(|c| *c != Spring::Operational);
    let end = (cur + 1).min(record.springs.len());
    if all_non_operational
        && ((record.springs.len() > cur && record.springs[cur] != Spring::Damaged)
            || record.springs.len() <= cur)
    {
        solutions = possible_solutions(
            memo,
            &Record::new(record.springs[end..].to_vec(), record.groups[1..].to_vec()),
        );
    }

    // If our current position is a unknown, we could also choose not
    // to use it, so include those possibilities.
    if record.springs[0] == Spring::Unknown {
        solutions += possible_solutions(
            memo,
            &Record::new(record.springs[1..].to_vec(), record.groups.clone()),
        );
    }

    // We now know the number of solutions for this record, so memoize
    // it and return it.
    memo.insert(record.clone(), solutions);
    solutions
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let records = Record::parse_all(&input);

    // p1
    let mut memo = HashMap::new();
    let now = std::time::Instant::now();
    let solutions = records
        .iter()
        .map(|r| possible_solutions(&mut memo, r))
        .sum::<usize>();
    println!(
        "p1: {} ({}) ({:?})",
        solutions,
        memo_hits().lock().unwrap(),
        now.elapsed()
    );

    // p2
    let now = std::time::Instant::now();
    let solutions = records
        .iter()
        .map(|r| possible_solutions(&mut memo, &r.expand()))
        .sum::<usize>();
    println!(
        "p2: {} ({}) ({:?})",
        solutions,
        memo_hits().lock().unwrap(),
        now.elapsed()
    );
}
