use nom::{
    bytes::complete::{tag, take_until},
    character::complete::digit1,
    multi::separated_list0,
    sequence::tuple,
    IResult,
};

use rayon::prelude::*;

use std::ops::Range;

#[derive(Debug)]
struct Mapping {
    range: Range<isize>,
    delta: isize,
}

impl Mapping {
    fn new(source: isize, destination: isize, length: isize) -> Self {
        Self {
            range: source..source + length,
            delta: destination - source,
        }
    }

    fn contains(&self, value: isize) -> bool {
        self.range.contains(&value)
    }

    fn apply(&self, value: isize) -> isize {
        value + self.delta
    }

    fn parse(input: &str) -> IResult<&str, Self> {
        let (input, (destination, _, source, _, length)) =
            tuple((digit1, tag(" "), digit1, tag(" "), digit1))(input)?;

        Ok((
            input,
            Self::new(
                source.parse().unwrap(),
                destination.parse().unwrap(),
                length.parse().unwrap(),
            ),
        ))
    }
}

#[derive(Debug)]
struct Almanac {
    seeds: Vec<isize>,
    mappings: Vec<Vec<Mapping>>,
}

impl Almanac {
    fn new(seeds: Vec<isize>, mappings: Vec<Vec<Mapping>>) -> Self {
        Self { seeds, mappings }
    }

    fn apply_seed(&self, seed: isize) -> isize {
        let mut value = seed;
        for mapping in &self.mappings {
            'range: for m in mapping {
                if m.contains(value) {
                    value = m.apply(value);
                    break 'range;
                }
            }
        }
        value
    }

    fn p1(&self) -> isize {
        self.seeds
            .iter()
            .map(|s| self.apply_seed(*s))
            .min()
            .unwrap()
    }

    fn p2(&self) -> isize {
        self.seeds
            .chunks(2)
            .map(|ss| self.helper(ss[0], ss[0] + ss[1], 0))
            .min()
            .unwrap()
    }

    fn helper(&self, start: isize, end: isize, depth: usize) -> isize {
        // We are at the end of the mappings, so it's just the lowerst
        // values (the start).
        if depth == self.mappings.len() {
            return start;
        }

        // If our range is below all the mappings, we don't have to do
        // anything at this depth.
        if start < self.mappings[depth][0].range.start && end < self.mappings[depth][0].range.end {
            return self.helper(start, end, depth + 1);
        }

        // Loop through all the mappings at this depth.
        for mapping in &self.mappings[depth] {
            if start < mapping.range.start {
                // If the start is below the mapping but the end is
                // not, break it into two pieces were we can recheck
                // the inclusive part but move on for the exclusive
                // part.
                return std::cmp::min(
                    // NOTE: We don't apply here because the end could
                    // be above of the range as well. When we recurse,
                    // the following else if blocks will handle it.
                    self.helper(mapping.range.start, end, depth),
                    self.helper(start, mapping.range.start - 1, depth + 1),
                );
            } else if start < mapping.range.end && end <= mapping.range.end {
                // In this case, the entire range is covered by this
                // mapping, so we can apply the mapping and go down
                // another level.
                return self.helper(mapping.apply(start), mapping.apply(end), depth + 1);
            } else if start < mapping.range.end {
                // This is like the second case, but the other end of
                // the range is encompassed in the mapping.
                return std::cmp::min(
                    self.helper(
                        mapping.apply(start),
                        mapping.apply(mapping.range.end),
                        depth + 1,
                    ),
                    self.helper(mapping.range.end, end, depth),
                );
            }
            // Othwerise, it's bigger than the range, so we move to
            // the next mapping.
        }

        // Final case, we didn't find a mapping, so the whole thing
        // moves to the next level.
        self.helper(start, end, depth + 1)
    }

    // 94120ms LOL and off by one?
    #[allow(dead_code)]
    fn p2_original(&self) -> isize {
        self.seeds
            .chunks(2)
            .map(|ss| {
                (ss[0]..ss[0] + ss[1])
                    .into_par_iter()
                    .map(|s| self.apply_seed(s))
                    .min()
                    .unwrap()
            })
            .min()
            .unwrap()
    }

    fn parse_mappings(input: &str) -> IResult<&str, Vec<Mapping>> {
        let (input, _) = tuple((take_until("\n"), tag("\n")))(input)?;
        let (input, mapping) = separated_list0(tag("\n"), Mapping::parse)(input)?;

        // Note: This is important for part 2.
        let mut mapping = mapping;
        mapping.sort_by(|a, b| a.range.start.cmp(&b.range.start));
        Ok((input, mapping))
    }

    fn parse(input: &str) -> IResult<&str, Self> {
        let (input, (_, seeds)) =
            tuple((tag("seeds: "), separated_list0(tag(" "), digit1)))(input)?;
        let seeds = seeds.iter().map(|s| s.parse().unwrap()).collect();
        let (input, _) = tag("\n\n")(input)?;
        let (input, mappings) = separated_list0(tag("\n\n"), Almanac::parse_mappings)(input)?;

        Ok((input, Self::new(seeds, mappings)))
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let (_, almanac) = Almanac::parse(&input).unwrap();
    println!("p1: {}", almanac.p1());

    let now = std::time::Instant::now();
    println!("p2: {}", almanac.p2());
    println!("took: {}ms", now.elapsed().as_millis());
}
