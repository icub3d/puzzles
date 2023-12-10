use nom::{
    bytes::complete::tag,
    character::complete::{digit1, space1},
    multi::separated_list0,
    sequence::tuple,
    IResult,
};

#[derive(Debug)]
struct Race {
    time: usize,
    distance: usize,
}

impl Race {
    fn new(time: usize, distance: usize) -> Self {
        Self { time, distance }
    }

    fn winners(&self) -> usize {
        let mut winners = 0;
        for i in 0..self.time {
            let distance = (self.time - i) * i;
            if distance > self.distance {
                winners += 1;
            }
        }
        winners
    }

    fn winners_faster(&self, counter: &mut usize) -> usize {
        let mut begin = 0;
        for i in 0..self.time {
            *counter += 1;
            let distance = (self.time - i) * i;
            if distance > self.distance {
                begin = i;
                break;
            }
        }

        let mut end = 0;
        for i in (begin..self.time).rev() {
            *counter += 1;
            let distance = (self.time - i) * i;
            if distance > self.distance {
                end = i;
                break;
            }
        }

        end - begin + 1
    }
}

fn parse_p2(input: &str) -> IResult<&str, Race> {
    let (input, (_, time, _)) = tuple((tag("Time:"), digit1, tag("\n")))(input)?;
    let (input, (_, distance)) = tuple((tag("Distance:"), digit1))(input)?;

    Ok((
        input,
        Race::new(
            time.parse::<usize>().unwrap(),
            distance.parse::<usize>().unwrap(),
        ),
    ))
}

fn parse(input: &str) -> IResult<&str, Vec<Race>> {
    let (input, (_, _, times, _)) = tuple((
        tag("Time:"),
        space1,
        separated_list0(space1, digit1),
        tag("\n"),
    ))(input)?;
    let (input, (_, _, distances)) =
        tuple((tag("Distance:"), space1, separated_list0(space1, digit1)))(input)?;

    Ok((
        input,
        times
            .iter()
            .map(|s| s.parse::<usize>().unwrap())
            .zip(distances.iter().map(|s| s.parse::<usize>().unwrap()))
            .map(|(time, distance)| Race::new(time, distance))
            .collect(),
    ))
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let races = parse(&input).unwrap().1;

    // Part 1
    let now = std::time::Instant::now();
    let p1 = races.iter().map(|r| r.winners()).product::<usize>();
    println!(
        "p1: {} ({}) ({:?}))",
        p1,
        races.iter().map(|r| r.time).sum::<usize>(),
        now.elapsed()
    );
    let now = std::time::Instant::now();
    let mut counter = 0;
    let p1_faster = races
        .iter()
        .map(|r| r.winners_faster(&mut counter))
        .product::<usize>();
    println!(
        "p1_faster: {} ({}) ({:?})",
        p1_faster,
        counter,
        now.elapsed()
    );

    // Part 2
    let p2_input = str::replace(&input, " ", "");
    let race = parse_p2(&p2_input).unwrap().1;
    let now = std::time::Instant::now();
    let p2 = race.winners();
    println!("p2: {} ({}) ({:?})", p2, race.time, now.elapsed());
    let now = std::time::Instant::now();
    let mut counter = 0;
    let p2_faster = race.winners_faster(&mut counter);
    println!(
        "p2_faster: {} ({}) ({:?})",
        p2_faster,
        counter,
        now.elapsed()
    );
}
