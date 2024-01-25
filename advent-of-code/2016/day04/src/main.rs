use nom::IResult;

#[derive(Debug)]
struct Room<'a> {
    name: &'a str,
    sector_id: usize,
    checksum: &'a str,
}

impl<'a> Room<'a> {
    fn parse(input: &'a str) -> IResult<&str, Room> {
        let (input, name) = nom::bytes::complete::take_while(|c: char| !c.is_ascii_digit())(input)?;
        let (input, sector_id) = nom::character::complete::digit1(input)?;
        let (input, _) = nom::bytes::complete::tag("[")(input)?;
        let (input, checksum) = nom::bytes::complete::take_while(|c| c != ']')(input)?;
        let (input, _) = nom::bytes::complete::tag("]")(input)?;
        let name = name.trim_end_matches('-');
        Ok((
            input,
            Room {
                name,
                sector_id: sector_id.parse().unwrap(),
                checksum,
            },
        ))
    }

    fn is_real(&self) -> usize {
        let mut counts = std::collections::HashMap::new();
        for c in self.name.chars() {
            if c == '-' {
                continue;
            }
            *counts.entry(c).or_insert(0) += 1;
        }
        let mut counts = counts.into_iter().collect::<Vec<_>>();
        counts.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
        let checksum = counts
            .into_iter()
            .take(5)
            .map(|(c, _)| c)
            .collect::<String>();
        if checksum == self.checksum {
            self.sector_id
        } else {
            0
        }
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let rooms = input
        .lines()
        .map(|line| Room::parse(line).unwrap().1)
        .collect::<Vec<_>>();
    let sum = rooms.iter().map(|room| room.is_real()).sum::<usize>();
    println!("p1: {}", sum);

    let real_rooms = rooms
        .into_iter()
        .filter(|room| room.is_real() != 0)
        .collect::<Vec<_>>();

    for room in real_rooms {
        let mut name = String::new();
        for c in room.name.chars() {
            if c == '-' {
                name.push(' ');
            } else {
                name.push(
                    (b'a' + (((c as u8 - b'a') as usize + room.sector_id) % 26) as u8) as char,
                );
            }
        }
        if name.contains("north") {
            println!("p2: {} {}", room.sector_id, name);
        }
    }
}
