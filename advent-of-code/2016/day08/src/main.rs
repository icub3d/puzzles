use nom::{
    branch::alt, bytes::complete::tag, character::complete::digit1, sequence::tuple, IResult,
};

#[derive(Debug)]
enum Instruction {
    Rectangle(u32, u32),
    RotateRow(u32, u32),
    RotateColumn(u32, u32),
}

impl Instruction {
    fn parse_rect(input: &str) -> IResult<&str, Instruction> {
        let (input, (_, x, _, y)) = tuple((tag("rect "), digit1, tag("x"), digit1))(input)?;
        Ok((
            input,
            Instruction::Rectangle(x.parse().unwrap(), y.parse().unwrap()),
        ))
    }

    fn rotate_row(input: &str) -> IResult<&str, Instruction> {
        let (input, (_, y, _, by)) =
            tuple((tag("rotate row y="), digit1, tag(" by "), digit1))(input)?;
        Ok((
            input,
            Instruction::RotateRow(y.parse().unwrap(), by.parse().unwrap()),
        ))
    }

    fn rotate_column(input: &str) -> IResult<&str, Instruction> {
        let (input, (_, x, _, by)) =
            tuple((tag("rotate column x="), digit1, tag(" by "), digit1))(input)?;
        Ok((
            input,
            Instruction::RotateColumn(x.parse().unwrap(), by.parse().unwrap()),
        ))
    }

    fn parse(input: &str) -> IResult<&str, Instruction> {
        alt((
            Instruction::parse_rect,
            Instruction::rotate_row,
            Instruction::rotate_column,
        ))(input)
    }

    fn apply(&self, screen: &mut [Vec<bool>]) {
        match self {
            Instruction::Rectangle(x, y) => {
                for i in 0..*y {
                    for j in 0..*x {
                        screen[i as usize][j as usize] = true;
                    }
                }
            }
            Instruction::RotateRow(y, by) => {
                let mut row = screen[*y as usize].clone();
                let r = row.len();
                for i in 0..r {
                    row[(i + *by as usize) % r] = screen[*y as usize][i];
                }
                screen[*y as usize] = row;
            }
            Instruction::RotateColumn(x, by) => {
                let mut column = screen
                    .iter()
                    .map(|row| row[*x as usize])
                    .collect::<Vec<_>>();
                let c = column.len();
                for i in 0..c {
                    column[(i + *by as usize) % c] = screen[i][*x as usize];
                }
                for i in 0..column.len() {
                    screen[i][*x as usize] = column[i];
                }
            }
        }
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let instructions = input
        .lines()
        .map(|line| Instruction::parse(line).unwrap().1)
        .collect::<Vec<_>>();
    let mut screen = vec![vec![false; 50]; 6];
    for instruction in instructions {
        instruction.apply(&mut screen);
    }
    let lit = screen
        .iter()
        .map(|row| row.iter().filter(|&&x| x).count())
        .sum::<usize>();
    println!("Part 1: {}", lit);
    println!("Part 2:");
    for row in screen {
        for pixel in row {
            print!("{}", if pixel { '#' } else { ' ' });
        }
        println!();
    }
}
