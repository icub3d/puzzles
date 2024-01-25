use nom::{
    character::complete::{alpha1, one_of, space1},
    multi::many0,
    sequence::tuple,
    IResult,
};

#[derive(Debug)]
enum Register {
    W,
    X,
    Y,
    Z,
}

impl Register {
    fn parse(input: &str) -> IResult<&str, Register> {
        let (input, register) = one_of("wxyz")(input)?;
        Ok((input, register.into()))
    }

    fn index(&self) -> usize {
        match self {
            Register::W => 0,
            Register::X => 1,
            Register::Y => 2,
            Register::Z => 3,
        }
    }
}

impl From<char> for Register {
    fn from(c: char) -> Self {
        match c {
            'w' => Register::W,
            'x' => Register::X,
            'y' => Register::Y,
            'z' => Register::Z,
            _ => panic!("Unknown register"),
        }
    }
}

#[derive(Debug)]
enum Value {
    Register(Register),
    Number(i64),
}

impl Value {
    fn parse(input: &str) -> IResult<&str, Value> {
        let (input, value) = many0(one_of("wxyz1234567890-"))(input)?;
        Ok((input, value.into()))
    }
}

impl From<Vec<char>> for Value {
    fn from(s: Vec<char>) -> Self {
        match s[0] {
            'w' => Value::Register(Register::W),
            'x' => Value::Register(Register::X),
            'y' => Value::Register(Register::Y),
            'z' => Value::Register(Register::Z),
            _ => Value::Number(s.iter().collect::<String>().parse().unwrap()),
        }
    }
}

#[derive(Debug)]
enum Instruction {
    Input(Register),
    Add(Register, Value),
    Mul(Register, Value),
    Div(Register, Value),
    Mod(Register, Value),
    Equal(Register, Value),
}

impl Instruction {
    fn parse(input: &str) -> IResult<&str, Instruction> {
        let (input, instruction) = alpha1(input)?;
        let (input, _) = space1(input)?;
        let (input, instruction) = match instruction {
            "inp" => one_of("wxyz")(input).map(|(i, r)| (i, Instruction::Input(r.into())))?,
            "add" => {
                let (input, (r, _, v)) = tuple((Register::parse, space1, Value::parse))(input)?;
                (input, Instruction::Add(r, v))
            }
            "mul" => {
                let (input, (r, _, v)) = tuple((Register::parse, space1, Value::parse))(input)?;
                (input, Instruction::Mul(r, v))
            }
            "div" => {
                let (input, (r, _, v)) = tuple((Register::parse, space1, Value::parse))(input)?;
                (input, Instruction::Div(r, v))
            }
            "mod" => {
                let (input, (r, _, v)) = tuple((Register::parse, space1, Value::parse))(input)?;
                (input, Instruction::Mod(r, v))
            }
            "eql" => {
                let (input, (r, _, v)) = tuple((Register::parse, space1, Value::parse))(input)?;
                (input, Instruction::Equal(r, v))
            }
            _ => panic!("Unknown instruction"),
        };
        Ok((input, instruction))
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let instructions = input
        .lines()
        .map(|line| Instruction::parse(line).unwrap().1)
        .collect::<Vec<_>>();

    // Get the values in the instructions that are actually
    // interesting for the problem space.
    let chunks = instructions.chunks(18);
    let mut increments = vec![];
    let mut decrements = vec![];
    for chunk in chunks {
        if let Instruction::Add(_, Value::Number(n)) = chunk[5] {
            if n < 0 {
                // This is our step down.
                decrements.push(Some(-n));
                increments.push(None);
                continue;
            }
        }
        decrements.push(None);
        if let Instruction::Add(_, Value::Number(n)) = chunk[15] {
            increments.push(Some(n));
        }
    }

    // We only need to check 7 digits, since 7 are "static" and can
    // always be 9.
    let mut cur = 9999999;
    while cur > 1000000 {
        // Get the digits of the current number.
        let digits = cur
            .to_string()
            .chars()
            .map(|c| c.to_digit(10).unwrap() as i64)
            .collect::<Vec<_>>();

        // Skip any that have a zero in them.
        if digits.iter().any(|d| *d == 0) {
            cur -= 1;
            continue;
        }
        // Check to see if we find a solution.
        if let Some(n) = find_digits(&increments, &decrements, &digits) {
            println!("p1: {}", n);
            break;
        }
        cur -= 1;
    }

    // For part two, we are going to do the same thing but start at 1111111.
    let mut cur = 1111111;
    while cur <= 9999999 {
        // Get the digits of the current number.
        let digits = cur
            .to_string()
            .chars()
            .map(|c| c.to_digit(10).unwrap() as i64)
            .collect::<Vec<_>>();

        // Skip any that have a zero in them.
        if digits.iter().any(|d| *d == 0) {
            cur += 1;
            continue;
        }
        // Check to see if we find a solution.
        if let Some(n) = find_digits(&increments, &decrements, &digits) {
            println!("p2: {}", n);
            break;
        }
        cur += 1;
    }
}

fn find_digits(
    increments: &[Option<i64>],
    decrements: &[Option<i64>],
    digits: &[i64],
) -> Option<i64> {
    let mut z = 0;
    let mut solution = [0; 14];
    let mut d = 0;
    for i in 0..14 {
        if let Some(n) = increments[i] {
            z = z * 26 + digits[d] + n;
            solution[i] = digits[d];
            d += 1;
        } else if let Some(n) = decrements[i] {
            solution[i] = (z % 26) - n;
            z /= 26;
            if solution[i] <= 0 || solution[i] > 9 {
                return None;
            }
        }
    }
    Some(solution.iter().fold(0, |acc, x| acc * 10 + x))
}

fn run(instructions: &[Instruction], inputs: &[i64]) -> Option<i64> {
    let mut registers = [0; 4];
    let mut cur_input = 0;
    for instruction in instructions {
        match instruction {
            Instruction::Input(r) => {
                registers[r.index()] = inputs[cur_input];
                cur_input += 1;
            }
            Instruction::Add(r, v) => match v {
                Value::Register(r2) => {
                    registers[r.index()] += registers[r2.index()];
                }
                Value::Number(n) => {
                    registers[r.index()] += n;
                }
            },
            Instruction::Mul(r, v) => match v {
                Value::Register(r2) => {
                    registers[r.index()] *= registers[r2.index()];
                }
                Value::Number(n) => {
                    registers[r.index()] *= n;
                }
            },
            Instruction::Div(r, v) => match v {
                Value::Register(r2) => {
                    if registers[r2.index()] == 0 {
                        return None;
                    }
                    registers[r.index()] /= registers[r2.index()];
                }
                Value::Number(n) => {
                    if *n == 0 {
                        return None;
                    }
                    registers[r.index()] /= n;
                }
            },
            Instruction::Mod(r, v) => match v {
                Value::Register(r2) => {
                    registers[r.index()] %= registers[r2.index()];
                }
                Value::Number(n) => {
                    registers[r.index()] %= n;
                }
            },
            Instruction::Equal(r, v) => match v {
                Value::Register(r2) => {
                    registers[r.index()] = if registers[r.index()] == registers[r2.index()] {
                        1
                    } else {
                        0
                    };
                }
                Value::Number(n) => {
                    registers[r.index()] = if registers[r.index()] == *n { 1 } else { 0 };
                }
            },
        }
    }
    Some(registers[Register::Z.index()])
}
