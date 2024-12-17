use std::fmt::Display;

use nom::{
    bytes::complete::tag, character::complete::digit1, combinator::map_res, multi::separated_list1,
    IResult,
};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use z3::ast::BV;

#[derive(Debug, PartialEq)]
enum Register {
    A,
    B,
    C,
}

#[derive(Debug, Default, PartialEq)]
struct Registers {
    a: usize,
    b: usize,
    c: usize,
}

impl Display for Registers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "A: {}, B: {}, C: {}", self.a, self.b, self.c)
    }
}

impl Registers {
    fn new(input: &str) -> IResult<&str, Self> {
        let (input, _) = tag("Register A: ")(input)?;
        let (input, a) = map_res(digit1, |d: &str| d.parse::<usize>())(input)?;
        let (input, _) = tag("\r\nRegister B: ")(input)?;
        let (input, b) = map_res(digit1, |d: &str| d.parse::<usize>())(input)?;
        let (input, _) = tag("\r\nRegister C: ")(input)?;
        let (input, c) = map_res(digit1, |d: &str| d.parse::<usize>())(input)?;
        Ok((input, Registers { a, b, c }))
    }

    fn get(&self, register: &Register) -> usize {
        match register {
            Register::A => self.a,
            Register::B => self.b,
            Register::C => self.c,
        }
    }

    fn set(&mut self, register: Register, value: usize) {
        match register {
            Register::A => self.a = value,
            Register::B => self.b = value,
            Register::C => self.c = value,
        }
    }
}

impl From<usize> for Register {
    fn from(value: usize) -> Self {
        match value {
            0 => Register::A,
            1 => Register::B,
            2 => Register::C,
            _ => panic!("Invalid register"),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Combo {
    Literal(usize),
    Register(Register),
}

impl Display for Combo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Combo::Literal(literal) => write!(f, "{}", literal),
            Combo::Register(register) => write!(f, "{:?}", register),
        }
    }
}

impl Combo {
    fn code(&self) -> usize {
        match self {
            Combo::Literal(l) => *l,
            Combo::Register(register) => match register {
                Register::A => 4,
                Register::B => 5,
                Register::C => 6,
            },
        }
    }

    fn new(op: usize) -> Self {
        match op {
            0 | 1 | 2 | 3 => Combo::Literal(op),
            4 => Combo::Register(Register::A),
            5 => Combo::Register(Register::B),
            6 => Combo::Register(Register::C),
            _ => panic!("Invalid op code"),
        }
    }

    fn eval(&self, registers: &Registers) -> usize {
        match self {
            Combo::Literal(literal) => *literal,
            Combo::Register(register) => registers.get(register),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Instruction {
    Adv(Combo), // A = A / (2 ^ Op) - truncate to int
    Bxl(usize), // B = B XOR Op
    Bst(Combo), // B = Op % 8
    Jnz(usize), // if a != 0 instruction_pointer = Op and don't increment
    Bxc(usize), // B = B XOR C
    Out(Combo), // output Combo % 8
    Bdv(Combo), // B = A / (2 ^ Op) - truncate to int
    Cdv(Combo), // C = A / (2 ^ Op) - truncate to int
}

impl Instruction {
    fn codes(&self) -> Vec<usize> {
        match self {
            Instruction::Adv(combo) => vec![0, combo.code()],
            Instruction::Bxl(op) => vec![1, *op],
            Instruction::Bst(combo) => vec![2, combo.code()],
            Instruction::Jnz(op) => vec![3, *op],
            Instruction::Bxc(op) => vec![4, *op],
            Instruction::Out(combo) => vec![5, combo.code()],
            Instruction::Bdv(combo) => vec![6, combo.code()],
            Instruction::Cdv(combo) => vec![7, combo.code()],
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Adv(combo) => write!(f, "Adv({})", combo),
            Instruction::Bxl(op) => write!(f, "Bxl({})", op),
            Instruction::Bst(combo) => write!(f, "Bst({})", combo),
            Instruction::Jnz(op) => write!(f, "Jnz({})", op),
            Instruction::Bxc(op) => write!(f, "Bxc({})", op),
            Instruction::Out(combo) => write!(f, "Out({})", combo),
            Instruction::Bdv(combo) => write!(f, "Bdv({})", combo),
            Instruction::Cdv(combo) => write!(f, "Cdv({})", combo),
        }
    }
}

impl Instruction {
    fn new(op: usize, a: usize) -> Self {
        match op {
            0 => Instruction::Adv(Combo::new(a)),
            1 => Instruction::Bxl(a),
            2 => Instruction::Bst(Combo::new(a)),
            3 => Instruction::Jnz(a),
            4 => Instruction::Bxc(a),
            5 => Instruction::Out(Combo::new(a)),
            6 => Instruction::Bdv(Combo::new(a)),
            7 => Instruction::Cdv(Combo::new(a)),
            _ => panic!("Invalid op code"),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Computer {
    registers: Registers,
    instruction_pointer: usize,
    instructions: Vec<Instruction>,
    output: Vec<usize>,
}

impl Display for Computer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.registers)?;
        writeln!(f, "Instruction Pointer: {}", self.instruction_pointer)?;
        writeln!(f, "Instructions:")?;
        for (i, instruction) in self.instructions.iter().enumerate() {
            writeln!(f, "  {}: {}", i, instruction)?;
        }
        writeln!(f, "Output: {:?}", self.output)
    }
}

impl Computer {
    fn program(&self) -> Vec<usize> {
        self.instructions.iter().flat_map(|i| i.codes()).collect()
    }

    fn new(input: &str) -> IResult<&str, Computer> {
        let (input, registers) = Registers::new(input)?;
        let (input, _) = tag("\r\n")(input)?;
        let (input, _) = tag("\r\nProgram: ")(input)?;
        let (input, instructions) =
            separated_list1(tag(","), map_res(digit1, |d: &str| d.parse::<usize>()))(input)?;
        let instructions = instructions
            .chunks(2)
            .map(|chunk| Instruction::new(chunk[0], chunk[1]))
            .collect();
        Ok((
            input,
            Computer {
                registers,
                instruction_pointer: 0,
                instructions,
                output: Vec::new(),
            },
        ))
    }

    fn run(&mut self) {
        loop {
            let instruction = &self.instructions[self.instruction_pointer];
            match instruction {
                Instruction::Adv(combo) => {
                    self.registers.set(
                        Register::A,
                        self.registers.get(&Register::A)
                            / (2usize.pow(combo.eval(&self.registers) as u32)),
                    );
                }
                Instruction::Bxl(op) => {
                    self.registers
                        .set(Register::B, self.registers.get(&Register::B) ^ op);
                }
                Instruction::Bst(combo) => {
                    self.registers
                        .set(Register::B, combo.eval(&self.registers) % 8);
                }
                Instruction::Jnz(op) => {
                    if self.registers.get(&Register::A) != 0 {
                        self.instruction_pointer = *op;
                        continue;
                    }
                }
                Instruction::Bxc(_) => {
                    self.registers.set(
                        Register::B,
                        self.registers.get(&Register::B) ^ self.registers.get(&Register::C),
                    );
                }
                Instruction::Out(combo) => {
                    self.output.push(combo.eval(&self.registers) % 8);
                }
                Instruction::Bdv(combo) => {
                    self.registers.set(
                        Register::B,
                        self.registers.get(&Register::A)
                            / (2usize.pow(combo.eval(&self.registers) as u32)),
                    );
                }
                Instruction::Cdv(combo) => {
                    self.registers.set(
                        Register::C,
                        self.registers.get(&Register::A)
                            / (2usize.pow(combo.eval(&self.registers) as u32)),
                    );
                }
            }
            self.instruction_pointer += 1;
            if self.instruction_pointer >= self.instructions.len() {
                break;
            }
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = include_str!("input.txt");
    let (_, mut computer) = Computer::new(input)?;
    let program = computer.program();
    computer.run();

    println!(
        "p1: {}",
        computer
            .output
            .iter()
            .map(|o| o.to_string())
            .collect::<Vec<String>>()
            .join(",")
    );

    // 35_184_372_088_832 - min
    // 281_474_976_710_656 - max
    let (_, mut computer) = Computer::new(input)?;
    computer.registers.a = 35_184_372_088_832;
    computer.run();
    assert!(computer.program().len() == 16);
    println!("a-min: {}", 35_184_372_088_832usize);

    let context = z3::Context::new(&z3::Config::new());
    let solver = z3::Optimize::new(&context);
    let s = BV::new_const(&context, "s", 64);
    let (mut a, mut b, mut c) = (
        s.clone(),
        BV::from_usize(&context, 0, 64),
        BV::from_usize(&context, 0, 64),
    );
    for x in program {
        b = a.bvsmod(&BV::from_usize(&context, 8, 64));
        b = b ^ &BV::from_usize(&context, program[3], 64);
        c = a.bvsdiv(&(BV::from_usize(&context, 1, 64) << &b));
        b = b ^ c;
        b = b ^ &BV::from_usize(&context, program[11], 64);
        a = a.bvsdiv(&BV::from_usize(&context, 8, 64));
        solver.assert(&(&b.bvsmod(&BV::from_usize(&ctx, 8, 64)))._eq(&BV::from_usize(&ctx, x, 64)));
    }
    solvers.assert(&a._eq(&BV::from_usize(&ctx, 0, 64)));
    solver.minimize(&s);
    match solver.check(&[]) {
        z3::SatResult::Sat => {
            let model = solver.get_model();
            println!("p2: {}", model)
        }
        _ => println!("p2: no solution"),
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_registers() {
        let input = "Register A: 1\r\nRegister B: 2\r\nRegister C: 3";
        let (_, registers) = Registers::new(input).unwrap();
        assert_eq!(registers.a, 1);
        assert_eq!(registers.b, 2);
        assert_eq!(registers.c, 3);
    }

    #[test]
    fn test_combo() {
        let registers = Registers::default();
        let combo = Combo::new(3);
        assert_eq!(combo.eval(&registers), 3);
    }

    #[test]
    fn test_instruction() {
        let instruction = Instruction::new(0, 3);
        assert_eq!(instruction, Instruction::Adv(Combo::Literal(3)));
    }

    #[test]
    fn test_runs() {
        // If register C contains 9, the program 2,6 would set register B to 1.
        let mut computer = Computer {
            registers: Registers { a: 0, b: 0, c: 9 },
            instruction_pointer: 0,
            instructions: vec![Instruction::Bst(Combo::Register(Register::C))],
            output: Vec::new(),
        };
        computer.run();
        assert_eq!(computer.registers.b, 1);

        // If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2.
        let mut computer = Computer {
            registers: Registers { a: 10, b: 0, c: 0 },
            instruction_pointer: 0,
            instructions: vec![
                Instruction::Out(Combo::new(0)),
                Instruction::Out(Combo::new(1)),
                Instruction::Out(Combo::new(4)),
            ],
            output: Vec::new(),
        };
        computer.run();
        assert_eq!(computer.output, vec![0, 1, 2]);

        // If register A contains 2024, the program 0,1,5,4,3,0 would output 4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.
        let mut computer = Computer {
            registers: Registers {
                a: 2024,
                b: 0,
                c: 0,
            },
            instruction_pointer: 0,
            instructions: vec![
                Instruction::Adv(Combo::new(1)),
                Instruction::Out(Combo::new(4)),
                Instruction::Jnz(0),
            ],
            output: Vec::new(),
        };
        computer.run();
        assert_eq!(computer.output, vec![4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0]);
        assert_eq!(computer.registers.a, 0);

        // If register B contains 29, the program 1,7 would set register B to 26.
        let mut computer = Computer {
            registers: Registers { a: 0, b: 29, c: 0 },
            instruction_pointer: 0,
            instructions: vec![Instruction::Bxl(7)],
            output: Vec::new(),
        };
        computer.run();
        assert_eq!(computer.registers.b, 26);

        // If register B contains 2024 and register C contains 43690, the program 4,0 would set register B to 44354.
        let mut computer = Computer {
            registers: Registers {
                a: 0,
                b: 2024,
                c: 43690,
            },
            instruction_pointer: 0,
            instructions: vec![Instruction::Bxc(0)],
            output: Vec::new(),
        };
        computer.run();
        assert_eq!(computer.registers.b, 44354);
    }
}
