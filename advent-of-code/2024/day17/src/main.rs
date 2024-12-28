use nom::{
    bytes::complete::tag, character::complete::digit1, combinator::map_res, multi::separated_list1,
    IResult,
};

#[derive(PartialEq)]
enum Register {
    A,
    B,
    C,
}

#[derive(Default, PartialEq)]
struct Registers {
    a: usize,
    b: usize,
    c: usize,
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

#[derive(PartialEq)]
enum Combo {
    Literal(usize),
    Register(Register),
}

impl Combo {
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

#[derive(PartialEq)]
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

#[derive(PartialEq)]
struct Computer {
    registers: Registers,
    instruction_pointer: usize,
    instructions: Vec<Instruction>,
    output: Vec<usize>,
}

impl Computer {
    fn new(input: &str) -> IResult<&str, (Computer, Vec<usize>)> {
        let (input, registers) = Registers::new(input)?;
        let (input, _) = tag("\r\n")(input)?;
        let (input, _) = tag("\r\nProgram: ")(input)?;
        let (input, instructions) =
            separated_list1(tag(","), map_res(digit1, |d: &str| d.parse::<usize>()))(input)?;
        let original_instructions = instructions.clone();
        let instructions = instructions
            .chunks(2)
            .map(|chunk| Instruction::new(chunk[0], chunk[1]))
            .collect();
        Ok((
            input,
            (
                Computer {
                    registers,
                    instruction_pointer: 0,
                    instructions,
                    output: Vec::new(),
                },
                original_instructions,
            ),
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
    let (_, (mut computer, program)) = Computer::new(input)?;
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

    match find_register(&program) {
        Some(register) => println!("p2: {}", register),
        None => println!("p2: no solution"),
    };

    Ok(())
}

fn simulate_loop(a: usize) -> usize {
    let mut b = a % 8;
    b = b ^ 3;
    let c = a >> b;
    b = b ^ c;
    b = b ^ 3;
    b % 8
}

fn find_register(program: &[usize]) -> Option<usize> {
    let mut stack = vec![(0, program.len())];
    let mut counter = 0;
    while let Some((a, depth)) = stack.pop() {
        counter += 1;
        // If we ever get to the beginning of the program, we have a solution.
        if depth == 0 {
            println!("counter: {}", counter);   
            return Some(a);
        }

        // Try all possible values for b and push ones that produce the correct
        // result on the stack.
        for b in 0..8 {
            let a = (a << 3) | b;
            if simulate_loop(a) == program[depth - 1] {
                stack.push((a, depth - 1));
            }
        }
    }

    None
}
