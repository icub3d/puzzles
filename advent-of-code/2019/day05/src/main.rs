use std::collections::VecDeque;

fn main() {
    let input = include_str!("../input");

    // Part 1 - Once we've added the given features to our Intcode computer, we can simply run it.
    // We'll want to make sure we get all zeros except for the last value. Then the last value is
    // the answer.
    let mut computer = Computer::new_with_program_and_input(input, &[1]);
    computer.run();
    assert!(computer
        .output
        .iter()
        .take(computer.output.len() - 1)
        .all(|&x| x == 0));
    println!("p1: {}", computer.output.last().unwrap());

    // Part 2 - We can do the same thing as part 1 but with a different input.
    let mut computer = Computer::new_with_program_and_input(input, &[5]);
    computer.run();
    println!("p2: {}", computer.output.first().unwrap());
}

// We add a parameter enum to represent the different parameter modes. We can then use this to
// determine how to interpret the values in the memory.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Parameter {
    Position(usize),
    Immediate(isize),
}

impl Parameter {
    // Create a constructor for the parameter that will do the magic math to determine the mode for
    // the given position.
    fn new(opcode: isize, position: isize, value: isize) -> Self {
        let mode = (opcode / 10_isize.pow(position as u32 + 1)) % 10;
        match mode {
            0 => Self::Position(value as usize),
            1 => Self::Immediate(value),
            _ => panic!("Invalid parameter mode"),
        }
    }
}

// Add our new commands to our instructions. All of them take in Parameter's now.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Instruction {
    Add(Parameter, Parameter, Parameter),
    Multiply(Parameter, Parameter, Parameter),
    Input(Parameter),
    Output(Parameter),
    JumpIfTrue(Parameter, Parameter),
    JumpIfFalse(Parameter, Parameter),
    LessThan(Parameter, Parameter, Parameter),
    Equals(Parameter, Parameter, Parameter),
    Halt,
}

// We include values for input and output. I chose a VecDeque to simplify popping from the front.
struct Computer {
    memory: Vec<isize>,
    instruction_pointer: usize,
    input: VecDeque<isize>,
    output: Vec<isize>,
}

impl std::ops::Index<usize> for Computer {
    type Output = isize;

    fn index(&self, index: usize) -> &Self::Output {
        &self.memory[index]
    }
}

impl std::ops::IndexMut<usize> for Computer {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.memory[index]
    }
}

impl Computer {
    // We add a new constructor that takes in a program and input.
    fn new_with_program_and_input(program: &str, input: &[isize]) -> Self {
        Self {
            memory: program
                .trim()
                .split(',')
                .map(|s| s.parse::<isize>().unwrap())
                .collect::<Vec<_>>(),
            instruction_pointer: 0,
            input: input.iter().copied().collect(),
            output: Vec::new(),
        }
    }

    // We add our new instructions and then use some new parameter evaluation functions to help us
    // get the actual values we need.
    fn run(&mut self) {
        // I'm going to try to use a macro here for parameter evaluation. I'm really curious what
        // you think about the readability of this. It can feel a bit like magic, but it makes the
        // code below seem more concise. Though maybe that isn't always the goal of writing code?
        macro_rules! eval {
            (write $dest:ident) => {
                let $dest = match $dest {
                   Parameter::Position(pos) => pos,
                   Parameter::Immediate(_) => panic!("invalid write parameter"),
                };
            };
            ($param:ident) => {
                let $param = match $param {
                    Parameter::Position(pos) => self[pos],
                    Parameter::Immediate(value) => value,
                };
            };
            ($param:ident, $($params:ident),+) => {
                eval! { $param }
                eval! { $($params),+ }
            };
            (write $dest:ident, $($params:ident),+) => {
                eval! { write $dest }
                eval! { $($params),+ }
            };
        }

        while let Some(instruction) = self.next_instruction() {
            match instruction {
                Instruction::Add(left, right, dest) => {
                    // Use the macro to evaluate the parameters.
                    eval! { write dest, left, right };
                    self[dest] = left + right;
                }
                Instruction::Multiply(left, right, dest) => {
                    // Try this one without the macro to compare.
                    let dest = match dest {
                        Parameter::Position(pos) => pos,
                        Parameter::Immediate(_) => panic!("invalid write parameter"),
                    };
                    let left = match left {
                        Parameter::Position(pos) => self[pos],
                        Parameter::Immediate(value) => value,
                    };
                    let right = match right {
                        Parameter::Position(pos) => self[pos],
                        Parameter::Immediate(value) => value,
                    };
                    self[dest] = left * right;
                }
                Instruction::LessThan(left, right, dest) => {
                    // Now try it with a method helper.
                    let dest = self.eval_write(dest);
                    let left = self.eval(left);
                    let right = self.eval(right);
                    self[dest] = match left < right {
                        true => 1,
                        false => 0,
                    }
                }
                Instruction::Input(dest) => {
                    eval! { write dest };
                    self[dest] = self.input.pop_front().unwrap();
                }
                Instruction::Output(value) => {
                    eval! { value };
                    self.output.push(value);
                }
                Instruction::JumpIfTrue(value, dest) => {
                    eval! { value, dest };
                    if value != 0 {
                        self.instruction_pointer = dest as usize;
                    }
                }
                Instruction::JumpIfFalse(value, dest) => {
                    eval! { value, dest };
                    if value == 0 {
                        self.instruction_pointer = dest as usize;
                    }
                }
                Instruction::Equals(left, right, dest) => {
                    eval! { write dest, left, right };
                    self[dest] = match left == right {
                        true => 1,
                        false => 0,
                    }
                }
                Instruction::Halt => break,
            }
        }
    }

    fn eval_write(&self, param: Parameter) -> usize {
        match param {
            Parameter::Position(pos) => pos,
            Parameter::Immediate(_) => panic!("invalid write parameter"),
        }
    }

    fn eval(&self, param: Parameter) -> isize {
        match param {
            Parameter::Position(pos) => self[pos],
            Parameter::Immediate(value) => value,
        }
    }

    fn next_instruction(&mut self) -> Option<Instruction> {
        if self.instruction_pointer >= self.memory.len() {
            return None;
        }

        // Our operation is now the first 2 digits of the opcode. We also create Parameter's for
        // each of the parameters to the instruction.
        let opcode = self[self.instruction_pointer];
        let op = opcode % 100;

        // What do you think about using a macro for something like this?
        macro_rules! param {
            ($instruction:expr, 3) => {
                $instruction(
                    Parameter::new(opcode, 1, self[self.instruction_pointer + 1]),
                    Parameter::new(opcode, 2, self[self.instruction_pointer + 2]),
                    Parameter::new(opcode, 3, self[self.instruction_pointer + 3]),
                )
            };
            ($instruction:expr, 2) => {
                $instruction(
                    Parameter::new(opcode, 1, self[self.instruction_pointer + 1]),
                    Parameter::new(opcode, 2, self[self.instruction_pointer + 2]),
                )
            };
            ($instruction:expr, 1) => {
                $instruction(Parameter::new(
                    opcode,
                    1,
                    self[self.instruction_pointer + 1],
                ))
            };
            ($instruction:expr) => {
                $instruction
            };
        }

        let instruction = match op {
            1 => param!(Instruction::Add, 3),
            2 => param!(Instruction::Multiply, 3),
            3 => param!(Instruction::Input, 1),
            4 => param!(Instruction::Output, 1),
            5 => param!(Instruction::JumpIfTrue, 2),
            6 => param!(Instruction::JumpIfFalse, 2),
            7 => param!(Instruction::LessThan, 3),
            8 => param!(Instruction::Equals, 3),
            99 => Instruction::Halt,
            _ => panic!("invalid opcode"),
        };

        // Update the instruction pointer and return our operation.
        self.instruction_pointer += match op {
            1 | 2 | 7 | 8 => 4,
            3 | 4 => 2,
            5 | 6 => 3,
            99 => 1,
            _ => panic!("invalid opcode"),
        };
        Some(instruction)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parameter_new() {
        // Does the math check out?
        assert_eq!(Parameter::new(1002, 1, 4), Parameter::Position(4));
        assert_eq!(Parameter::new(1002, 2, 3), Parameter::Immediate(3));
        assert_eq!(Parameter::new(1002, 3, 2), Parameter::Position(2));
    }
}
