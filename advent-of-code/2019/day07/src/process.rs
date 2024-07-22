use anyhow::Result;

use crate::instruction::Instruction;
use crate::ipc::{ChannelReceiver, ChannelSender};
use crate::parameter::Parameter;

// Separate out the state of the process so we can easily pass it to the tui.
#[derive(Debug, Clone)]
pub struct State {
    pub memory: Vec<isize>,
    pub instruction_pointer: usize,
    pub last_output: Option<isize>,
    pub last_input: Option<isize>,
    pub halted: bool,
}

impl State {
    pub fn new(memory: Vec<isize>) -> Self {
        Self {
            memory,
            instruction_pointer: 0,
            last_output: None,
            last_input: None,
            halted: false,
        }
    }

    pub fn len(&self) -> usize {
        self.memory.len()
    }

    pub fn next_instruction(&self) -> Option<(Instruction, usize)> {
        if self.instruction_pointer >= self.len() || self.halted {
            return None;
        }

        let opcode = self[self.instruction_pointer];
        let op = opcode % 100;

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

        Some((instruction, instruction.parameter_count() + 1))
    }
}

impl std::ops::Index<usize> for State {
    type Output = isize;

    fn index(&self, index: usize) -> &Self::Output {
        &self.memory[index]
    }
}

impl std::ops::IndexMut<usize> for State {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.memory[index]
    }
}

pub struct Process {
    state: State,
    pub channel_receiver: ChannelReceiver,
    channel_sender: ChannelSender,
}

impl Process {
    pub fn state(&self) -> State {
        self.state.clone()
    }

    pub fn new_with_program_and_input(
        program: &str,
        channel_receiver: ChannelReceiver,
        channel_sender: ChannelSender,
    ) -> Self {
        Self {
            state: State::new(
                program
                    .trim()
                    .split(',')
                    .map(|s| s.parse::<isize>().unwrap())
                    .collect::<Vec<_>>(),
            ),
            channel_receiver,
            channel_sender,
        }
    }

    pub async fn run(&mut self) -> Result<()> {
        while !self.state().halted {
            self.step().await?;
        }
        Ok(())
    }

    pub async fn step(&mut self) -> Result<()> {
        if let Some((instruction, instruction_size)) = self.state.next_instruction() {
            match self.evaluate_instruction(instruction).await {
                Ok(true) => self.state.instruction_pointer += instruction_size,
                Ok(false) => (),
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }

    pub async fn evaluate_instruction(&mut self, instruction: Instruction) -> Result<bool> {
        if self.state.halted {
            return Ok(false);
        }

        macro_rules! eval {
            (write $dest:ident) => {
                let $dest = match $dest {
                   Parameter::Position(pos) => pos,
                   Parameter::Immediate(_) => panic!("invalid write parameter"),
                };
            };
            ($param:ident) => {
                let $param = match $param {
                    Parameter::Position(pos) => self.state[pos],
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

        match instruction {
            Instruction::Add(left, right, dest) => {
                eval! { write dest, left, right };
                self.state[dest] = left + right;
            }
            Instruction::Multiply(left, right, dest) => {
                eval! { write dest, left, right };
                self.state[dest] = left * right;
            }
            Instruction::LessThan(left, right, dest) => {
                eval! { write dest, left, right };
                self.state[dest] = match left < right {
                    true => 1,
                    false => 0,
                }
            }
            Instruction::Input(dest) => {
                eval! { write dest };
                self.state[dest] = match self.channel_receiver.recv().await {
                    Some(value) => value,
                    None => return Ok(false),
                };

                self.state.last_input = Some(self.state[dest]);
            }
            Instruction::Output(value) => {
                eval! { value };
                self.state.last_output = Some(value);
                match self.channel_sender.send(value).await {
                    Ok(_) => (),
                    Err(_) => return Ok(false),
                }
            }
            Instruction::JumpIfTrue(value, dest) => {
                eval! { value, dest };
                if value != 0 {
                    self.state.instruction_pointer = dest as usize;
                    // We don't want to update the instruction pointer.
                    return Ok(false);
                }
            }
            Instruction::JumpIfFalse(value, dest) => {
                eval! { value, dest };
                if value == 0 {
                    self.state.instruction_pointer = dest as usize;
                    // We don't want to update the instruction pointer.
                    return Ok(false);
                }
            }
            Instruction::Equals(left, right, dest) => {
                eval! { write dest, left, right };
                self.state[dest] = match left == right {
                    true => 1,
                    false => 0,
                }
            }
            Instruction::Halt => {
                self.state.halted = true;
            }
        };
        Ok(true)
    }
}
