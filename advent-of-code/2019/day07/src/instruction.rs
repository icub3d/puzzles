use std::fmt::Display;

use crate::parameter::Parameter;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Instruction {
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

impl Instruction {
    pub fn parameter_count(&self) -> usize {
        // Get the number of parameters for a given instruction. This will be used by the tui to
        // highlight the parameters.
        match self {
            Instruction::Add(_, _, _) => 3,
            Instruction::Multiply(_, _, _) => 3,
            Instruction::Input(_) => 1,
            Instruction::Output(_) => 1,
            Instruction::JumpIfTrue(_, _) => 2,
            Instruction::JumpIfFalse(_, _) => 2,
            Instruction::LessThan(_, _, _) => 3,
            Instruction::Equals(_, _, _) => 3,
            Instruction::Halt => 0,
        }
    }

    pub fn position_parameters(&self) -> Vec<usize> {
        // Get the parameters in position mode for a given instruction. This will be used by the tui
        // to highlight the memory locations that are being read from or written to.
        let mut positions = Vec::new();
        macro_rules! add_positions {
            ($param:ident) => {
                if let Parameter::Position(pos) = $param {
                    positions.push(*pos);
                }
            };
            ($param:ident, $($params:ident),+) => {
                add_positions! { $param }
                add_positions! { $($params),+ }
            };
        }
        match self {
            Instruction::Add(left, right, dest) => {
                add_positions! { left, right, dest }
            }
            Instruction::Multiply(left, right, dest) => {
                add_positions! { left, right, dest }
            }
            Instruction::Input(dest) => {
                add_positions! { dest }
            }
            Instruction::Output(value) => {
                add_positions! { value }
            }
            Instruction::JumpIfTrue(value, dest) => {
                add_positions! { value, dest }
            }
            Instruction::JumpIfFalse(value, dest) => {
                add_positions! { value, dest }
            }
            Instruction::LessThan(left, right, dest) => {
                add_positions! { left, right, dest }
            }
            Instruction::Equals(left, right, dest) => {
                add_positions! { left, right, dest }
            }
            Instruction::Halt => {}
        }
        positions
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Display an instruction in a human-readable format. This will be used by the tui to
        // display the current instruction.
        match self {
            Instruction::Add(left, right, dest) => {
                write!(f, "ADD {} + {} -> {}", left, right, dest)
            }
            Instruction::Multiply(left, right, dest) => {
                write!(f, "MUL {} * {} -> {}", left, right, dest)
            }
            Instruction::Input(dest) => write!(f, "INP -> {}", dest),
            Instruction::Output(value) => write!(f, "OUT -> {}", value),
            Instruction::JumpIfTrue(value, dest) => write!(f, "JIT {} -> {}", value, dest),
            Instruction::JumpIfFalse(value, dest) => write!(f, "JIF {} -> {}", value, dest),
            Instruction::LessThan(left, right, dest) => {
                write!(f, "LST {} < {} -> {}", left, right, dest)
            }
            Instruction::Equals(left, right, dest) => {
                write!(f, "EQL {} == {} -> {}", left, right, dest)
            }
            Instruction::Halt => write!(f, "HLT"),
        }
    }
}
