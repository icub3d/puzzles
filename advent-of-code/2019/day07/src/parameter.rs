use std::fmt::Display;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Parameter {
    Position(usize),
    Immediate(isize),
}

impl Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Parameter::Position(pos) => write!(f, "P[{}]", pos),
            Parameter::Immediate(value) => write!(f, "I[{}]", value),
        }
    }
}

impl Parameter {
    pub fn new(opcode: isize, position: isize, value: isize) -> Self {
        let mode = (opcode / 10_isize.pow(position as u32 + 1)) % 10;
        match mode {
            0 => Self::Position(value as usize),
            1 => Self::Immediate(value),
            _ => panic!("Invalid parameter mode"),
        }
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
