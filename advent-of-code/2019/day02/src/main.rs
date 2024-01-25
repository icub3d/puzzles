fn main() {
    let input = include_str!("../input");

    // For part 1, we can simply run the program with the two given inputs.
    let mut computer = Computer::new_with_program(input);
    computer[1] = 12;
    computer[2] = 2;
    computer.run();
    println!("p1: {}", computer[0]);

    // For part 2, we are looking for a specific output. The numbers are small enough to brute
    // force, so we just look for the correct output.
    for noun in 0..=99 {
        for verb in 0..=99 {
            let mut computer = Computer::new_with_program(input);
            computer[1] = noun;
            computer[2] = verb;
            computer.run();
            if computer[0] == 19_690_720 {
                println!("p2: {}", 100 * noun + verb);
                return;
            }
        }
    }
}

// Create an enumeration of the possible instructions. For this first day, there are only a few but I
// image we'll be adding more in the future.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Instruction {
    Add(usize, usize, usize),
    Multiply(usize, usize, usize),
    Halt,
}

// A Computer struct to hold the memory and instruction pointer.
struct Computer {
    memory: Vec<isize>,
    instruction_pointer: usize,
}

// Implement indexing for the Computer struct. This simplifies access to the memory.
impl std::ops::Index<usize> for Computer {
    type Output = isize;

    fn index(&self, index: usize) -> &Self::Output {
        &self.memory[index]
    }
}

// Do the same but for mutable indexing.
impl std::ops::IndexMut<usize> for Computer {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.memory[index]
    }
}

impl Computer {
    // Create a new computer and load up the given program.
    fn new_with_program(input: &str) -> Self {
        Self {
            memory: input
                .trim()
                .split(',')
                .map(|s| s.parse::<isize>().unwrap())
                .collect::<Vec<_>>(),
            instruction_pointer: 0,
        }
    }

    // Run the program. We can simply keep asking for operations until we get a halt.
    fn run(&mut self) {
        while let Some(instruction) = self.next_instruction() {
            match instruction {
                Instruction::Add(left, right, dest) => self[dest] = self[left] + self[right],
                Instruction::Multiply(left, right, dest) => self[dest] = self[left] * self[right],
                Instruction::Halt => break,
            }
        }
    }

    // Get the next instruction from the memory and update the instruction pointer.
    fn next_instruction(&mut self) -> Option<Instruction> {
        // If we are at the end of the memory, return None.
        if self.instruction_pointer >= self.memory.len() {
            return None;
        }

        // Otherwise, get the next instruction.
        let instruction = match self[self.instruction_pointer] {
            1 => Instruction::Add(
                self[self.instruction_pointer + 1] as usize,
                self[self.instruction_pointer + 2] as usize,
                self[self.instruction_pointer + 3] as usize,
            ),
            2 => Instruction::Multiply(
                self[self.instruction_pointer + 1] as usize,
                self[self.instruction_pointer + 2] as usize,
                self[self.instruction_pointer + 3] as usize,
            ),
            99 => Instruction::Halt,
            _ => panic!("Invalid opcode"),
        };

        // Update the instruction pointer and return our operation.
        self.instruction_pointer += 4;
        Some(instruction)
    }
}
