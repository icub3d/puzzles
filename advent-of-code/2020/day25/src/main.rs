use std::fs;

use mod_exp::mod_exp;

fn transform(subject_number: usize, loop_size: usize) -> usize {
    // This is actually a modular exponentiation after some research.
    mod_exp(subject_number, loop_size, 20201227)
}

fn main() {
    let input = fs::read_to_string("input").unwrap();
    let inputs = input.split('\n').collect::<Vec<&str>>().clone();
    let first = inputs[0].parse::<usize>().unwrap();
    let second = inputs[1].parse::<usize>().unwrap();

    // Figure out he two loop sizes.
    let mut first_loop_size = 1;
    loop {
        let t = transform(7, first_loop_size);
        if t == first {
            break;
        }
        first_loop_size += 1;
    }
    println!("{}", first_loop_size);

    let mut second_loop_size = 1;
    loop {
        let t = transform(7, second_loop_size);
        if t == second {
            break;
        }
        second_loop_size += 1;
    }
    println!("{}", second_loop_size);

    println!("encryption key: {}", transform(first, second_loop_size));
}
