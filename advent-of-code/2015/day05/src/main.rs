use std::collections::HashSet;

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let lines: Vec<&str> = input.lines().collect();
    let mut nice = 0;

    // Part 1
    let vowels: HashSet<char> = "aeiou".chars().collect();
    for line in lines.iter() {
        let chars = line.chars().collect::<Vec<char>>();
        // We'll always check the right in the windows so we need to check if the first is vowel.
        // We can't check both in the windows because it's sliding so we might double count.
        let mut vowel_count = match vowels.contains(&chars[0]) {
            true => 1,
            false => 0,
        };
        let mut double_found = false;
        let mut bad = false;
        chars.windows(2).for_each(|w| {
            if vowels.contains(&w[1]) {
                vowel_count += 1;
            }
            if &w[0] == &w[1] {
                double_found = true;
            }
            match (&w[0], &w[1]) {
                ('a', 'b') | ('c', 'd') | ('p', 'q') | ('x', 'y') => {
                    bad = true;
                }
                _ => (),
            };
        });
        if vowel_count >= 3 && double_found && !bad {
            nice += 1;
        }
    }
    println!("Part 1: {}", nice);

    // Part 2
    nice = 0;
    for line in lines.iter() {
        let cc = line.chars().collect::<Vec<char>>();
        let repeats = cc.windows(3).any(|w| &w[0] == &w[2]);
        let contains = cc.windows(2).enumerate().any(|(i, w)| {
            // use the rest of the string after the window.
            cc[i + 2..]
                .iter()
                .collect::<String>()
                .contains(&w.iter().collect::<String>())
        });
        if repeats && contains {
            nice += 1;
        }
    }

    println!("Part 2: {}", nice);
}
