use std::{collections::HashMap, fs};

fn main() {
    let points: HashMap<&str, i32> = HashMap::from([
        ("A X", 1 + 3), // Rock Rock
        ("A Y", 2 + 6), // Rock Paper
        ("A Z", 3 + 0), // Rock Scissors
        ("B X", 1 + 0), // Paper Rock
        ("B Y", 2 + 3), // Paper Paper
        ("B Z", 3 + 6), // Paper Scissors
        ("C X", 1 + 6), // Scissors Rock
        ("C Y", 2 + 0), // Scissors Paper
        ("C Z", 3 + 3), // Scissors Scissors
    ]);

    let points_p2: HashMap<&str, i32> = HashMap::from([
        ("A X", 0 + 3), // Rock Lose - scissors
        ("A Y", 3 + 1), // Rock Draw - rock
        ("A Z", 6 + 2), // Rock Win - paper
        ("B X", 0 + 1), // Paper Lose - rock
        ("B Y", 3 + 2), // Paper Draw - paper
        ("B Z", 6 + 3), // Paper Win - scissors
        ("C X", 0 + 2), // Scissors Lose - paper
        ("C Y", 3 + 3), // Scissors Draw - scissors
        ("C Z", 6 + 1), // Scissors Win - rock
    ]);

    let lines = fs::read_to_string("input").unwrap();
    let lines = lines.lines();

    let mut score = 0;
    let mut score_p2 = 0;
    for line in lines {
        score += points[line];
        score_p2 += points_p2[line];
    }

    println!("p1: {}", score);
    println!("p2: {}", score_p2);
}
