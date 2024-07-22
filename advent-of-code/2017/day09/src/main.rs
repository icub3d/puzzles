fn main() {
    let input = include_str!("../input").chars().collect::<Vec<char>>();
    let (p1, p2) = run(&input);
    println!("p1: {}", p1);
    println!("p2: {}", p2);
}

fn run(input: &Vec<char>) -> (usize, usize) {
    let mut score = 0;
    let mut garbage = 0;
    let mut group_value = 0;
    let mut in_garbage = false;
    let mut ignore_next = false;

    for c in input {
        if ignore_next {
            ignore_next = false;
            continue;
        }

        match (c, in_garbage) {
            ('>', _) => in_garbage = false,
            ('!', _) => ignore_next = true,
            ('<', false) => in_garbage = true,
            ('{', false) => group_value += 1,
            ('}', false) => {
                score += group_value;
                group_value -= 1;
            }
            (_, true) => garbage += 1,
            _ => (),
        }
    }
    (score, garbage)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_score() {
        assert_eq!(run(&"{}".chars().collect()).0, 1);
        assert_eq!(run(&"{{{}}}".chars().collect()).0, 6);
        assert_eq!(run(&"{{},{}}".chars().collect()).0, 5);
        assert_eq!(run(&"{{{},{},{{}}}}".chars().collect()).0, 16);
        assert_eq!(run(&"{<a>,<a>,<a>,<a>}".chars().collect()).0, 1);
        assert_eq!(run(&"{{<ab>},{<ab>},{<ab>},{<ab>}}".chars().collect()).0, 9);
        assert_eq!(run(&"{{<!!>},{<!!>},{<!!>},{<!!>}}".chars().collect()).0, 9);
        assert_eq!(run(&"{{<a!>},{<a!>},{<a!>},{<ab>}}".chars().collect()).0, 3);
    }

    #[test]
    fn test_garbage() {
        assert_eq!(run(&"<>".chars().collect()).1, 0);
        assert_eq!(run(&"<random characters>".chars().collect()).1, 17);
        assert_eq!(run(&"<<<<>".chars().collect()).1, 3);
        assert_eq!(run(&"<{!>}>".chars().collect()).1, 2);
        assert_eq!(run(&"<!!>".chars().collect()).1, 0);
        assert_eq!(run(&"<!!!>>".chars().collect()).1, 0);
        assert_eq!(run(&"<{o\"i!a,<{i<a>".chars().collect()).1, 10);
    }
}
