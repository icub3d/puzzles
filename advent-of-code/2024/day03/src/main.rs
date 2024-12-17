fn main() {
    let input = include_str!("input.txt");
    let expression = r"mul\(([0-9]{1,3}),([0-9]{1,3})\)";
    let re = regex::Regex::new(expression).unwrap();

    let p1 = re.captures_iter(input).filter_map(|cap| {
        let a = cap[1].parse::<i32>().ok()?;
        let b = cap[2].parse::<i32>().ok()?;
        Some(a * b)
    }).sum::<i32>();
    println!("p1: {}", p1);

    let expression = r"(mul\(([0-9]{1,3}),([0-9]{1,3})\)|don't\(\)|do\(\))";
    let re = regex::Regex::new(expression).unwrap();

    let mut enabled = true;
    let p2 = re.captures_iter(input).filter_map(|cap| {
        match (&cap[0], enabled) {
            ("do()", _) => { enabled = true; None },
            ("don't()", _) => { enabled = false; None },
            (_, true) => {
                let a = cap[2].parse::<i32>().ok()?;
                let b = cap[3].parse::<i32>().ok()?;
                Some(a * b)
            },
            _ => None,
        }
    }).sum::<i32>();
    println!("p2: {:?}", p2);
}
