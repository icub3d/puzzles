fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let data = serde_json::from_str(&input).unwrap();

    println!("p1: {}", part1(&data));
    println!("p2: {}", part2(&data));
}

fn part1(data: &serde_json::Value) -> i64 {
    match data {
        serde_json::Value::Number(n) => n.as_i64().unwrap(),
        serde_json::Value::Array(a) => a.iter().map(|v| part1(v)).sum(),
        serde_json::Value::Object(o) => o.values().map(|v| part1(v)).sum(),
        _ => 0,
    }
}

fn part2(data: &serde_json::Value) -> i64 {
    match data {
        serde_json::Value::Number(n) => n.as_i64().unwrap(),
        serde_json::Value::Array(a) => a.iter().map(|v| part2(v)).sum(),
        serde_json::Value::Object(o) => match o.values().any(|v| v == "red") {
            true => 0,
            false => o.values().map(|v| part2(v)).sum(),
        },
        _ => 0,
    }
}
