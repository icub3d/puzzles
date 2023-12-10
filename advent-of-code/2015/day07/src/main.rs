use std::collections::HashMap;

#[derive(Debug, Eq, PartialEq)]
enum Value<'a> {
    Actual(u16),
    Variable(&'a str),
}

impl<'a> From<&'a str> for Value<'a> {
    fn from(s: &'a str) -> Value<'a> {
        match s.parse::<u16>() {
            Ok(n) => Value::Actual(n),
            Err(_) => Value::Variable(s),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Operation<'a> {
    Value(Value<'a>),
    And(Value<'a>, Value<'a>),
    Or(Value<'a>, Value<'a>),
    LShift(Value<'a>, usize),
    RShift(Value<'a>, usize),
    Not(Value<'a>),
}

impl<'a> From<&'a str> for Operation<'a> {
    fn from(s: &'a str) -> Operation<'a> {
        let parts = s.split(' ').collect::<Vec<&str>>();
        match parts.len() {
            1 => Operation::Value(Value::from(parts[0])),
            2 => Operation::Not(Value::from(parts[1])),
            3 => match parts[1] {
                "AND" => Operation::And(Value::from(parts[0]), Value::from(parts[2])),
                "OR" => Operation::Or(Value::from(parts[0]), Value::from(parts[2])),
                "LSHIFT" => {
                    Operation::LShift(Value::from(parts[0]), parts[2].parse::<usize>().unwrap())
                }
                "RSHIFT" => {
                    Operation::RShift(Value::from(parts[0]), parts[2].parse::<usize>().unwrap())
                }
                _ => panic!("unknown 3 part operation: {}", parts.len()),
            },
            _ => panic!("unknown parts len: {}", parts.len()),
        }
    }
}

fn solve<'a>(
    wires: &HashMap<&'a str, Operation<'a>>,
    memo: &mut HashMap<&'a str, u16>,
    k: &'a str,
) -> u16 {
    if memo.contains_key(k) {
        return memo[k];
    }
    let op = wires.get(k).unwrap();
    let result = match op {
        Operation::Value(v) => match v {
            Value::Actual(v) => *v,
            Value::Variable(v) => solve(wires, memo, v),
        },
        Operation::Not(v) => match v {
            Value::Actual(v) => !*v,
            Value::Variable(v) => !solve(wires, memo, v),
        },
        Operation::And(Value::Actual(l), Value::Actual(r)) => l & r,
        Operation::And(Value::Actual(l), Value::Variable(r)) => l & solve(wires, memo, r),
        Operation::And(Value::Variable(l), Value::Actual(r)) => solve(wires, memo, l) & r,
        Operation::And(Value::Variable(l), Value::Variable(r)) => {
            solve(wires, memo, l) & solve(wires, memo, r)
        }
        Operation::Or(Value::Actual(l), Value::Actual(r)) => l | r,
        Operation::Or(Value::Actual(l), Value::Variable(r)) => l | solve(wires, memo, r),
        Operation::Or(Value::Variable(l), Value::Actual(r)) => solve(wires, memo, l) | r,
        Operation::Or(Value::Variable(l), Value::Variable(r)) => {
            solve(wires, memo, l) | solve(wires, memo, r)
        }
        Operation::LShift(Value::Actual(l), r) => l << r,
        Operation::LShift(Value::Variable(l), r) => solve(wires, memo, l) << r,
        Operation::RShift(Value::Actual(l), r) => l >> r,
        Operation::RShift(Value::Variable(l), r) => solve(wires, memo, l) >> r,
    };

    memo.insert(k, result);

    result
}

fn main() {
    let lines = std::fs::read_to_string("input").unwrap();
    let mut wires: HashMap<&str, Operation> = HashMap::new();

    for line in lines.lines() {
        let parts = line.split(" -> ").collect::<Vec<&str>>();
        wires.insert(parts[1], parts[0].into());
    }

    // The trick here was to use memoization.
    let mut memo = HashMap::new();
    let p1 = solve(&wires, &mut memo, "a");
    println!("part 1: {}", p1);

    wires.insert("b", Operation::Value(Value::Actual(p1)));
    let mut memo = HashMap::new();
    println!("part 2: {}", solve(&wires, &mut memo, "a"));
}
