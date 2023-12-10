use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

use regex::Regex;

#[derive(Debug, PartialEq, Clone)]
enum Rule {
    Term(char),
    Rules(Vec<usize>),
}

fn main() {
    let file = File::open("input").unwrap();
    let buf = BufReader::new(file);
    let aa: Vec<String> = buf.lines().map(|l| l.unwrap()).collect();

    let mut rules: HashMap<usize, Vec<Rule>> = HashMap::new();
    let mut start = 0;
    for (i, a) in aa.iter().enumerate() {
        // We break when we get an empty line.
        if a.trim() == "" {
            start = i + 1;
            break;
        }

        // split by space and get the ID (first element).
        let pp = a.split(":").collect::<Vec<&str>>();
        let id = pp[0].parse::<usize>().unwrap();
        let p = pp[1].trim();

        let mut rr = vec![];
        for p in p.split("|") {
            let p = p.trim();
            // It's a character, not a reference to another rule.
            if p.contains("\"") {
                rr.push(Rule::Term(p.chars().nth(1).unwrap()));
                continue;
            }
            rr.push(Rule::Rules(
                p.split(" ").map(|l| l.parse::<usize>().unwrap()).collect(),
            ))
        }
        rules.insert(id, rr);
    }

    // For part 1 I basically build a regex recursively but that won't
    // work for part 2 which can recurse infinitely.
    let rule = expand(&rules, 0);
    let rule = "^".to_owned() + rule.as_str() + &"$".to_owned();
    let re = Regex::new(&rule).unwrap();
    let mut count = 0;
    for a in aa[start..].iter() {
        if re.is_match(a.as_str()) {
            count += 1;
        }
    }
    println!("{}", count);

    // Modify the rules - we split up the new rule 11 so we don't have
    // any rules with 3+ elements.
    rules.insert(8, vec![Rule::Rules(vec![42]), Rule::Rules(vec![42, 8])]);
    rules.insert(
        11,
        vec![Rule::Rules(vec![42, 31]), Rule::Rules(vec![42, 137])],
    );
    rules.insert(137, vec![Rule::Rules(vec![11, 31])]);

    // Convert to CNF so CYK algorithm will work.
    cnf(&mut rules);

    // YIKES! much slower, but it was right :)
    let mut count = 0;
    for a in aa[start..].iter() {
        if cyk(&rules, a.chars().collect()) {
            count += 1;
        }
    }
    println!("{}", count);
}

fn expand(rules: &HashMap<usize, Vec<Rule>>, cur: usize) -> String {
    let mut s = String::new();
    let mut rr = vec![];
    for rule in rules[&cur].iter() {
        match rule {
            Rule::Term(c) => rr.push(c.to_string()),
            Rule::Rules(vv) => {
                let mut s = String::new();
                s.push_str(
                    &vv.iter()
                        .map(|i| expand(rules, *i))
                        .collect::<Vec<String>>()
                        .join(""),
                );
                rr.push(s)
            }
        }
    }

    if rr.len() == 1 {
        return rr[0].clone();
    }

    s.push_str("(");
    s.push_str(&rr.join("|"));
    s.push_str(")");
    s
}

fn cnf(rules: &mut HashMap<usize, Vec<Rule>>) {
    // This is a more simplified removal because we know the grammar
    // only produces units (and we fixed rules with elements > 2).
    loop {
        let refc = rules.clone();
        let mut stop = true;
        for (_, rr) in rules.iter_mut() {
            // drain_filter is in nightly :-(
            let mut removed = vec![];
            let mut i = 0;
            while i != rr.len() {
                if let Rule::Rules(v) = &rr[i] {
                    if v.len() == 1 {
                        removed.push(rr.remove(i));
                        continue;
                    }
                }
                i += 1;
            }
            if removed.len() > 0 {
                stop = false;
            }
            for rule in removed {
                if let Rule::Rules(v) = rule {
                    for rule in refc[&v[0]].iter() {
                        rr.push(rule.clone());
                    }
                }
            }
        }
        if stop {
            break;
        }
    }
}

fn produces(rules: &HashMap<usize, Vec<Rule>>, c: char) -> Vec<usize> {
    let mut pp = Vec::new();
    for (k, rr) in rules.iter() {
        for rule in rr.iter() {
            if let Rule::Term(t) = rule {
                if c == *t {
                    pp.push(*k);
                }
            }
        }
    }
    pp
}

fn cyk(rules: &HashMap<usize, Vec<Rule>>, line: Vec<char>) -> bool {
    let n = line.len();
    let r = rules.keys().max().unwrap();
    let mut P = vec![vec![vec![false; r + 1]; n + 1]; n + 1];

    // Set the unit values to true.
    for s in 0..n {
        for i in produces(rules, line[s]) {
            P[1][s][i] = true;
        }
    }

    // length of span
    for l in 2..=n {
        // start of span
        for s in 0..=n - l + 1 {
            // partition of span
            for p in 1..=l - 1 {
                for (&a, rr) in rules.iter() {
                    for rule in rr {
                        match rule {
                            Rule::Term(_) => continue,
                            Rule::Rules(v) => {
                                let b = v[0];
                                let c = v[1];

                                if P[p][s][b] && P[l - p][s + p][c] {
                                    P[l][s][a] = true;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    P[n][0][0]
}
