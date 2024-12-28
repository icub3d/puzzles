use std::collections::HashMap;

const INPUT: &'static str = include_str!("input.txt");

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // This was a fairly simply input, so I just split_once on the double
    // newline and then split the patterns by comma and the problems by newline.
    let (patterns, problems) = INPUT
        .split_once("\r\n\r\n")
        .ok_or("Failed to split input")?;
    let patterns = patterns.split(", ").collect::<Vec<_>>();
    let problems = problems.lines().collect::<Vec<_>>();

    // Part 1, dynamic programming with memoization.
    let now = std::time::Instant::now();
    let mut memo = HashMap::new();
    let p1 = problems
        .iter()
        .filter(|problem| solve_p1(&mut memo, &patterns, problem))
        .count();
    println!("p1: {} ({:?})", p1, now.elapsed());

    // Part 2, same but we keep counts instead of stopping at the first match.
    let now = std::time::Instant::now();
    let mut memo = HashMap::new();
    let mut hits = 0;
    let p2 = problems
        .iter()
        .map(|problem| solve_p2(&mut memo, &patterns, problem, &mut hits))
        .sum::<usize>();
    println!("p2: {} (time: {:?}, hits: {})", p2, now.elapsed(), hits);

    Ok(())
}

fn solve_p1<'a>(memo: &mut HashMap<&'a str, bool>, patterns: &[&'a str], problem: &'a str) -> bool {
    // Some base cases: If we have an empty problem, we're done. If we've
    // already solved this problem, return the result.
    if problem.len() == 0 {
        return true;
    }
    if let Some(&result) = memo.get(problem) {
        return result;
    }

    // Try each pattern and see if it matches the start of the problem.
    for pattern in patterns {
        if problem.starts_with(pattern) {
            // If we find a match, recurse on the rest of the problem.
            if solve_p1(memo, patterns, &problem[pattern.len()..]) {
                memo.insert(pattern, true);
                return true;
            }
        }
    }

    // If we didn't find a match, memoize and return false.
    memo.insert(problem, false);
    false
}

fn solve_p2<'a>(
    memo: &mut HashMap<&'a str, usize>,
    patterns: &[&'a str],
    problem: &'a str,
    hits: &mut usize,
) -> usize {
    // Same as part 1, but we keep counts instead of stopping at the first match.
    if problem.len() == 0 {
        return 1;
    }
    if let Some(&count) = memo.get(problem) {
        *hits += 1;
        return count;
    }

    // Try each pattern and see if it matches the start of the problem.
    let mut count = 0;
    for pattern in patterns {
        if problem.starts_with(pattern) {
            // If we find a match, recurse on the rest of the problem and add
            // the count.  Note, no return here, we want to try all patterns.
            count += solve_p2(memo, patterns, &problem[pattern.len()..], hits);
        }
    }

    // Memoize and return the count.
    memo.insert(problem, count);
    count
}
