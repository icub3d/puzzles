fn find_next(history: &[isize], lasts: &mut Vec<isize>) -> isize {
    let diffs = history.windows(2).map(|w| w[1] - w[0]).collect::<Vec<_>>();
    if !diffs.iter().any(|&d| d != 0) {
        return lasts.iter().sum::<isize>();
    }
    lasts.push(diffs[diffs.len() - 1]);
    find_next(&diffs, lasts)
}

fn find_prev(history: &[isize], firsts: &mut Vec<isize>) -> isize {
    let diffs = history.windows(2).map(|w| w[1] - w[0]).collect::<Vec<_>>();
    if !diffs.iter().any(|&d| d != 0) {
        return firsts.iter().rev().fold(0, |acc, x| x - acc);
    }
    firsts.push(diffs[0]);
    find_prev(&diffs, firsts)
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let histories = input
        .lines()
        .map(|l| {
            l.split_whitespace()
                .map(|d| d.parse::<isize>().unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let p1 = histories
        .iter()
        .map(|h| find_next(h, &mut vec![h[h.len() - 1]]))
        .sum::<isize>();
    println!("p1: {}", p1);
    let p2 = histories
        .iter()
        .map(|h| find_prev(h, &mut vec![h[0]]))
        .sum::<isize>();
    println!("p2: {}", p2);
}
