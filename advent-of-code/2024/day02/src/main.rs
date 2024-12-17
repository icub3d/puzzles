fn main() {
    let input = include_str!("input.txt");
    let reports = input
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|num| num.parse::<i32>().unwrap())
                .collect::<Vec<i32>>()
        })
        .collect::<Vec<Vec<i32>>>();

    // For each report we count it if it's valid.
    let p1 = reports
        .iter()
        .filter(|report| is_valid_report(report))
        .count();
    println!("p1: {}", p1);

    let p2 = reports
        .iter()
        .filter(|report| could_be_valid_report(report))
        .count();
    println!("p2: {}", p2);
}

fn could_be_valid_report(report: &Vec<i32>) -> bool {
    if is_valid_report(report) {
        return true;
    }

    // Try removing each element and see if it's valid.
    for i in 0..report.len() {
        let mut new_report = (*report).clone();
        new_report.remove(i);
        if is_valid_report(&new_report) {
            return true;
        }
    }

    return false;
}

fn is_valid_report(report: &[i32]) -> bool {
    // First find if we are increasing or decreasing.
    let mut increasing = true;
    if report[0] > report[1] {
        increasing = false;
    }

    let windows = report.windows(2);
    for window in windows {
        // Check equality and increasing/decreasing.
        if window[0] == window[1] {
            return false;
        } else if increasing && window[0] > window[1] {
            return false;
        } else if !increasing && window[0] < window[1] {
            return false;
        }

        // Check if the difference is more than 3. We already know it's at
        // least one because of the increasing/decreasing check.
        if (window[1] - window[0]).abs() > 3 {
            return false;
        }
    }

    // If we have gone through all the windows, it's valid.
    true
}
