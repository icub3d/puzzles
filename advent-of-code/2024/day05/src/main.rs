use std::collections::HashMap;

fn main() {
    let input = include_str!("input.txt");
    let (rules, updates) = input.split_once("\r\n\r\n").unwrap();

    let rules: HashMap<usize, Vec<usize>> = rules
        .lines()
        .filter_map(|l| {
            let mut parts = l.split("|");
            let key = parts.next()?.parse::<usize>().ok()?;
            let right = parts.next()?.parse::<usize>().ok()?;
            Some((key, right))
        })
        .fold(HashMap::new(), |mut acc, (key, right)| {
            acc.entry(key).or_default().push(right);
            acc
        });

    let updates = updates
        .lines()
        .map(|l| {
            l.split(",")
                .map(|w| w.parse::<usize>().unwrap())
                .collect::<Vec<usize>>()
        })
        .collect::<Vec<Vec<usize>>>();

    // Iterate through all the updates.
    let mut p1 = 0;
    let mut invalid = Vec::new();
    'outer: for pages in &updates {
        // Iterate through all the pages.
        for (i, page) in pages.iter().enumerate() {
            // Get the pages that must go after this page.
            let expect_after = match rules.get(page) {
                Some(v) => v,
                None => continue,
            };

            // Go through all the pages before and make sure none are in the
            // expect_after.
            for before in &pages[..i] {
                if expect_after.contains(before) {
                    invalid.push(pages.clone());
                    continue 'outer;
                }
            }
        }
        p1 += pages[pages.len() / 2];
    }
    println!("p1: {}", p1);

    // Part 2
    let mut p2 = 0;
    for pages in &invalid {
        // We are going to start with a new empty array and then we are going to
        // add pages in the correct order from the invalid array. We do this by
        // finding the pages that have to go after it and adding them. This is
        // essentially an insertion sort where the sort order is maintained by
        // the rules.
        let mut new_pages = Vec::new();
        for page in pages {
            let expect_after = match rules.get(page) {
                Some(v) => v,
                None => {
                    new_pages.push(*page);
                    continue;
                }
            };
            let mut i = 0;
            while i < new_pages.len() {
                if expect_after.contains(&new_pages[i]) {
                    break;
                }
                i += 1;
            }
            new_pages.insert(i, *page);
        }
        p2 += new_pages[new_pages.len() / 2];
    }
    println!("p2: {}", p2);
}
