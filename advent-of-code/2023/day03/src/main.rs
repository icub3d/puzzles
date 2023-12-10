fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let map: Vec<Vec<char>> = input.lines().map(|l| l.chars().collect()).collect();

    // Scan through all the positions and see if we find a digit. If
    // we do, look for a special character next to it (recursively
    // looking right).
    let mut p1 = 0;
    for x in 0..map.len() {
        let mut y = 0;
        while y < map[x].len() {
            if map[x][y].is_ascii_digit() && adjacent_to_symbol(&map, x, y) {
                // Find the whole number, convert it add it to
                // total and then update y position.
                let (n, len) = whole_number(&map, x, y);
                y += len;
                p1 += n;
            } else {
                y += 1;
            }
        }
    }
    println!("p1: {}", p1);

    // Scan through the map again and find all instances of '*'. If we
    // find one, we want check to see if it has exactly two adjacent
    // numbers.
    let mut p2 = 0;
    for x in 0..map.len() {
        for y in 0..map[x].len() {
            if map[x][y] == '*' {
                let nn = adjacent_numbers(&map, x, y);
                if nn.len() == 2 {
                    p2 += nn[0] * nn[1];
                }
            }
        }
    }
    println!("p2: {}", p2);
}

// Find all adjacent coordinates to a given coordinate. Only returns
// coordinates inside the map.
fn adjacent(x: usize, y: usize, max_x: usize, max_y: usize) -> Vec<(usize, usize)> {
    let mut result = Vec::new();
    for i in x.saturating_sub(1)..max_x.min(x + 2) {
        for j in y.saturating_sub(1)..max_y.min(y + 2) {
            if i == x && j == y {
                continue;
            }
            result.push((i, j));
        }
    }
    result
}

// Check all adjacent coordinates for a symbol. If we happen to have a
// digit to the right of us, then we can also check if that digit is
// adjacent to a symbol and if it is, then this one is too.
fn adjacent_to_symbol(map: &[Vec<char>], x: usize, y: usize) -> bool {
    for (i, j) in adjacent(x, y, map.len(), map[0].len()) {
        if !map[i][j].is_ascii_digit() && map[i][j] != '.' {
            return true;
        }
        // Any right digits would also be included and so can be checked as well.
        if y + 1 == j && x == i && map[i][j].is_ascii_digit() && adjacent_to_symbol(map, i, j) {
            return true;
        }
    }
    false
}

// Find the whole number starting at (x, y). Return the number and the
// length of the number.
fn whole_number(map: &[Vec<char>], x: usize, y: usize) -> (usize, usize) {
    let mut j = y + 1;
    while j < map[x].len() && map[x][j].is_ascii_digit() {
        j += 1;
    }
    (
        map[x][y..j]
            .iter()
            .collect::<String>()
            .parse::<usize>()
            .unwrap(),
        j - y,
    )
}

// Check to see if there is a number at (x, y).
fn number(map: &[Vec<char>], x: usize, y: usize) -> Option<usize> {
    if map[x][y].is_ascii_digit() {
        let mut left = y;
        while left != 0 && map[x][left - 1].is_ascii_digit() {
            left -= 1;
        }
        return Some(whole_number(map, x, left).0);
    }
    None
}

// Find all adjacent numbers to (x, y).
fn adjacent_numbers(map: &[Vec<char>], x: usize, y: usize) -> Vec<usize> {
    let mut result = Vec::new();

    // Check going left.
    if let Some(n) = number(map, x, y - 1) {
        result.push(n);
    }

    // Check going right.
    if let Some(n) = number(map, x, y + 1) {
        result.push(n);
    }

    // Check if the top middle is a digit, if so, the the entire top
    // must be contained in it.
    if x > 0 && map[x - 1][y].is_ascii_digit() {
        if let Some(n) = number(map, x - 1, y) {
            result.push(n);
        }
    } else if x > 0 {
        // Either the left or right could have a number, check both.
        if let Some(n) = number(map, x - 1, y - 1) {
            result.push(n);
        }

        if let Some(n) = number(map, x - 1, y + 1) {
            result.push(n);
        }
    }

    // Check if the bottom middle is a digit, if so, the the entire top
    // must be contained in it.
    if x < map.len() && map[x + 1][y].is_ascii_digit() {
        if let Some(n) = number(map, x + 1, y) {
            result.push(n);
        }
    } else if x < map.len() {
        // Either the left or right could have a number, check both.
        if let Some(n) = number(map, x + 1, y - 1) {
            result.push(n);
        }

        if let Some(n) = number(map, x + 1, y + 1) {
            result.push(n);
        }
    }

    result
}
