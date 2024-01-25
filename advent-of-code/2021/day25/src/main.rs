fn p1(grid: &[Vec<char>]) -> usize {
    let mut steps = 0;
    let mut grid = grid.to_vec();

    let w = grid[0].len();
    let h = grid.len();

    loop {
        let mut moved = false;
        steps += 1;

        // First collect all the > who have the ability to move.
        let mut move_rights = Vec::new();
        for (y, row) in grid.iter().enumerate() {
            for (x, c) in row.iter().enumerate() {
                if *c == '>'
                    && (((x < w - 1) && row[x + 1] == '.') || (x == w - 1 && row[0] == '.'))
                {
                    move_rights.push((x, y));
                }
            }
        }

        // Performa all the move rights.
        for (x, y) in move_rights {
            grid[y][x] = '.';
            grid[y][if x == w - 1 { 0 } else { x + 1 }] = '>';
            moved = true;
        }

        // Now collect all the 'v' who have the ability to move.
        let mut move_downs = Vec::new();
        for (y, row) in grid.iter().enumerate() {
            for (x, c) in row.iter().enumerate() {
                if *c == 'v'
                    && (((y < h - 1) && grid[y + 1][x] == '.') || (y == h - 1 && grid[0][x] == '.'))
                {
                    move_downs.push((x, y));
                }
            }
        }

        // Perform all the move downs.
        for (x, y) in move_downs {
            grid[y][x] = '.';
            grid[if y == h - 1 { 0 } else { y + 1 }][x] = 'v';
            moved = true;
        }

        if !moved {
            break;
        }
    }

    steps
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let grid = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    println!("p1: {}", p1(&grid));
}
