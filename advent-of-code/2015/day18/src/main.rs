fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let mut grid = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    for _ in 0..100 {
        grid = step(&grid);
    }

    let result = grid
        .iter()
        .map(|row| row.iter().filter(|&&c| c == '#').count())
        .sum::<usize>();
    println!("{}", result);

    let mut grid = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    for _ in 0..100 {
        grid = step2(&grid);
    }

    let result = grid
        .iter()
        .map(|row| row.iter().filter(|&&c| c == '#').count())
        .sum::<usize>();
    println!("{}", result);
}

fn step(grid: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut new_grid = grid.clone();
    for (i, row) in grid.iter().enumerate() {
        for (j, &c) in row.iter().enumerate() {
            let on = surrounding_on(grid, i, j);
            if c == '#' && on != 2 && on != 3 {
                new_grid[i][j] = '.';
            } else if c == '.' && on == 3 {
                new_grid[i][j] = '#';
            }
        }
    }
    new_grid
}

fn step2(grid: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut new_grid = grid.clone();
    for (i, row) in grid.iter().enumerate() {
        for (j, &c) in row.iter().enumerate() {
            if (i == 0 && j == 0)
                || (i == 0 && j == grid.len() - 1)
                || (i == grid.len() - 1 && j == 0)
                || (i == grid.len() - 1 && j == grid.len() - 1)
            {
                continue;
            }
            let on = surrounding_on(grid, i, j);
            if c == '#' && on != 2 && on != 3 {
                new_grid[i][j] = '.';
            } else if c == '.' && on == 3 {
                new_grid[i][j] = '#';
            }
        }
    }
    new_grid
}

fn surrounding_on(grid: &Vec<Vec<char>>, x: usize, y: usize) -> usize {
    let mut result = 0;
    for i in x.saturating_sub(1)..=x.saturating_add(1) {
        for j in y.saturating_sub(1)..=y.saturating_add(1) {
            if i == x && j == y {
                continue;
            }
            if let Some(row) = grid.get(i) {
                if let Some(&c) = row.get(j) {
                    if c == '#' {
                        result += 1;
                    }
                }
            }
        }
    }
    result
}
