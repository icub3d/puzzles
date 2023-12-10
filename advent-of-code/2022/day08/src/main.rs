use std::fs;

fn main() {
    let lines = fs::read_to_string("input").unwrap();
    let grid = lines
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| c.to_digit(10).unwrap() as isize)
                .collect::<Vec<isize>>()
        })
        .collect::<Vec<Vec<isize>>>();

    let mut p1 = 0;
    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            if x == 0
                || y == 0
                || x == grid[y].len() - 1
                || y == grid.len() - 1
                || visible(&grid, x, y, -1, 0)
                || visible(&grid, x, y, 1, 0)
                || visible(&grid, x, y, 0, 1)
                || visible(&grid, x, y, 0, -1)
            {
                p1 += 1;
            }
        }
    }
    println!("p1: {p1}");

    let mut largest = 0;
    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            let s = scenic_score(&grid, x, y);
            if s > largest {
                largest = s;
            }
        }
    }
    println!("p2: {largest}");
}

fn visible(grid: &[Vec<isize>], x: usize, y: usize, dx: isize, dy: isize) -> bool {
    let max = grid[y][x];
    let w = grid[y].len();
    let h = grid.len();
    let (mut x, mut y) = (x as isize + dx, y as isize + dy);
    while x >= 0 && y >= 0 && x < w as isize && y < h as isize {
        if grid[y as usize][x as usize] >= max {
            return false;
        }
        x += dx;
        y += dy;
    }
    true
}

fn visible_dist(
    grid: &[Vec<isize>],
    x: usize,
    y: usize,
    w: usize,
    h: usize,
    dx: isize,
    dy: isize,
) -> usize {
    let max = grid[y][x];
    let (mut x, mut y) = (x as isize + dx, y as isize + dy);
    let mut dist = 0;
    while x >= 0 && y >= 0 && x < w as isize && y < h as isize {
        dist += 1;
        if grid[y as usize][x as usize] >= max {
            break;
        }
        x += dx;
        y += dy;
    }
    dist
}

fn scenic_score(grid: &[Vec<isize>], x: usize, y: usize) -> usize {
    let w = grid[0].len();
    let h = grid.len();

    let l = visible_dist(&grid, x, y, w, h, -1, 0);
    let r = visible_dist(&grid, x, y, w, h, 1, 0);
    let d = visible_dist(&grid, x, y, w, h, 0, 1);
    let u = visible_dist(&grid, x, y, w, h, 0, -1);

    l * r * u * d
}
