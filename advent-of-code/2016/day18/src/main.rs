#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
    Trap,
    Safe,
}

fn is_trap(left: Tile, center: Tile, right: Tile) -> Tile {
    match (left, center, right) {
        (Tile::Trap, Tile::Trap, Tile::Safe) => Tile::Trap,
        (Tile::Safe, Tile::Trap, Tile::Trap) => Tile::Trap,
        (Tile::Trap, Tile::Safe, Tile::Safe) => Tile::Trap,
        (Tile::Safe, Tile::Safe, Tile::Trap) => Tile::Trap,
        _ => Tile::Safe,
    }
}

fn next(row: Vec<Tile>) -> Vec<Tile> {
    let mut next_row = Vec::new();
    for i in 0..row.len() {
        let left = if i == 0 { Tile::Safe } else { row[i - 1] };
        let center = row[i];
        let right = if i == row.len() - 1 {
            Tile::Safe
        } else {
            row[i + 1]
        };
        next_row.push(is_trap(left, center, right));
    }
    next_row
}

fn main() {
    let input = include_str!("../input").trim();
    let start = input
        .chars()
        .map(|c| match c {
            '^' => Tile::Trap,
            _ => Tile::Safe,
        })
        .collect::<Vec<_>>();
    println!("p1: {}", p1(start.clone(), 40));
    println!("p2: {}", p1(start, 400_000));
}

fn p1(start: Vec<Tile>, rows: usize) -> usize {
    let mut row = start;
    let mut count = row.iter().filter(|&&t| t == Tile::Safe).count();
    for _ in 1..rows {
        row = next(row);
        count += row.iter().filter(|&&t| t == Tile::Safe).count();
    }
    count
}
