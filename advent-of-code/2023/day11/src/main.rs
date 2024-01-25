#[derive(Debug)]
struct Point {
    x: isize,
    y: isize,
}

impl Point {
    // https://en.wikipedia.org/wiki/Taxicab_geometry
    fn manhattan_distance(&self, other: &Point) -> usize {
        ((self.x - other.x).abs() + (self.y - other.y).abs()) as usize
    }

    fn mins(&self, other: &Point) -> (isize, isize) {
        (self.x.min(other.x), self.y.min(other.y))
    }

    fn maxes(&self, other: &Point) -> (isize, isize) {
        (self.x.max(other.x), self.y.max(other.y))
    }
}

fn sum_distances(
    stars: &[Point],
    empty_rows: &[isize],
    empty_columns: &[isize],
    expansion: usize,
) -> usize {
    let mut sum = 0;
    // For each start, calculate the manhattan distance to each other
    // start left in the list. If the stars would traverse a row or
    // column in our empty lists, we need to add 1 for each of them.
    for (i, star) in stars.iter().enumerate() {
        for other in &stars[i + 1..] {
            // Calculate manhattan distance between the two stars.
            let dist = star.manhattan_distance(other);

            // Add {expansion} to the distance for each row or column
            // it would traverse.
            let (min_x, min_y) = star.mins(other);
            let (max_x, max_y) = star.maxes(other);
            let rows = empty_rows
                .iter()
                .filter(|r| min_y < **r && max_y > **r)
                .count()
                * expansion;
            let cols = empty_columns
                .iter()
                .filter(|c| min_x < **c && max_x > **c)
                .count()
                * expansion;

            // Add it to our total.
            sum += dist + rows + cols;
        }
    }
    sum
}

fn main() {
    // Get the input
    let input = std::fs::read_to_string("input").unwrap();
    let grid = input
        .lines()
        .map(|l| l.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    // Find all the '#' in the grid and get their coordinates.
    let stars = grid
        .iter()
        .enumerate()
        .flat_map(|(y, r)| {
            r.iter()
                .enumerate()
                .filter(|(_, c)| **c == '#')
                .map(move |(x, _)| Point {
                    x: x as isize,
                    y: y as isize,
                })
        })
        .collect::<Vec<_>>();

    // Determine which rows and columns are empty.
    let empty_rows = grid
        .iter()
        .enumerate()
        .filter(|(_, r)| r.iter().all(|c| *c == '.'))
        .map(|(y, _)| y as isize)
        .collect::<Vec<_>>();
    let empty_cols = (0..grid[0].len())
        .filter(|x| grid.iter().all(|r| r[*x] == '.'))
        .map(|x| x as isize)
        .collect::<Vec<_>>();

    println!("p1: {}", sum_distances(&stars, &empty_rows, &empty_cols, 1));
    println!(
        "p2: {}",
        sum_distances(&stars, &empty_rows, &empty_cols, 999_999)
    );
}
