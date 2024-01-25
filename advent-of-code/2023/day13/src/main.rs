#[derive(Clone)]
struct Note {
    grid: Vec<Vec<char>>,
}

impl From<&str> for Note {
    fn from(s: &str) -> Self {
        let grid = s.lines().map(|line| line.chars().collect()).collect();
        Self { grid }
    }
}

impl Note {
    fn rows_diff(&self, y1: usize, y2: usize) -> usize {
        let mut delta = 0;
        for x in 0..self.grid[0].len() {
            if self.grid[y1][x] != self.grid[y2][x] {
                delta += 1;
            }
        }
        delta
    }

    fn columns_diff(&self, x1: usize, x2: usize) -> usize {
        let mut delta = 0;
        for y in 0..self.grid.len() {
            if self.grid[y][x1] != self.grid[y][x2] {
                delta += 1;
            }
        }
        delta
    }

    // Similar to p1_reflection, but now we want to know if the
    // difference in the two sides of the reflection are 1.
    fn p2_reflection(&self) -> usize {
        'rows: for i in 0..self.grid.len() - 1 {
            // If the delta is 1 or less, we have a potential middle point.
            let mut diff = self.rows_diff(i, i + 1);
            if diff <= 1 {
                // Perform a similar check as p1_reflection, but now
                // we track the differences between the rows. If we
                // ever total more than 1 distance, we know this can't
                // be it, so move on.
                let min_distance_to_edge = i.min(self.grid.len() - i - 2);
                for d in 1..=min_distance_to_edge {
                    diff += self.rows_diff(i - d, i + d + 1);
                    if diff > 1 {
                        continue 'rows;
                    }
                }

                // "every mirror has exactly one smudge" - This was
                // the key to this check. Since there must be a smudge
                // in each mirror, we'd likely not get a diff of
                // 0.
                if diff == 0 {
                    continue 'rows;
                }
                return (i + 1) * 100;
            }
        }

        // Do the same as above, but look for columns now.
        'columns: for i in 0..self.grid[0].len() - 1 {
            let mut diff = self.columns_diff(i, i + 1);
            if diff <= 1 {
                let min_distance_to_edge = i.min(self.grid[0].len() - i - 2);
                for d in 1..=min_distance_to_edge {
                    diff += self.columns_diff(i - d, i + d + 1);
                    if diff > 1 {
                        continue 'columns;
                    }
                }
                if diff == 0 {
                    continue 'columns;
                }
                return i + 1;
            }
        }

        0
    }

    fn rows_equal(&self, y1: usize, y2: usize) -> bool {
        self.grid[y1] == self.grid[y2]
    }

    fn columns_equal(&self, x1: usize, x2: usize) -> bool {
        for line in &self.grid {
            if line[x1] != line[x2] {
                return false;
            }
        }
        true
    }

    // The description of the problem is key here:
    //
    // "to find the reflection in each patter" - this implies there is
    // only one reflection for each grid.
    //
    // "you need to find a perfect reflection" - this implies that the
    // reflection spans as much of the grid as possible. That is, one
    // of the edges of the reflection will be the edge of the grid.
    fn p1_reflection(&self) -> usize {
        // For each row (and column in next block) look for a
        // potential middle point
        'rows: for i in 0..self.grid.len() - 1 {
            if self.rows_equal(i, i + 1) {
                // We've found a potential middle point, now see if we
                // can make a "perfect" reflection. By this, it means
                // can you fold at this point and have it equal.
                //
                // We can verify this by finding the minumum distance
                // to the edge and then checking that each reflected
                // pair is also equal.
                let min_distance_to_edge = i.min(self.grid.len() - i - 2);
                for d in 1..=min_distance_to_edge {
                    // If we ever find a row pair that isn't equal, we
                    // know this can't be it, so move on.
                    if !self.rows_equal(i - d, i + d + 1) {
                        continue 'rows;
                    }
                }

                // If we found one, return the "value" of this reflection.
                return (i + 1) * 100;
            }
        }

        // Do the same for columns as we did for rows.
        'columns: for i in 0..self.grid[0].len() - 1 {
            if self.columns_equal(i, i + 1) {
                let min_distance_to_edge = i.min(self.grid[0].len() - i - 2);
                for d in 1..=min_distance_to_edge {
                    if !self.columns_equal(i - d, i + d + 1) {
                        continue 'columns;
                    }
                }
                return i + 1;
            }
        }

        0
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let notes = input
        .split("\n\n")
        .map(|note| note.into())
        .collect::<Vec<Note>>();

    let now = std::time::Instant::now();
    let p1 = notes.iter().map(|note| note.p1_reflection()).sum::<usize>();
    println!("p1: {} ({:?})", p1, now.elapsed());

    let now = std::time::Instant::now();
    let p2 = notes.iter().map(|note| note.p2_reflection()).sum::<usize>();
    println!("p2: {} ({:?})", p2, now.elapsed());
}
