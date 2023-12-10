// My apologies for anyone looking at this. it's ugly. The general
// idea is to get each tile and produce a set of all of it's rotations
// (8 in total). Once that's done, I discovered there are only 4 with
// two matches. In fact, there is only one solution, so there is no
// possibility of a tile going in more than one spot. This made it
// simpler because I could just find a corner piece, place it in the
// top right, then find the top row, then find the rest of the rows by
// looking for the match to the piece above it.
//
// Once that's done, we have a map that we can then rotate as well and
// look for the sea monster. The problem was that I was unfamiliar
// with the matrix library I chose and was constantly getting rows and
// columns mixed up. So, I think the map uses column/row and the
// monster uses row/column.
use pathfinding::matrix::Matrix;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;

const DIM: usize = 10;
const SOL_DIM: usize = 12;

#[derive(Debug, Clone)]
struct Tile {
    id: usize,
    matrix: Matrix<char>,
    edges: Vec<String>,
    rotations: HashSet<Matrix<char>>,
    matches: Vec<(usize, String)>,
}

impl Tile {
    fn find_match(&self, m: String) -> usize {
        for mm in self.matches.iter() {
            if mm.1 == m {
                return mm.0;
            } else if mm.1.chars().rev().collect::<String>() == m {
                return mm.0;
            }
        }
        println!("not found: {}: {} {:?}", self.id, m, self.matches);
        return 0;
    }

    fn rotations(&mut self) {
        let mut mat = self.matrix.clone();
        for _x in 0..4 {
            self.rotations.insert(mat.clone());
            self.rotations.insert(mat.flipped_lr());
            self.rotations.insert(mat.flipped_ud());
            self.rotations.insert(mat.flipped_ud().flipped_lr());
            self.rotations.insert(mat.flipped_lr().flipped_ud());
            mat.rotate_cw(1);
        }
    }

    fn south(&self) -> String {
        let mut bottom = "".to_string();
        for x in 0..DIM {
            bottom.push(self.matrix[&(x, 9)]);
        }
        bottom
    }

    fn east(&self) -> String {
        let mut right = "".to_string();
        for x in 0..DIM {
            right.push(self.matrix[&(9, x)]);
        }
        right
    }

    fn edges(&mut self) {
        // Use integers for edges so we can easily determine the matches.
        let mut top = "".to_string();
        let mut bottom = "".to_string();
        let mut left = "".to_string();
        let mut right = "".to_string();

        for x in 0..DIM {
            top.push(self.matrix[&(x, 0)]);
            bottom.push(self.matrix[&(x, 9)]);
            left.push(self.matrix[&(0, x)]);
            right.push(self.matrix[&(9, x)]);
        }
        self.edges = vec![top, bottom, left, right];
        self.edges.append(
            &mut self
                .edges
                .iter()
                .map(|e| e.chars().rev().collect::<String>())
                .collect::<Vec<String>>(),
        );
    }
}

fn main() {
    // Just playing around here with the functional style :)
    let contents = fs::read_to_string("input").unwrap();
    let mut tiles: Vec<Tile> = contents
        .split("\n\n")
        .collect::<Vec<&str>>()
        .iter()
        .map(|&ll| {
            let ll = ll.split("\n").collect::<Vec<&str>>();
            let mut tile = Tile {
                rotations: HashSet::new(),
                matches: Vec::new(),
                id: ll[0]
                    .split(" ")
                    .last()
                    .unwrap()
                    .trim_end_matches(":")
                    .parse::<usize>()
                    .unwrap(),
                matrix: Matrix::from_rows(
                    ll[1..]
                        .iter()
                        .map(|l| l.chars().collect::<Vec<char>>())
                        .collect::<Vec<Vec<char>>>(),
                )
                .unwrap(),
                edges: vec![],
            };
            tile.edges();
            tile.rotations();
            tile
        })
        .collect();

    // Find all matches between all edges of the tiles.
    for i in 0..tiles.len() {
        for j in i + 1..tiles.len() {
            if let Some(edge) = match_edge(&(tiles[i]).edges, &(tiles[j]).edges) {
                let id1 = tiles[i].id.clone();
                let id2 = tiles[j].id.clone();
                tiles[i].matches.push((id2, edge.clone()));
                tiles[j].matches.push((id1, edge.clone()));
            }
        }
    }

    // Corners will only have two matches. This is a bit of a hack
    // because technically, there could be some edges that are the
    // same and this would break.
    let product: usize = tiles
        .iter()
        .filter(|t| t.matches.len() == 2)
        .map(|t| t.id)
        .product();
    println!("product of corners: {}", product);

    let mut solution: Matrix<Option<Tile>> = Matrix::new(SOL_DIM, SOL_DIM, None);
    // we want to grab one wit only two matches and then orient it in
    // such a way that it's matches are south and east.
    'outer: for tile in tiles.iter() {
        if tile.matches.len() != 2 {
            continue;
        }
        for rot in tile.rotations.iter() {
            let mut matches = vec![(tile.matches[0].1.clone(), tile.matches[1].1.clone())];
            matches.push((
                tile.matches[0].1.chars().rev().collect::<String>(),
                tile.matches[1].1.clone(),
            ));
            matches.push((
                tile.matches[1].1.chars().rev().collect::<String>(),
                tile.matches[0].1.clone(),
            ));
            matches.push((
                tile.matches[0].1.chars().rev().collect::<String>(),
                tile.matches[1].1.chars().rev().collect::<String>(),
            ));
            for (l, r) in matches {
                if (rot_east(&rot) == l && rot_south(&rot) == r)
                    || (rot_south(&rot) == l && rot_east(&rot) == r)
                {
                    let mut top_left = tile.clone();
                    top_left.matrix = rot.clone();
                    solution[&(0, 0)] = Some(top_left);
                    break 'outer;
                }
            }
        }
    }

    let mut tile_map = HashMap::new();
    for tile in tiles.iter() {
        tile_map.insert(tile.id, tile.clone());
    }

    // Next, find the top row by finding the right match.
    for x in 1..SOL_DIM {
        let left = solution[&(x - 1, 0)].as_ref().unwrap();
        let east = left.east();
        let mut tile = tile_map
            .get(&left.find_match(east.clone()))
            .unwrap()
            .clone();
        for rot in tile.rotations.iter() {
            let west = rot_west(&rot);
            if west == east {
                tile.matrix = rot.clone();
                solution[&(x, 0)] = Some(tile);
                break;
            }
        }
    }

    // Now we do the same thing but for the rest of the rows and columns.
    for y in 1..SOL_DIM {
        for x in 0..SOL_DIM {
            let top = solution[&(x, y - 1)].as_ref().unwrap();
            let south = top.south();
            let mut tile = tile_map
                .get(&top.find_match(south.clone()))
                .unwrap()
                .clone();
            for rot in tile.rotations.iter() {
                let north = rot_north(&rot);
                if south == north {
                    tile.matrix = rot.clone();
                    solution[&(x, y)] = Some(tile);
                    break;
                }
            }
        }
    }

    // Now we have a solution, we only want to keep the insides and we
    // are gonna turn it into it's own matrix.
    let dims = (SOL_DIM * (DIM - 2), SOL_DIM * (DIM - 2));
    let mut map = Matrix::new(dims.0, dims.1, ' ');
    let mut total = 0;
    for y in 0..SOL_DIM {
        for x in 0..SOL_DIM {
            let tile = solution[&(x, y)].as_ref().unwrap();
            let inner = match tile.matrix.slice(1..DIM - 1, 1..DIM - 1) {
                Ok(v) => v,
                Err(e) => panic!("{}", e),
            };
            // should now have an 8x8 matrix; put it into the map.
            for iy in 0..DIM - 2 {
                for ix in 0..DIM - 2 {
                    // count all the '#' for later.
                    if inner[&(ix, iy)] == '#' {
                        total += 1;
                    }
                    map[&(x * (DIM - 2) + ix, y * (DIM - 2) + iy)] = inner[&(ix, iy)];
                }
            }
        }
    }

    // Get all of the rotations of the map.
    let mut rotations = HashSet::new();
    for _x in 0..4 {
        rotations.insert(map.clone());
        rotations.insert(map.flipped_lr());
        rotations.insert(map.flipped_ud());
        rotations.insert(map.flipped_ud().flipped_lr());
        rotations.insert(map.flipped_lr().flipped_ud());
        map.rotate_cw(1);
    }

    // create the sea monster
    let sea_monster = "                  # 
#    ##    ##    ###
 #  #  #  #  #  #   ";
    let mut monster = Matrix::from_rows(sea_monster.lines().map(|l| l.chars())).unwrap();
    println!("map: {} {}", map.rows, map.columns);
    println!("monster: {} {}", monster.rows, monster.columns);
    // Find the rotation with any matches.
    for rot in rotations.iter() {
        // it's 20 by 3, so we only need to look through part of the solution.
        let mut found = 0;
        for y in 0..rot.columns - monster.columns {
            'matcher: for x in 0..rot.rows - monster.rows {
                for dy in 0..monster.columns {
                    for dx in 0..monster.rows {
                        let m = monster[&(dx, dy)];
                        let r = rot[&(x + dx, y + dy)];
                        if m == '#' && r != '#' {
                            continue 'matcher;
                        }
                    }
                }
                // If we got all the way through, we found a match!
                found += 1;
            }
        }
        if found != 0 {
            println!("found: {} {}", found, total - (found * 15));
            break;
        }
    }
}

fn rot_west(rot: &Matrix<char>) -> String {
    let mut left = "".to_string();
    for x in 0..DIM {
        left.push(rot[&(0, x)]);
    }
    left
}

fn rot_north(rot: &Matrix<char>) -> String {
    let mut top = "".to_string();
    for x in 0..DIM {
        top.push(rot[&(x, 0)]);
    }
    top
}

fn rot_south(rot: &Matrix<char>) -> String {
    let mut bottom = "".to_string();
    for x in 0..DIM {
        bottom.push(rot[&(x, 9)]);
    }
    bottom
}

fn rot_east(rot: &Matrix<char>) -> String {
    let mut right = "".to_string();
    for x in 0..DIM {
        right.push(rot[&(9, x)]);
    }
    right
}

fn match_edge(ee1: &Vec<String>, ee2: &Vec<String>) -> Option<String> {
    for e1 in ee1.iter() {
        for e2 in ee2.iter() {
            if e1 == e2 {
                return Some(e1.to_string());
            }
        }
    }
    None
}
