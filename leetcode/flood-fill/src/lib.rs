pub struct Solution;

use std::collections::HashSet;
impl Solution {
    pub fn flood_fill(image: Vec<Vec<i32>>, sr: i32, sc: i32, color: i32) -> Vec<Vec<i32>> {
        let mut visited = HashSet::new();
        let mut updated = image.clone();
        let original_color = image[sr as usize][sc as usize];
        let m = image.len() as i32;
        let n = image[0].len() as i32;
        Solution::flood_fill_helper(
            &image,
            &mut updated,
            &mut visited,
            sr,
            sc,
            color,
            original_color,
            m,
            n,
        );
        updated
    }

    pub fn flood_fill_helper(
        original: &Vec<Vec<i32>>,
        updated: &mut Vec<Vec<i32>>,
        visited: &mut HashSet<(i32, i32)>,
        sr: i32,
        sc: i32,
        color: i32,
        original_color: i32,
        r: i32,
        c: i32,
    ) {
        // If we have already visited, we don't need to visit again.
        if visited.contains(&(sr, sc)) {
            return;
        }

        // update this pixel
        updated[sr as usize][sc as usize] = color;
        visited.insert((sr, sc));

        // Check surrounding pixels.
        for (dr, dc) in vec![(-1, 0), (1, 0), (0, -1), (0, 1)] {
            if sr + dr < 0 || sr + dr >= r || sc + dc < 0 || sc + dc >= c {
                continue;
            }
            if original[(sr + dr) as usize][(sc + dc) as usize] == original_color {
                Solution::flood_fill_helper(
                    original,
                    updated,
                    visited,
                    sr + dr,
                    sc + dc,
                    color,
                    original_color,
                    r,
                    c,
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::flood_fill(vec![vec![1, 1, 1], vec![1, 1, 0], vec![1, 0, 1]], 1, 1, 2),
            vec![vec![2, 2, 2], vec![2, 2, 0], vec![2, 0, 1]]
        );
        assert_eq!(
            Solution::flood_fill(vec![vec![0, 0, 0], vec![0, 0, 0]], 0, 0, 0),
            vec![vec![0, 0, 0], vec![0, 0, 0]]
        );
    }
}
