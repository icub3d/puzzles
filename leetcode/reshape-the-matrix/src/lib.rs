pub struct Solution;

impl Solution {
    pub fn matrix_reshape(mat: Vec<Vec<i32>>, r: i32, c: i32) -> Vec<Vec<i32>> {
        let r = r as usize;
        let c = c as usize;
        let f = mat.iter().flatten().collect::<Vec<_>>();
        if f.len() != r * c {
            return mat;
        }

        let mut mat = vec![vec![0i32; c]; r];
        for x in 0..r {
            for y in 0..c {
                mat[x][y] = *f[x * c + y];
            }
        }
        mat
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::matrix_reshape(vec![vec![1, 2], vec![3, 4]], 2, 4),
            vec![vec![1, 2], vec![3, 4]]
        );
        assert_eq!(
            Solution::matrix_reshape(vec![vec![1, 2], vec![3, 4]], 1, 4),
            vec![vec![1, 2, 3, 4]]
        );
    }
}
