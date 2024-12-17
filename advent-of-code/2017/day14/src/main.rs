fn main() {
    let input = "vbqugkhl";
    let mut used = 0;
    let mut grid = [[false; 128]; 128];
    for i in 0..128 {
        let row = format!("{}-{}", input, i);
        let hash = knot_hash(&row);
        let binary = hex_to_binary(&hash);
        used += binary.chars().filter(|&c| c == '1').count();
        for (j, c) in binary.chars().enumerate() {
            grid[i][j] = c == '1';
        }
    }
    println!("p1: {}", used);

    let mut regions = 0;

    fn dfs(grid: &mut [[bool; 128]], i: isize, j: isize) {
        if i < 0 || i >= 128 || j < 0 || j >= 128 || !grid[i as usize][j as usize] {
            return;
        }
        grid[i as usize][j as usize] = false;
        dfs(grid, i + 1, j);
        dfs(grid, i - 1, j);
        dfs(grid, i, j + 1);
        dfs(grid, i, j - 1);
    }

    for i in 0..128_isize {
        for j in 0..128_isize {
            if grid[i as usize][j as usize] {
                regions += 1;
                dfs(&mut grid, i, j);
            }
        }
    }

    println!("p2: {}", regions);
}

fn knot_hash(input: &str) -> String {
    let mut lengths: Vec<usize> = input.chars().map(|c| c as usize).collect();
    lengths.extend(&[17, 31, 73, 47, 23]);
    let mut list: Vec<usize> = (0..256).collect();
    let mut current_position = 0;
    let mut skip_size = 0;
    for _ in 0..64 {
        for length in &lengths {
            reverse(&mut list, current_position, *length);
            current_position = (current_position + length + skip_size) % list.len();
            skip_size += 1;
        }
    }
    let dense_hash = list
        .chunks(16)
        .map(|chunk| chunk.iter().fold(0, |acc, &x| acc ^ x));
    dense_hash.map(|x| format!("{:02x}", x)).collect()
}

fn reverse(list: &mut Vec<usize>, start: usize, length: usize) {
    let len = list.len();
    for i in 0..length / 2 {
        let a = (start + i) % len;
        let b = (start + length - i - 1) % len;
        list.swap(a, b);
    }
}

fn hex_to_binary(hex: &str) -> String {
    hex.chars()
        .map(|c| format!("{:04b}", c.to_digit(16).unwrap()))
        .collect()
}
