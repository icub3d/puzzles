#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BlockType {
    File,
    Free,
}

#[derive(Debug)]
struct Block {
    type_: BlockType,
    data: Vec<usize>,
    free: usize,
}

impl Block {
    fn new(id: usize, size: usize) -> Self {
        if id % 2 == 0 {
            Block {
                type_: BlockType::File,
                free: 0,
                data: vec![id / 2; size],
            }
        } else {
            Block {
                type_: BlockType::Free,
                free: size,
                data: Vec::new(),
            }
        }
    }

    fn fill(&mut self, data: &Vec<usize>) {
        self.data.extend(data);
        self.free -= data.len();
        if self.free == 0 {
            self.type_ = BlockType::File;
        }
    }

    fn clear(&mut self) {
        self.free = self.data.len();
        self.data.clear();
        self.type_ = BlockType::Free;
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = include_str!("input.txt");

    // How big is the "disk" going to be?
    let sum: usize = input
        .chars()
        .filter(|&c| c != '\n')
        .filter_map(|c| Some(c.to_digit(10)? as usize))
        .sum();
    println!("Sum: {}", sum);

    // P1
    let now = std::time::Instant::now();

    // Build the disk.
    let mut disk: Vec<Option<usize>> = input
        .chars()
        .filter(|&c| c.is_digit(10)) // No whitespace.
        .filter_map(|c| c.to_digit(10)) // Convert to digit.
        .map(|c| c as usize) // We'll use this as the size of the vec.
        .enumerate()
        .flat_map(|(i, c)| {
            if i % 2 == 0 {
                vec![Some(i / 2); c]
            } else {
                vec![None; c]
            }
        })
        .collect();

    // Now that we have the disk, we should keep two pointers, one for the
    // left-most free space and one for the right most filled space.
    // We'll then swap and move to the next until they reach each other.
    let mut left = 0;
    let mut right = disk.len() - 1;
    while left < right {
        // Find the next free space.
        while left < right && disk[left].is_some() {
            left += 1;
        }

        // Find the last filled space.
        while left < right && disk[right].is_none() {
            right -= 1;
        }

        // Swap the two.
        disk.swap(left, right);

        // Increment the pointers.
        left += 1;
        right -= 1;
    }

    let p1: usize = disk
        .iter()
        .enumerate()
        .filter_map(|(i, &x)| x.map(|x| x * i))
        .sum();
    println!("p1: {} ({:?})", p1, now.elapsed());

    // P2
    let now = std::time::Instant::now();

    // Build by blocks now instead of "bytes" so we can easily determine if
    // there is space for a file.
    let mut disk: Vec<Block> = input
        .chars()
        .filter(|&c| c.is_digit(10)) // No whitespace.
        .filter_map(|c| c.to_digit(10)) // Convert to digit.
        .map(|c| c as usize) // We'll use this as the size of the vec.
        .enumerate()
        .map(|(i, size)| Block::new(i, size))
        .collect();

    // We now find files from right to left. For each of them, we want to
    // find the left-most free space where we could move it. If we find a
    // space, we move the file.
    let mut right = disk.len() - 1;
    while right > 0 {
        // We can ignore free space.
        if disk[right].type_ == BlockType::Free {
            right -= 1;
            continue;
        }

        // Find the next free space.
        let mut left = 0;
        while left < right {
            // If it's filled or there isn't enough space, move to the next.
            if disk[left].type_ == BlockType::File
                || (disk[left].type_ == BlockType::Free && disk[left].free < disk[right].data.len())
            {
                left += 1;
                continue;
            }

            // Otherwise, we found a space, so we can move the file.
            let r = disk[right].data.clone();
            disk[left].fill(&r);
            disk[right].clear();
            break;
        }
        right -= 1;
    }

    let p2 = disk
        .iter()
        .flat_map(|d| {
            // Include all the data first and then pad with 0s for all the free space.
            d.data
                .iter()
                .cloned()
                .chain(std::iter::repeat(0).take(d.free))
        })
        .enumerate()
        .map(|(i, x)| x * i)
        .sum::<usize>();
    println!("p2: {} ({:?})", p2, now.elapsed());

    Ok(())
}
