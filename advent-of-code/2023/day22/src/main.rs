use std::{
    cell::RefCell,
    collections::{HashMap, HashSet, VecDeque},
    rc::Rc,
};

// Point is used to track the start and end of a block.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Point {
    x: usize,
    y: usize,
    z: usize,
}

// Used to parse the input.
impl From<&str> for Point {
    fn from(input: &str) -> Self {
        let mut split = input.split(',');
        let x = split.next().unwrap().parse().unwrap();
        let y = split.next().unwrap().parse().unwrap();
        let z = split.next().unwrap().parse().unwrap();
        Self { x, y, z }
    }
}

// Block represents a block within the grid.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct Block {
    start: Point,
    end: Point,
}

// Used to parse the input.
impl From<&str> for Block {
    fn from(input: &str) -> Self {
        let mut split = input.split('~');
        let start = split.next().unwrap().into();
        let end = split.next().unwrap().into();

        Self { start, end }
    }
}

impl Block {
    // A lot of these functions assume that the start is the "min" and
    // the end is the "max". As such, we don't have to do any checks
    // for lower/higher bounds.

    // Check if the given block intersects with this block in only the
    // X and Y Direction. We ignore Z here becaus the methods below
    // are interested in blocks that are above or below.
    fn intersects_xy(&self, other: &Block) -> bool {
        self.start.x <= other.end.x
            && self.end.x >= other.start.x
            && self.start.y <= other.end.y
            && self.end.y >= other.start.y
    }

    // Find any block below us in the z direction that we'd collide
    // with. Return that z's value
    fn highest_z(&self, others: &[Block]) -> usize {
        others
            .iter()
            .filter(|b| *b != self)
            .filter(|b| self.intersects_xy(b))
            .map(|b| b.end.z)
            .max()
            .unwrap_or(0)
    }

    // Find any block directly above (z + 1) above us. These would be
    // blocks that are resting on us.
    fn blocks_above(&self, others: &[Block]) -> Vec<Block> {
        others
            .iter()
            .filter(|b| *b != self && self.intersects_xy(b) && b.start.z == self.end.z + 1)
            .cloned()
            .collect()
    }

    // Find any block directly below (z - 1) us. These would be blocks
    // that w are sitting on.
    fn blocks_below(&self, others: &[Block]) -> Vec<Block> {
        others
            .iter()
            .filter(|b| *b != self && self.intersects_xy(b) && b.end.z == self.start.z - 1)
            .cloned()
            .collect()
    }

    // Check if the given block is safe to remove. This means that any
    // blocks above myself would still be supported by some other
    // block.
    fn safe_to_remove(&self, others: &[Block]) -> bool {
        let above = self.blocks_above(others);
        if above.is_empty() {
            return true;
        }

        let to_check = others
            .iter()
            .filter(|b| *b != self)
            .cloned()
            .collect::<Vec<_>>();

        for above in above.iter() {
            let below = above.blocks_below(&to_check);
            if below.is_empty() {
                return false;
            }
        }

        true
    }

    // Second implementation. We basically are slowly removing blocks
    // that would fall and then check if that fall would cause others
    // to fall.
    fn falls_faster(&self, others: &[Block]) -> usize {
        let mut fallen = HashSet::new();
        let mut frontier = VecDeque::new();
        frontier.push_back(self.clone());

        let mut remaining = others.to_vec();

        // Note, I struggled for a bit thinking about blocks where
        // there are two or more supporting them. By checking all
        // blocks that fall, if all of them are going to be removed,
        // we'll eventually get to a point where the block above would
        // fall.

        while let Some(block) = frontier.pop_front() {
            // If I've already fallen because of something else, I
            // don't need to be checked again.
            if fallen.contains(&block) {
                continue;
            }

            // Add myself the list of blocks that would fall and
            // remove my self from the remaining list of blocks.
            fallen.insert(block.clone());
            remaining.retain(|p| *p != block);

            // Find any blocks that would now fall because I'm gone.
            let new_fallen = block
                .blocks_above(&remaining)
                .iter()
                .filter(|b| b.blocks_below(&remaining).is_empty())
                .cloned()
                .collect::<Vec<_>>();

            // Add them to the frontier to be checked.
            frontier.extend(new_fallen);
        }

        // Self is included here, so subtract 1.
        fallen.len() - 1
    }

    // First implementation. It works but takes about a minute to finish.
    #[allow(dead_code)]
    fn falls(&self, others: &[Block]) -> usize {
        // Start out the list of blocks minus mine.
        let mut cur = others
            .iter()
            .filter(|b| *b != self)
            .cloned()
            .collect::<Vec<_>>();

        let mut count = 0;
        loop {
            // Find any blocks that would fall because I'm missing.
            let falls = cur
                .iter()
                .filter(|b| b.blocks_below(&cur).is_empty() && b.start.z != 1)
                .cloned()
                .collect::<Vec<_>>();
            if falls.is_empty() {
                // If there are no more blocks that would fall, we are done.
                break;
            }

            // Increase our fallen count.
            count += falls.len();

            // Remove any of the blocks that have fallen.
            cur = cur
                .iter()
                .filter(|b| !falls.contains(b))
                .cloned()
                .collect::<Vec<_>>();
        }

        count
    }
}

fn main() {
    // Make the blocks and then sort them by their z axis. We'll want
    // to move lower ones first.
    let input = std::fs::read_to_string("input").unwrap();
    let mut blocks: Vec<Block> = input.lines().map(|line| line.into()).collect();

    // Sorting is important here. I also did some sanity checks to
    // simplify intersection checking.
    blocks.sort_by_key(|b| b.start.z.min(b.end.z));
    assert!(blocks.iter().all(|b| b.start.x <= b.end.x));
    assert!(blocks.iter().all(|b| b.start.y <= b.end.y));
    assert!(blocks.iter().all(|b| b.start.z <= b.end.z));

    let org_time = std::time::Instant::now();
    // Now we can drop the blocks one by one. If a block collides with
    // any other block, we can't drop it anymore.
    let now = std::time::Instant::now();
    let mut dropped = Vec::new();
    for block in blocks.iter() {
        let mut block = block.clone();
        let lowest = block.highest_z(&dropped);
        // We assume the blocks Z directions start lower than they end.
        let diff = block.end.z - block.start.z;
        block.start.z = lowest + 1;
        block.end.z = block.start.z + diff;
        dropped.push(block.clone());
    }

    // We want to see which blocks can be removed and wouldn't cause a
    // drop. For each block, see if any are above it. If not, we can
    // clearly remove it. If there are, make sure there is at least
    // one other block below it that could support it.
    let p1 = dropped
        .iter()
        .filter(|b| b.safe_to_remove(&dropped))
        .count();
    println!("p1: {} ({:?})", p1, now.elapsed());

    // For part 2, we now want to find which of the blocks would cause
    // the most to fall. We start by seeing if removing the given
    // block would cause the block above it to fall. If so, we can
    // check the block above it. We do this recursively until the
    // block wouln't fall.
    let now = std::time::Instant::now();
    let p2 = dropped
        .iter()
        .rev()
        .map(|b| b.falls_faster(&dropped))
        .sum::<usize>();
    println!("p2: {} ({:?})", p2, now.elapsed());
    println!("total-time-initial: {:?}", org_time.elapsed());
    println!();

    // Generate a graph of the blocks to solve both parts.
    let total_time = std::time::Instant::now();
    let mut dropped: Vec<Block> = Vec::new();

    // We'll use these to keep our "directed" edges in the graph.
    let mut aboves: HashMap<Block, Vec<Block>> = HashMap::new();
    let mut belows: HashMap<Block, Vec<Block>> = HashMap::new();

    // We'll start by dropping the blocks and then updating the edges.
    for block in blocks.iter() {
        // This logic is essentially the same as the first solution,
        // we still need to find it's new position in the world.
        let mut block = block.clone();
        let lowest = block.highest_z(&dropped);
        let diff = block.end.z - block.start.z;
        block.start.z = lowest + 1;
        block.end.z = block.start.z + diff;
        dropped.push(block.clone());

        // Find all the blocks below me.
        let below = dropped
            .iter()
            .filter(|b| b.end.z == block.start.z - 1 && b.intersects_xy(&block))
            .cloned()
            .collect::<Vec<_>>();
        belows
            .entry(block.clone())
            .or_default()
            .extend(below.iter().cloned());

        // Add myself to the list of blocks above the blocks below me.
        below
            .iter()
            .for_each(|b| aboves.entry(b.clone()).or_default().push(block.clone()));
    }

    // Part 1 using the above and below edges.
    let now = std::time::Instant::now();
    let p1 = dropped
        .iter()
        .filter(|b| {
            // All of the blocks above me need to have one other block
            // below them.
            aboves
                .get(b)
                .map(|bs| {
                    bs.iter()
                        .all(|b| belows.get(b).map(|bs| bs.len() > 1).unwrap_or(false))
                })
                .unwrap_or(true)
        })
        .count();
    println!("p1: {} ({:?})", p1, now.elapsed());

    // Part 2 using the above and below edges.
    let now = std::time::Instant::now();
    let p2 = dropped
        .iter()
        .rev()
        .map(|b| falls_graph(b, &aboves, &belows))
        .sum::<usize>();
    println!("p2: {} ({:?})", p2, now.elapsed());
    println!("total-time-hashmaps: {:?}", total_time.elapsed());
    println!();

    // Use an acual graph.
    let total_time = std::time::Instant::now();
    let mut dropped: Vec<Rc<RefCell<GraphBlock>>> = Vec::new();
    for block in blocks {
        // Again this part is essentially the same but now we are
        // using a GraphBlock.
        let mut block = GraphBlock::new(&block);
        let lowest = block.highest_z(&dropped);
        let diff = block.end.z - block.start.z;
        block.start.z = lowest + 1;
        block.end.z = block.start.z + diff;

        // Add myself to the list of blocks.
        let block = Rc::new(RefCell::new(block));
        dropped.push(block.clone());

        // Update the above and below edges for the blocks.
        dropped
            .iter()
            .filter(|b| {
                block.borrow().intersects_xy(&b.borrow())
                    && b.borrow().end.z == block.borrow().start.z - 1
            })
            .for_each(|b| {
                b.borrow_mut().above.push(block.clone());
                block.borrow_mut().below.push(b.clone());
            });
    }

    // Part 1 using the graph.
    let now = std::time::Instant::now();
    let p1 = dropped
        .iter()
        .filter(|b| {
            // All of the blocks above me need to have one other block below them
            b.borrow().above.iter().all(|b| b.borrow().below.len() > 1)
        })
        .count();
    println!("p1: {} ({:?})", p1, now.elapsed());

    // Part 2 using the graph.
    let now = std::time::Instant::now();
    let p2 = dropped
        .iter()
        .rev()
        .map(|b| b.borrow().falls())
        .sum::<usize>();
    println!("p2: {} ({:?})", p2, now.elapsed());
    println!("total-time-graph: {:?}", total_time.elapsed());
}

fn falls_graph(
    block: &Block,
    aboves: &HashMap<Block, Vec<Block>>,
    belows: &HashMap<Block, Vec<Block>>,
) -> usize {
    // This is essentially the same as the falls_faster method above, but
    // we can use the aboves and belows to find the new frontier blocks
    // instead of having to calculate them each time.

    let mut fallen = HashSet::new();
    let mut frontier = VecDeque::new();
    frontier.push_back(block.clone());

    while let Some(block) = frontier.pop_front() {
        if fallen.contains(&block) {
            continue;
        }

        fallen.insert(block.clone());

        // Fand any blocks above me that have no other blocks below
        // them when we remove the fallen blocks from their belows.
        let new_fallen = aboves
            .get(&block)
            .map(|bs| {
                bs.iter()
                    .filter(|b| {
                        belows
                            .get(b)
                            .map(|bs| bs.iter().filter(|b| !fallen.contains(b)).count() < 1)
                            .unwrap_or(true)
                    })
                    .cloned()
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();

        frontier.extend(new_fallen);
    }

    fallen.len() - 1
}

#[derive(Debug, Clone, Eq)]
struct GraphBlock {
    start: Point,
    end: Point,

    above: Vec<Rc<RefCell<GraphBlock>>>,
    below: Vec<Rc<RefCell<GraphBlock>>>,
}

impl std::hash::Hash for GraphBlock {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.start.hash(state);
        self.end.hash(state);
    }
}

impl PartialEq for GraphBlock {
    fn eq(&self, other: &Self) -> bool {
        self.start == other.start && self.end == other.end
    }
}

impl GraphBlock {
    fn new(block: &Block) -> Self {
        let start = block.start;
        let end = block.end;
        Self {
            start,
            end,
            above: Vec::new(),
            below: Vec::new(),
        }
    }

    fn intersects_xy(&self, other: &GraphBlock) -> bool {
        self.start.x <= other.end.x
            && self.end.x >= other.start.x
            && self.start.y <= other.end.y
            && self.end.y >= other.start.y
    }

    fn highest_z(&self, others: &[Rc<RefCell<GraphBlock>>]) -> usize {
        others
            .iter()
            .filter(|b| *b.borrow() != *self)
            .filter(|b| self.intersects_xy(&b.borrow()))
            .map(|b| b.borrow().end.z)
            .max()
            .unwrap_or(0)
    }

    fn falls(&self) -> usize {
        use std::ops::Deref;

        // This is essentially the same as the falls_faster method above, but
        // we can use the graph now.
        let mut fallen = HashSet::new();
        let mut frontier = VecDeque::new();
        frontier.push_back(self.clone());

        while let Some(block) = frontier.pop_front() {
            if fallen.contains(&block) {
                continue;
            }
            fallen.insert(block.clone());

            // Add any blocks that are above me and have no other
            // blocks below them to the frontier.
            for above in block.above.iter() {
                if above
                    .borrow()
                    .below
                    .iter()
                    .filter(|b| !fallen.contains(b.borrow().deref()))
                    .count()
                    < 1
                {
                    frontier.push_back(above.borrow().deref().clone());
                }
            }
        }

        fallen.len() - 1
    }
}
