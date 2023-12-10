#[derive(Debug, Clone)]
struct Cuboid {
    x: std::ops::RangeInclusive<isize>,
    y: std::ops::RangeInclusive<isize>,
    z: std::ops::RangeInclusive<isize>,
}

fn range_from_str(s: &str) -> std::ops::RangeInclusive<isize> {
    let parts = s.split("..").collect::<Vec<&str>>();
    parts[0].parse::<isize>().unwrap()..=parts[1].parse::<isize>().unwrap()
}

impl Cuboid {
    fn new(line: &str) -> Self {
        let parts = line.split(',').collect::<Vec<&str>>();
        Self {
            x: range_from_str(&parts[0][2..]),
            y: range_from_str(&parts[1][2..]),
            z: range_from_str(&parts[2][2..]),
        }
    }
    fn volume(&self) -> isize {
        (self.x.end() - self.x.start() + 1)
            * (self.y.end() - self.y.start() + 1)
            * (self.z.end() - self.z.start() + 1)
    }
}

#[derive(Debug, Clone)]
struct Input {
    on: bool,
    cuboid: Cuboid,
}

impl Input {
    fn new(line: &str) -> Self {
        let parts = line.split(' ').collect::<Vec<&str>>();
        Self {
            on: matches!(parts[0], "on"),
            cuboid: Cuboid::new(parts[1]),
        }
    }

    // If the cube is not within the range, we ignore the whole
    // thing. I'm hoping that's what's meant from the description.
    fn ignore_part_one(&self) -> bool {
        self.cuboid.x.start() < &-50
            || self.cuboid.x.start() > &50
            || self.cuboid.x.end() < &-50
            || self.cuboid.x.end() > &50
            || self.cuboid.y.start() < &-50
            || self.cuboid.y.start() > &50
            || self.cuboid.y.end() < &-50
            || self.cuboid.y.end() > &50
            || self.cuboid.z.start() < &-50
            || self.cuboid.z.start() > &50
            || self.cuboid.z.end() < &-50
            || self.cuboid.z.end() > &50
    }

    // Determine where these two intersect and if they do, return the
    // intersection.
    fn intersect(&self, other: &Self) -> Option<Self> {
        // We can find the overlap by the max start and min end
        // positions.

        // Find the max start positions.
        let x_start = *self.cuboid.x.start().max(other.cuboid.x.start());
        let y_start = *self.cuboid.y.start().max(other.cuboid.y.start());
        let z_start = *self.cuboid.z.start().max(other.cuboid.z.start());

        // Find the min end positions.
        let x_end = *self.cuboid.x.end().min(other.cuboid.x.end());
        let y_end = *self.cuboid.y.end().min(other.cuboid.y.end());
        let z_end = *self.cuboid.z.end().min(other.cuboid.z.end());

        // They may not overlap.
        if x_start > x_end || y_start > y_end || z_start > z_end {
            return None;
        }

        // Determine whether overlap will be on or off.
        let on = if self.on && other.on {
            false
        } else if !self.on && !other.on {
            true
        } else {
            // Use second state assuming we are flipping first.
            other.on
        };

        Some(Self {
            on,
            cuboid: Cuboid {
                x: x_start..=x_end,
                y: y_start..=y_end,
                z: z_start..=z_end,
            },
        })
    }

    // For the input, we consider the volume a negative volume if it's
    // off.
    fn volume(&self) -> isize {
        if self.on {
            self.cuboid.volume()
        } else {
            -self.cuboid.volume()
        }
    }
}

fn main() {
    let cuboids = include_str!("../input")
        .lines()
        .map(Input::new)
        .collect::<Vec<Input>>();

    // Part 1 - naive - check each cube in the cuboid.
    // let mut on = HashSet::new();
    // for cuboid in cuboids.iter() {
    //     if cuboid.ignore_part_one() {
    //         continue;
    //     }
    //     for x in cuboid.cuboid.x.clone() {
    //         for y in cuboid.cuboid.y.clone() {
    //             for z in cuboid.cuboid.z.clone() {
    //                 match cuboid.on {
    //                     true => on.insert(Point::new(x, y, z)),
    //                     false => on.remove(&Point::new(x, y, z)),
    //                 };
    //             }
    //         }
    //     }
    // }
    // println!("part 1: {}", on.len());

    // Part 1 using part 2 logic.
    let mut state: Vec<Input> = Vec::new();
    for cuboid in cuboids.iter() {
        if cuboid.ignore_part_one() {
            continue;
        }
        // Check our final state and see if there are any
        // intersections. If so, we'll want to include it in our final
        // state as well.
        for cur in state.clone().iter() {
            if let Some(intersected) = cur.intersect(cuboid) {
                state.push(intersected);
            }
        }

        // If this cube is on, we'll want to include it as well.
        if cuboid.on {
            state.push(cuboid.clone());
        }
    }

    // Now we just sum up the volumes.
    println!(
        "part 1: {}",
        state.iter().map(|c| c.volume()).sum::<isize>()
    );

    // Part 2 - We are basically tracking intersections
    // here. Initially, I thought of creating new cubes from the
    // intersections, but doing some research I realized I could just
    // track the volume which greatly simplified part 2.
    let mut state: Vec<Input> = Vec::new();
    for cuboid in cuboids.iter() {
        // Check our final state and see if there are any
        // intersections. If so, we'll want to include it in our final
        // state as well.
        for cur in state.clone().iter() {
            if let Some(intersected) = cur.intersect(cuboid) {
                state.push(intersected);
            }
        }

        // If this cube is on, we'll want to include it as well.
        if cuboid.on {
            state.push(cuboid.clone());
        }
    }

    // Now we just sum up the volumes.
    println!(
        "part 2: {}",
        state.iter().map(|c| c.volume()).sum::<isize>()
    );
}
