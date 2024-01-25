#![allow(unused)]
#![allow(dead_code)]

use rayon::prelude::*;

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    // regexes are pretty overkill for this task,
    // but since I hadn't used them in Rust I wanted to try them out
    static ref RE_BP: Regex = Regex::new(r"Blueprint (?<id>\d+): (?<content>.*)").unwrap();
    static ref RE_RECIPE: Regex = Regex::new(r"Each (?<robotype>\w+) robot costs (?<res1>\d+\s\w+)( and (?<res2>\d+\s\w+))?").unwrap();
    static ref RE_RESOURCE: Regex = Regex::new(r"(?<amt>\d+)\s(?<res>\w+)").unwrap();
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct Resources {
    ore: usize,
    clay: usize,
    obsidian: usize,
    geode: usize,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct ResourcesDiff {
    // in contrast to Resources, these may get negative
    ore: isize,
    clay: isize,
    obsidian: isize,
    geode: isize,
}

use std::cmp::max;
impl ResourcesDiff {
    fn need(have: Resources, need: Resources) -> ResourcesDiff {
        ResourcesDiff {
            ore: need.ore as isize - have.ore as isize,
            clay: need.clay as isize - have.clay as isize,
            obsidian: need.obsidian as isize - have.obsidian as isize,
            geode: need.geode as isize - have.geode as isize,
        }
    }
}

use std::cmp::PartialOrd;
use std::ops::{Add, Sub};

impl PartialOrd for Resources {
    fn partial_cmp(&self, other: &Resources) -> Option<std::cmp::Ordering> {
        if self == other {
            return Some(std::cmp::Ordering::Equal);
        }
        if self.ore <= other.ore
            && self.clay <= other.clay
            && self.obsidian <= other.obsidian
            && self.geode <= other.geode
        {
            return Some(std::cmp::Ordering::Less);
        }
        if self.ore >= other.ore
            && self.clay >= other.clay
            && self.obsidian >= other.obsidian
            && self.geode >= other.geode
        {
            return Some(std::cmp::Ordering::Greater);
        }
        None
    }
}

impl Add for Resources {
    type Output = Resources;

    fn add(self, other: Resources) -> Resources {
        Resources {
            ore: self.ore + other.ore,
            clay: self.clay + other.clay,
            obsidian: self.obsidian + other.obsidian,
            geode: self.geode + other.geode,
        }
    }
}

impl Sub for Resources {
    type Output = Resources;

    fn sub(self, other: Resources) -> Resources {
        Resources {
            ore: self.ore - other.ore,
            clay: self.clay - other.clay,
            obsidian: self.obsidian - other.obsidian,
            geode: self.geode - other.geode,
        }
    }
}

impl Resources {
    fn new() -> Resources {
        Resources {
            ore: 0,
            clay: 0,
            obsidian: 0,
            geode: 0,
        }
    }

    fn add_one(self, resource_type: ResourceType) -> Resources {
        let mut result = self.clone();
        match (resource_type) {
            ResourceType::Ore => result.ore += 1,
            ResourceType::Clay => result.clay += 1,
            ResourceType::Obsidian => result.obsidian += 1,
            ResourceType::Geode => result.geode += 1,
        }
        result
    }

    fn max(&self, other: &Resources) -> Resources {
        Resources {
            ore: max(self.ore, other.ore),
            clay: max(self.clay, other.clay),
            obsidian: max(self.obsidian, other.obsidian),
            geode: max(self.geode, other.geode),
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
enum ResourceType {
    Ore,
    Clay,
    Obsidian,
    Geode,
}

impl ResourceType {
    fn parse(string: &str) -> Result<ResourceType, String> {
        match (string.to_lowercase().as_str()) {
            "ore" => Ok(ResourceType::Ore),
            "clay" => Ok(ResourceType::Clay),
            "obsidian" => Ok(ResourceType::Obsidian),
            "geode" => Ok(ResourceType::Geode),
            _ => Err(format!("Unknown robot type: {}", string)),
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
struct Recipe {
    robot: ResourceType,
    costs: Resources,
}

impl Recipe {
    fn parse(recipe: &str) -> Recipe {
        let caps = RE_RECIPE
            .captures(recipe)
            .expect("Recipe parsing is faulty :(");
        let robotype = ResourceType::parse(&caps["robotype"]).unwrap();
        let mut resources = Vec::new();
        resources.push(Self::parse_resource(&caps["res1"]));
        if let Some(matched) = caps.name("res2") {
            resources.push(Self::parse_resource(matched.as_str()));
        }
        let mut resources = resources.iter().fold(Resources::new(), |mut acc, res| {
            match res {
                Ok((ResourceType::Ore, amt)) => acc.ore += amt,
                Ok((ResourceType::Clay, amt)) => acc.clay += amt,
                Ok((ResourceType::Obsidian, amt)) => acc.obsidian += amt,
                Ok((ResourceType::Geode, amt)) => acc.geode += amt,
                Err(err) => {
                    panic!("Could not parse resource: {}", err)
                }
            }
            acc
        });
        Recipe {
            robot: robotype,
            costs: resources,
        }
    }
    fn parse_resource(string: &str) -> Result<(ResourceType, usize), String> {
        let caps = RE_RESOURCE
            .captures(string)
            .expect("Resource parsing is faulty");
        let amt = caps["amt"]
            .parse::<usize>()
            .expect("Could not parse amount");
        match (caps["res"].to_lowercase().as_str()) {
            "ore" => Ok((ResourceType::Ore, amt)),
            "clay" => Ok((ResourceType::Clay, amt)),
            "obsidian" => Ok((ResourceType::Obsidian, amt)),
            "geode" => Ok((ResourceType::Geode, amt)),
            _ => Err(format!("Unknown resource: {}", string)),
        }
    }
}

// This blueprint struct is unnecessary flexible as the type of resources
// is fixed for each robot type
#[derive(Debug, Hash, PartialEq, Eq)]
struct BluePrint {
    id: usize,
    recipes: Vec<Recipe>,
    max_spend: Resources,
}

impl BluePrint {
    fn new(id: usize, recipes: Vec<Recipe>) -> BluePrint {
        assert!(recipes.len() == 4);
        let max_spend = recipes
            .iter()
            .fold(Resources::new(), |mut acc, recipe| acc.max(&recipe.costs));
        BluePrint {
            id,
            recipes,
            max_spend,
        }
    }

    fn get_costs(&self, resource_type: ResourceType) -> &Resources {
        // hashmap wasn't really worth it for 4 elements
        for recipe in self.recipes.iter() {
            if recipe.robot == resource_type {
                return &recipe.costs;
            }
        }
        panic!(
            "Could not find recipe for resource type: {:?}",
            resource_type
        );
    }

    fn get_recipes(&self) -> &Vec<Recipe> {
        &self.recipes
    }

    fn parse(line: &str) -> BluePrint {
        use std::collections::HashMap;
        let caps = RE_BP.captures(line).expect("Regex failed to parse input.");
        let pbid: usize = caps["id"].parse().unwrap();
        let mut recipes = Vec::new();
        for recipestr in caps["content"].split(".") {
            if recipestr.len() <= 1 {
                continue;
            }
            let recipe = Recipe::parse(recipestr);
            recipes.push(recipe);
        }
        BluePrint::new(pbid, recipes)
    }

    fn calculate_quality_level(&self, maxstep: usize) -> usize {
        use std::collections::HashSet;
        // iterative DFS
        let mut queue = Vec::new();
        let mut seen: HashSet<DFSBranch> = HashSet::new();
        queue.push(DFSBranch::new());
        let mut best = 0;
        while let Some(branch) = queue.pop() {
            if seen.contains(&branch) {
                continue;
            }
            if branch.resources.geode > best {
                best = branch.resources.geode;
            }
            if branch.minute >= maxstep {
                continue;
            }
            // Very rough upper bound:
            // How many geodes would be end up with if we built
            // a new geode robot every minute from now on?
            let remaining = maxstep - branch.minute;
            let maxproducable = branch.robots.geode * (remaining)
                + (remaining * (remaining + 1)) / 2
                + branch.resources.geode;
            if maxproducable <= best {
                continue;
            }

            let mut newarms = branch.run_step(&self);
            for arm in newarms.into_iter() {
                queue.push(arm);
            }
            seen.insert(branch);
        }
        best
    }
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
struct DFSBranch {
    minute: usize,
    robots: Resources,
    resources: Resources,
}

impl DFSBranch {
    fn new() -> DFSBranch {
        let mut robots = Resources::new();
        robots.ore += 1;
        DFSBranch {
            minute: 0,
            robots,
            resources: Resources::new(),
        }
    }

    fn diff_fullfillable(&self, diff: &ResourcesDiff) -> bool {
        (diff.ore <= 0 || self.robots.ore > 0)
            && (diff.clay <= 0 || self.robots.clay > 0)
            && (diff.obsidian <= 0 || self.robots.obsidian > 0)
            && (diff.geode <= 0 || self.robots.geode > 0)
    }

    fn other_recipe_unlockable(&self, blueprint: &BluePrint) -> bool {
        // check whether we can unlock a further recipe if we just wait
        for recipe in blueprint.get_recipes() {
            let diff = ResourcesDiff::need(self.resources, recipe.costs);
            if !(recipe.costs <= self.resources) && self.diff_fullfillable(&diff) {
                return true;
            }
        }
        false
    }

    fn run_step(&self, blueprint: &BluePrint) -> Vec<DFSBranch> {
        // simulate all the possible action the arm can take this step
        let mut newarms: Vec<DFSBranch> = Vec::new();
        let mined_resources = self.robots;
        let minute = self.minute + 1;

        // We only accept 'do nothing' if the current production can
        // unlock a new recipe for us
        if self.other_recipe_unlockable(blueprint) {
            newarms.push(DFSBranch {
                minute,
                robots: self.robots,
                resources: self.resources + mined_resources,
            });
        }

        // branch out for every recipe we have sufficient resources
        for recipe in blueprint.get_recipes().iter() {
            let resourcetype = recipe.robot;
            let diff = ResourcesDiff::need(self.resources, recipe.costs);
            let newresources = self.resources + mined_resources;
            let should_build_more = match resourcetype {
                ResourceType::Ore => blueprint.max_spend.ore > self.robots.ore,
                ResourceType::Clay => blueprint.max_spend.clay > self.robots.clay,
                ResourceType::Obsidian => blueprint.max_spend.obsidian > self.robots.obsidian,
                ResourceType::Geode => true,
            };
            if should_build_more && recipe.costs <= self.resources {
                newarms.push(DFSBranch {
                    minute,
                    robots: self.robots.add_one(resourcetype),
                    resources: newresources - recipe.costs,
                });
            }
        }
        newarms
    }
}

fn part1(lines: &Vec<&str>) -> Option<i64> {
    Some(
        lines
            .par_iter()
            .map(|line| BluePrint::parse(line).calculate_quality_level(24))
            .enumerate()
            .map(|(i, x)| x * (i + 1))
            .sum::<usize>() as i64,
    )
}

fn part2(lines: &Vec<&str>) -> Option<i64> {
    Some(
        lines
            .par_iter()
            .take(3)
            .map(|line| BluePrint::parse(line).calculate_quality_level(32))
            .product::<usize>() as i64,
    )
}

fn main() {
    use std::env;
    use std::fs;
    use std::time::Instant;
    let contents = fs::read_to_string("../2022/day19/input").expect("Could not read in file");

    let lines: Vec<&str> = contents.lines().collect();

    // execute part 1 and part 2, print their results if they exist
    // later parts may follow, so we loop over the part functions
    let parts = [part1, part2];
    for (index, part) in parts.iter().enumerate() {
        let partstart = Instant::now();
        let result = part(&lines);
        match result {
            Some(result) => println!(
                "Part {}: {}\t({:?})",
                index + 1,
                result,
                partstart.elapsed()
            ),
            None => println!("Part {}: No result", index + 1),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    static TESTBLUEPRINTS: &str = "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.";

    #[test]
    fn test_part1() {
        let lines: Vec<&str> = TESTBLUEPRINTS.lines().collect();
        assert_eq!(part1(&lines), Some(33));
    }

    #[test]
    fn test_part2() {
        let lines: Vec<&str> = TESTBLUEPRINTS.lines().collect();
        assert_eq!(part2(&lines), Some(56 * 62));
    }
}
