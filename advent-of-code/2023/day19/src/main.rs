use std::collections::HashMap;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, digit1, one_of},
    combinator::map,
    multi::separated_list1,
    sequence::tuple,
    IResult,
};

// A destination describes either a final state (accept or reject) or
// the next workflow to consider.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Destination {
    Accept,
    Reject,
    Workflow(String),
}

impl Destination {
    fn parse(input: &str) -> IResult<&str, Destination> {
        let (input, destination) = alt((tag("A"), tag("R"), alpha1))(input)?;
        match destination {
            "A" => Ok((input, Destination::Accept)),
            "R" => Ok((input, Destination::Reject)),
            _ => Ok((input, Destination::Workflow(destination.to_string()))),
        }
    }
}

// A condition describes a comparison between a variable and a
// value. Each workflow will have some number of rules, some of which
// will have conditions to check.
#[derive(Debug, Copy, Clone)]
enum Condition {
    LessThan(char, isize),
    GreaterThan(char, isize),
    LessThanEqual(char, isize),
    GreaterThanEqual(char, isize),
}

impl Condition {
    // For part 2, during our tree traversal, any previous conditions
    // will turn into their opposites.
    fn opposite(&self) -> Self {
        match self {
            Self::LessThan(var, val) => Self::GreaterThanEqual(*var, *val),
            Self::GreaterThan(var, val) => Self::LessThanEqual(*var, *val),
            Self::LessThanEqual(var, val) => Self::GreaterThan(*var, *val),
            Self::GreaterThanEqual(var, val) => Self::LessThan(*var, *val),
        }
    }

    // We are simply evaluating the condition against the part here.
    fn evaluate(&self, part: Part) -> bool {
        match self {
            Self::LessThan(var, val) if part.value(var) < *val => true,
            Self::GreaterThan(var, val) if part.value(var) > *val => true,
            Self::LessThanEqual(var, val) if part.value(var) <= *val => true,
            Self::GreaterThanEqual(var, val) if part.value(var) >= *val => true,
            _ => false,
        }
    }

    fn parse(input: &str) -> IResult<&str, Condition> {
        let (input, (var, op, val)) = tuple((one_of("xmas"), one_of("<>"), digit1))(input)?;
        let val = val.parse().unwrap();
        let condition = match op {
            '<' => Condition::LessThan(var, val),
            '>' => Condition::GreaterThan(var, val),
            _ => unreachable!(),
        };
        Ok((input, condition))
    }
}

#[derive(Debug, Clone)]
enum Rule {
    Evaluation(Condition, Destination),
    Fallthrough(Destination),
}

impl Rule {
    fn evaluate(&self, part: Part) -> Option<Destination> {
        match self {
            // If it's an evaluation, we only return the destination
            // if the condition is true.
            Self::Evaluation(condition, destination) => {
                if condition.evaluate(part) {
                    Some(destination.clone())
                } else {
                    None
                }
            }

            // If it's a fallthrough, we always return the destination.
            Self::Fallthrough(destination) => Some(destination.clone()),
        }
    }

    fn parse(input: &str) -> IResult<&str, Rule> {
        alt((
            map(
                tuple((Condition::parse, tag(":"), Destination::parse)),
                |(c, _, d)| Rule::Evaluation(c, d),
            ),
            map(Destination::parse, Rule::Fallthrough),
        ))(input)
    }
}

#[derive(Debug, Clone)]
struct Workflow {
    name: String,
    rules: Vec<Rule>,
}

impl Workflow {
    fn parse(input: &str) -> IResult<&str, Workflow> {
        let (input, name) = alpha1(input)?;
        let name = name.to_string();
        let (input, _) = tag("{")(input)?;
        let (input, rules) = separated_list1(tag(","), Rule::parse)(input)?;
        let (input, _) = tag("}")(input)?;
        Ok((input, Workflow { name, rules }))
    }

    fn evaluate(&self, part: &Part) -> Destination {
        // Because of how we defined the rules, we can simply find the
        // first that evaluates to a Destination using find_map. We
        // can unwrap here because we assume all rules will have a
        // fallthrough.
        self.rules
            .iter()
            .find_map(|rule| rule.evaluate(part.clone()))
            .unwrap()
    }
}

#[derive(Debug, Clone)]
struct Part {
    x: isize,
    m: isize,
    a: isize,
    s: isize,
}

impl Part {
    // For part 1, it's a sum of the ratings.
    fn total_rating(&self) -> isize {
        self.x + self.m + self.a + self.s
    }

    // A helper functions to set the appropriate value of the part.
    fn set(&mut self, var: char, val: isize) {
        match var {
            'x' => self.x = val,
            'm' => self.m = val,
            'a' => self.a = val,
            's' => self.s = val,
            _ => unreachable!(),
        }
    }

    // A helper function to get the appropriate value of the part.
    fn value(&self, var: &char) -> isize {
        match var {
            'x' => self.x,
            'm' => self.m,
            'a' => self.a,
            's' => self.s,
            _ => unreachable!(),
        }
    }

    fn parse(input: &str) -> IResult<&str, Part> {
        let (input, _) = tag("{x=")(input)?;
        let (input, x) = digit1(input)?;
        let x = x.parse().unwrap();
        let (input, _) = tag(",m=")(input)?;
        let (input, m) = digit1(input)?;
        let m = m.parse().unwrap();
        let (input, _) = tag(",a=")(input)?;
        let (input, a) = digit1(input)?;
        let a = a.parse().unwrap();
        let (input, _) = tag(",s=")(input)?;
        let (input, s) = digit1(input)?;
        let s = s.parse().unwrap();
        let (input, _) = tag("}")(input)?;
        Ok((input, Part { x, m, a, s }))
    }

    fn process(&self, workflows: &HashMap<String, Workflow>) -> Destination {
        // We start at in.
        let mut cur = workflows.get("in").unwrap();
        loop {
            // Evaluate the function.
            match cur.evaluate(self) {
                // If we accept or rject, we are done.
                Destination::Accept => return Destination::Accept,
                Destination::Reject => return Destination::Reject,
                // Otherwise, we'll need to keep evaluating.
                Destination::Workflow(name) => {
                    cur = workflows.get(&name).unwrap();
                }
            }
        }
    }
}

fn generate_accepted_paths(
    workflows: &HashMap<String, Workflow>,
    cur: &str,
    parents: &[Condition],
) -> Vec<Vec<Condition>> {
    // We are going to recursively walk through the tree of
    // workflows. Parents is used to track the previous conditions we
    // encountered that got use to cur. Paths are the list of path's
    // that have ended in an accepted state.
    //
    // As we iterate through the rules, any previous rules are also
    // tracked. We keep the "opposite" of the rule because in order to
    // get to a current rule, the previous opposites would have to be
    // true.

    // This will be the list of paths that end in an accepted state
    // from this position.
    let mut paths = vec![];

    // We start at the current workflow.
    let workflow = workflows.get(cur).unwrap();
    let mut previous_conditions = vec![];

    // Each rule is a potential child of our parents.
    for rule in &workflow.rules {
        // We want to our new list of parents and include any previous
        // conditions we've encountered in previous rules.
        let mut new_parents = parents.to_vec();
        new_parents.extend(previous_conditions.clone());
        match rule {
            Rule::Evaluation(condition, dest) => {
                // We have a new condition to add.
                new_parents.push(*condition);

                // Add the opposite of the condition to the
                // previous_conditions so later rules can have them as
                // new parents.
                previous_conditions.push(condition.opposite());
                match dest {
                    Destination::Accept => {
                        // We found an acccepted path, so add it to
                        // the list.
                        paths.push(new_parents);
                    }
                    Destination::Reject => {}
                    Destination::Workflow(name) => {
                        // We need to keep going further down this
                        // branch.
                        paths.extend(generate_accepted_paths(workflows, name, &new_parents));
                    }
                }
            }
            Rule::Fallthrough(dest) => match dest {
                Destination::Accept => {
                    // We found an acccepted path, so add it to the
                    // list.
                    paths.push(new_parents);
                }
                Destination::Reject => {}
                Destination::Workflow(name) => {
                    // We need to keep going further down this branch.
                    paths.extend(generate_accepted_paths(workflows, name, &new_parents));
                }
            },
        }
    }
    paths
}

fn calculate_possible_combinations(path: &[Condition]) -> isize {
    // Track our minimum and maximum values.
    let mut min_part = Part {
        x: 1,
        m: 1,
        a: 1,
        s: 1,
    };
    let mut max_part = Part {
        x: 4000,
        m: 4000,
        a: 4000,
        s: 4000,
    };

    for condition in path {
        // For each condition, we reduce our max values or increase
        // our min values base on the given condition.
        match condition {
            Condition::LessThan(var, val) => {
                max_part.set(*var, max_part.value(var).min(val - 1));
            }
            Condition::LessThanEqual(var, val) => {
                max_part.set(*var, max_part.value(var).min(*val));
            }
            Condition::GreaterThan(var, val) => {
                min_part.set(*var, min_part.value(var).max(val + 1));
            }
            Condition::GreaterThanEqual(var, val) => {
                min_part.set(*var, min_part.value(var).max(*val));
            }
        }
    }

    // Potential off by one error here. Remember it's inclusive, so we
    // need to add one.
    (max_part.x - min_part.x + 1)
        * (max_part.m - min_part.m + 1)
        * (max_part.a - min_part.a + 1)
        * (max_part.s - min_part.s + 1)
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();

    // Collect the workflows and store them in a HashMap for easy
    // lookup.
    let (rest, workflows) = separated_list1(tag("\n"), Workflow::parse)(&input).unwrap();
    let workflows = workflows
        .iter()
        .map(|w| (w.name.clone(), w.clone()))
        .collect::<HashMap<String, Workflow>>();

    // The rest of the input is the parts.
    let parts = rest
        .trim()
        .split('\n')
        .map(|line| Part::parse(line).unwrap().1)
        .collect::<Vec<Part>>();

    // For part one, we simply process each part and sum the total
    // ratings.
    let now = std::time::Instant::now();
    let p1 = parts
        .iter()
        .filter(|p| p.process(&workflows) == Destination::Accept)
        .map(|p| p.total_rating())
        .sum::<isize>();
    println!("p1: {} ({:?})", p1, now.elapsed());

    // For part two, we now need to find all the possible values. We
    // are going to treat the workflows like a tree and walk through
    // it. We'll keep track of the paths that end in an accepted
    // state. We'll then calculate the possible combinations for each
    // path and sum them up.
    let now = std::time::Instant::now();
    let paths = generate_accepted_paths(&workflows, "in", &[]);
    let p2 = paths
        .iter()
        .map(|p| calculate_possible_combinations(p))
        .sum::<isize>();
    println!("p2: {} ({:?})", p2, now.elapsed());
}
