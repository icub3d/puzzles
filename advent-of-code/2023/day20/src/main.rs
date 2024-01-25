use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum Pulse {
    High,
    Low,
}

// A communication is a pulse from one module to another.
#[derive(Debug, Eq, PartialEq, Clone)]
struct Communication {
    source: String,
    destination: String,
    pulse: Pulse,
}

trait HandleCommunication {
    // Handle a given communication, update it's state, and return the
    // output communications.
    fn handle_communication(&mut self, communcation: &Communication) -> Vec<Communication>;
}

// State is used by the flip flop to track it's current state.
#[derive(Debug, Eq, PartialEq, Clone)]
enum State {
    On,
    Off,
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct FlipFlop {
    name: String,
    state: State,
    destinations: Vec<String>,
}

impl FlipFlop {
    fn new(name: String, destinations: Vec<String>) -> Self {
        FlipFlop {
            name,
            state: State::Off, // Defaults to off.
            destinations,
        }
    }
}

impl HandleCommunication for FlipFlop {
    fn handle_communication(&mut self, communication: &Communication) -> Vec<Communication> {
        match communication.pulse {
            // If we get a high pulse, we don't do anything.
            Pulse::High => {
                vec![]
            }
            // If we get a low pulse, we flip our state and send a
            // pulse to our destinations.
            Pulse::Low => match self.state {
                State::On => {
                    self.state = State::Off;
                    self.destinations
                        .iter()
                        .map(|d| Communication {
                            source: self.name.to_owned(),
                            destination: d.to_owned(),
                            pulse: Pulse::Low,
                        })
                        .collect()
                }
                State::Off => {
                    self.state = State::On;
                    self.destinations
                        .iter()
                        .map(|d| Communication {
                            source: self.name.to_owned(),
                            destination: d.to_owned(),
                            pulse: Pulse::High,
                        })
                        .collect()
                }
            },
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Conjunction {
    name: String,
    inputs: HashMap<String, Pulse>,
    destinations: Vec<String>,
}

impl Conjunction {
    fn new(name: String, inputs: Vec<String>, destinations: Vec<String>) -> Self {
        // Default all inputs to low.
        let inputs = inputs.into_iter().map(|i| (i, Pulse::Low)).collect();
        Conjunction {
            name,
            inputs,
            destinations,
        }
    }
}

impl HandleCommunication for Conjunction {
    fn handle_communication(&mut self, communication: &Communication) -> Vec<Communication> {
        // Update our inputs.
        self.inputs
            .insert(communication.source.clone(), communication.pulse);

        // Figure out what pulse to send. It will be low if all the
        // inputs are high.
        let pulse_to_send = if self.inputs.values().all(|p| *p == Pulse::High) {
            Pulse::Low
        } else {
            Pulse::High
        };

        // Send the pulse to our destinations.
        self.destinations
            .iter()
            .map(|d| Communication {
                source: self.name.to_owned(),
                destination: d.to_owned(),
                pulse: pulse_to_send,
            })
            .collect()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Broadcast {
    name: String,
    destinations: Vec<String>,
}

impl Broadcast {
    fn new(name: String, destinations: Vec<String>) -> Self {
        Broadcast { name, destinations }
    }
}

impl HandleCommunication for Broadcast {
    fn handle_communication(&mut self, communication: &Communication) -> Vec<Communication> {
        // Simply send the pulse to all of our destinations.
        self.destinations
            .iter()
            .map(|d| Communication {
                source: self.name.to_owned(),
                destination: d.to_owned(),
                pulse: communication.pulse,
            })
            .collect()
    }
}

#[allow(dead_code)]
struct Configuration {
    // Map of module name to module. The all implement
    // HandleCommunication which allows us to collect them like this.
    map: HashMap<String, Box<dyn HandleCommunication>>,

    // This is tracked for the mermaid output. Otherwise, not helpful
    // to the algorithm.
    modules: HashMap<String, Vec<String>>,
}

impl Configuration {
    fn parse_module(input: &str) -> (char, String, Vec<String>) {
        // The first part is the name, the second part is the
        // destinations.
        let parts = input.split(" -> ").collect::<Vec<_>>();

        // Determine the type from the first character. We'll remove
        // the first character from the name if it's not the
        // broadcaster.
        let (type_, name) = match parts[0].chars().next().unwrap() {
            'b' => ('b', parts[0].to_owned()),
            '%' => ('%', parts[0][1..].to_owned()),
            '&' => ('&', parts[0][1..].to_owned()),
            _ => panic!("Unknown type"),
        };

        // The destinations are just comma separated.
        let destinations = parts[1]
            .split(", ")
            .map(|s| s.to_owned())
            .collect::<Vec<_>>();
        (type_, name, destinations)
    }

    fn parse(input: &str) -> Configuration {
        let mut map: HashMap<String, Box<dyn HandleCommunication>> = HashMap::new();

        // Get all of our modules.
        let modules = input.lines().map(Self::parse_module).collect::<Vec<_>>();

        // We also want a list of inputs per destination, for the
        // conjunctions.
        let mut inputs: HashMap<String, Vec<String>> = HashMap::new();
        for (_, name, destinations) in &modules {
            for destination in destinations {
                inputs
                    .entry(destination.to_owned())
                    .or_default()
                    .push(name.to_owned());
            }
        }

        // For each module, create the appropriate type and add it to
        // our map.
        for (type_, name, destinations) in modules.clone() {
            let module: Box<dyn HandleCommunication> = match type_ {
                'b' => Box::new(Broadcast::new(name.to_owned(), destinations)),
                '%' => Box::new(FlipFlop::new(name.to_owned(), destinations)),
                '&' => Box::new(Conjunction::new(
                    name.to_owned(),
                    inputs.get(&name.to_owned()).unwrap().clone(),
                    destinations,
                )),
                _ => panic!("Unknown type {}", type_),
            };
            map.insert(name, module);
        }

        // We also want a list of destinations per module, for the
        // mermaid output.
        let modules = modules
            .into_iter()
            .map(|(_, name, destinations)| (name, destinations))
            .collect::<HashMap<_, _>>();

        Configuration { map, modules }
    }

    #[allow(dead_code)]
    fn mermaid(&self) {
        // The initial graph was a mess and there are loops. We'll do
        // a BFS but with seen nodes been tracked so we don't get
        // stuck in a loop.
        println!("graph TD;");
        let mut queue: VecDeque<String> = VecDeque::new();
        let mut seen = HashSet::new();

        // Out initial node is the broadcaster.
        queue.push_back("broadcaster".to_owned());
        while let Some(module) = queue.pop_front() {
            let destinations = match self.modules.get(&module) {
                Some(d) => d,
                // There are leaf nodes, like rx. We won't have
                // destinations for them and can stop at this node.
                None => continue,
            };
            for destination in destinations {
                // If we've already seen the (src, dest) pair, we can move on.
                if seen.contains(&(module.clone(), destination.clone())) {
                    continue;
                }
                println!("    {} --> {}", module, destination);

                // Add the destination to the queue so we can process it later.
                queue.push_back(destination.to_owned());

                // Mark the (src, dest) pair as seen.
                seen.insert((module.to_owned(), destination.to_owned()));
            }
        }
    }

    fn push<F>(&mut self, mut helper_fn: F) -> (usize, usize)
    where
        F: FnMut(&Communication),
    {
        // We are tracking lows and highs for part 1.
        let mut low = 0;
        let mut high = 0;

        // Keep track of all the work we have to do, starting with the
        // button being pushed. We use a VecDeque because the problem
        // states we need to handle communications in order. So we
        // want grab from the front and push to the back.
        let mut queue: VecDeque<Communication> = VecDeque::new();
        queue.push_back(Communication {
            source: "button".to_owned(),
            destination: "broadcaster".to_owned(),
            pulse: Pulse::Low,
        });

        // Loop through all the work until we are done.
        while let Some(communication) = queue.pop_front() {
            // Update our trackers.
            match communication.pulse {
                Pulse::High => high += 1,
                Pulse::Low => low += 1,
            }

            // If we don't have a module for the destination, we can move on.
            let module = match self.map.get_mut(&communication.destination) {
                Some(m) => m,
                None => {
                    continue;
                }
            };

            // Call our helper function for part 2.
            helper_fn(&communication);

            // Handle the communication and extend our queue with any
            // new communications.
            let new_communications = module.handle_communication(&communication);
            queue.extend(new_communications);
        }

        // We are done with all the signal handling, return our highs
        // and lows.
        (low, high)
    }
}

fn main() {
    // Parse our input.
    let input = std::fs::read_to_string("input").unwrap();
    let mut configuration = Configuration::parse(&input);

    // Part 1 - Bush the button 1_000 times.
    let (lows, highs) = (0..1_000)
        .map(|_| configuration.push(|_| ()))
        .fold((0, 0), |(l1, h1), (l2, h2)| (l1 + l2, h1 + h2));
    println!("p1: {:?}", lows * highs);

    // Part 2 - Some sort of cycle finding?
    let mut configuration = Configuration::parse(&input);
    // configuration.mermaid();

    // For my input, rx is attached to kj, which is a conjunction
    // box. The only way to get a low pulse from it is to have all of
    // it's input be high. It's inputs are ln, vn, zx, and dr which
    // are all conjunction boxes as well.
    //
    // Let's see if we can track when these get switched to high.
    let mut tracker: HashMap<String, Vec<usize>> = HashMap::new();
    for i in 1..=20_000 {
        let mut helper = |c: &Communication| {
            if [
                "ln".to_string(),
                "vn".to_string(),
                "zx".to_string(),
                "dr".to_string(),
            ]
            .contains(&c.source)
                && c.pulse == Pulse::High
            {
                tracker.entry(c.source.to_owned()).or_default().push(i);
            }
        };
        configuration.push(&mut helper);
    }

    // Sure enough, we have a cycle.
    dbg!(&tracker);

    // Print out the diffs to verify.
    tracker.iter().for_each(|(k, v)| {
        let mut last = 0;
        let mut diffs = vec![];
        for i in v {
            diffs.push(i - last);
            last = *i;
        }
        dbg!(k, diffs);
    });

    // Get the cycle lengths.
    let cycles = tracker
        .iter()
        .map(|(k, v)| (k, v[1] - v[0]))
        .collect::<Vec<_>>();
    dbg!(&cycles);

    // Calculate the LCM of the cycle lengths.
    let nums = cycles.iter().map(|(_, v)| *v).collect::<Vec<_>>();
    dbg!(&nums);
    let p2 = lcm(&nums);
    println!("p2: {}", p2);
}

// Pulled this from day 8.
//
// https://github.com/TheAlgorithms/Rust/blob/master/src/math/lcm_of_n_numbers.rs

pub fn lcm(nums: &[usize]) -> usize {
    if nums.len() == 1 {
        return nums[0];
    }
    let a = nums[0];
    let b = lcm(&nums[1..]);
    a * b / gcd_of_two_numbers(a, b)
}

fn gcd_of_two_numbers(a: usize, b: usize) -> usize {
    if b == 0 {
        return a;
    }
    gcd_of_two_numbers(b, a % b)
}
