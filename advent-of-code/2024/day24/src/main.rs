use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Display;
use std::io::Write;

use nom::{
    branch::alt, bytes::complete::tag, character::complete::alphanumeric1, combinator::map, error,
    multi::separated_list1, sequence::tuple, IResult,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum GateType {
    And,
    Or,
    Xor,
}

impl Display for GateType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GateType::And => write!(f, "AND"),
            GateType::Or => write!(f, "OR"),
            GateType::Xor => write!(f, "XOR"),
        }
    }
}

impl From<&str> for GateType {
    fn from(s: &str) -> Self {
        match s {
            "AND" => GateType::And,
            "OR" => GateType::Or,
            "XOR" => GateType::Xor,
            _ => panic!("Unknown gate type: {}", s),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct Gate<'a> {
    a: &'a str,
    b: &'a str,
    q: &'a str,
    t: GateType,
}

impl<'a> Gate<'a> {
    fn new(a: &'a str, b: &'a str, t: GateType, q: &'a str) -> Self {
        Gate { a, b, t, q }
    }

    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        let (input, gate) = map(
            tuple((
                alphanumeric1,
                tag(" "),
                alt((tag("AND"), tag("OR"), tag("XOR"))),
                tag(" "),
                alphanumeric1,
                tag(" -> "),
                alphanumeric1,
            )),
            |(a, _, t, _, b, _, s)| Gate::new(a, b, t.into(), s),
        )(input)?;
        Ok((input, gate))
    }

    fn run(&self, a: bool, b: bool) -> bool {
        match self.t {
            GateType::And => a && b,
            GateType::Or => a || b,
            GateType::Xor => a ^ b,
        }
    }
}

#[derive(Debug)]
struct Wires<'a> {
    wires: HashMap<&'a str, bool>,
}

impl<'a> Wires<'a> {
    fn parse_bool(input: &'a str) -> IResult<&'a str, bool> {
        let (input, b) = alt((tag("0"), tag("1")))(input)?;
        Ok((input, b == "1"))
    }

    fn parse_wire(input: &'a str) -> IResult<&'a str, (&'a str, bool)> {
        let (input, (name, _, state)) =
            tuple((alphanumeric1, tag(": "), Wires::parse_bool))(input)?;
        Ok((input, (name, state)))
    }

    fn parse_all(input: &'a str) -> IResult<&'a str, Self> {
        let (input, wires) = separated_list1(tag("\r\n"), Wires::parse_wire)(input)?;
        let wires = wires.into_iter().collect();
        Ok((input, Self { wires }))
    }

    fn get(&self, wire: &'a str) -> Option<bool> {
        self.wires.get(wire).copied()
    }

    fn get_inputs(&self, a: &'a str, b: &'a str) -> Option<(bool, bool)> {
        let a = self.get(a)?;
        let b = self.get(b)?;
        Some((a, b))
    }

    fn set(&mut self, wire: &'a str, value: bool) {
        self.wires.insert(wire, value);
    }

    fn with_prefix(&self, prefix: &str) -> Vec<(&'a str, bool)> {
        self.wires
            .iter()
            .filter(|(k, _)| k.starts_with(prefix))
            .map(|(&k, &v)| (k, v))
            .collect()
    }
}

#[derive(Debug)]
struct Device<'a> {
    gates: Vec<Gate<'a>>,
    wires: Wires<'a>,
}

impl<'a> Device<'a> {
    fn new(input: &'a str) -> IResult<&'a str, Self> {
        let (input, wires) = Wires::parse_all(input)?;
        let (input, _) = tag("\r\n\r\n")(input)?;
        let (input, gates) = separated_list1(tag("\r\n"), Gate::parse)(input)?;
        Ok((input, Device { gates, wires }))
    }

    fn run(&mut self) {
        let mut changed = true;
        while changed {
            changed = false;
            for gate in &self.gates {
                if let Some((a, b)) = self.wires.get_inputs(gate.a, gate.b) {
                    // If we already have a value for this wire, skip it.
                    if let Some(_) = self.wires.get(gate.q) {
                        continue;
                    }
                    self.wires.set(gate.q, gate.run(a, b));
                    changed = true;
                }
            }
        }
    }

    fn z_as_usize(&self) -> usize {
        let mut wires = self.wires.with_prefix("z");
        wires.sort_by(|a, b| b.cmp(a));
        wires.into_iter().fold(0_usize, |mut acc, (_, z)| {
            acc = acc << 1;
            if z {
                acc |= 1;
            }
            acc
        })
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = include_str!("input.txt");

    // For part 1, simply run the device and find all the z wires and sort them.
    let now = std::time::Instant::now();
    let (_, mut device) = Device::new(input)?;
    device.run();
    let p1 = device.z_as_usize();
    println!("p1: {} ({:?})", p1, now.elapsed());

    let half_adder = vec![
        Gate::new("x00", "y00", GateType::Xor, "z00"),
        Gate::new("x00", "y00", GateType::And, "c00"),
    ];
    mermaid(&half_adder, 0, "half_adder.mmd")?;

    let full_adder = vec![
        Gate::new("x00", "y00", GateType::Xor, "v1"),
        Gate::new("c00", "v1", GateType::Xor, "z00"),
        Gate::new("x00", "y00", GateType::And, "v2"),
        Gate::new("c00", "v1", GateType::And, "v3"),
        Gate::new("v2", "v3", GateType::Or, "c001"),
    ];
    mermaid(&full_adder, 0, "full_adder.mmd")?;

    // print our actual input
    mermaid(&device.gates, 0, "circuit.mmd")?;

    Ok(())
}

fn get_class(name: &str) -> &'static str {
    if name.starts_with("x") || name.starts_with("y") {
        "INPUT"
    } else if name.starts_with("z") {
        "Z"
    } else {
        "VARIABLE"
    }
}

fn mermaid<'a>(gates: &Vec<Gate<'a>>, start: usize, filename: &str) -> std::io::Result<()> {
    let mut file = std::fs::File::create(filename)?;

    // Initialize the graph with some classes.
    writeln!(file, "graph TD;")?;
    writeln!(
        file,
        "classDef AND fill:#f38ba8,stroke:#11111b,stroke-width:2px;"
    )?;
    writeln!(
        file,
        "classDef OR fill:#a6e3a1,stroke:#11111b,stroke-width:2px;"
    )?;
    writeln!(
        file,
        "classDef XOR fill:#b4befe,stroke:#11111b,stroke-width:2px;"
    )?;
    writeln!(
        file,
        "classDef INPUT fill:#f9e2af,stroke:#11111b,stroke-width:2px;"
    )?;
    writeln!(
        file,
        "classDef Z fill:#f2cdcd,stroke:#11111b,stroke-width:2px;"
    )?;
    writeln!(
        file,
        "classDef VARIABLE fill:#74c7ec,stroke:#11111b,stroke-width:2px;"
    )?;

    // Start with all the x/y/z gates.
    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_back(format!("x{:02}", start).to_string());
    queue.push_back(format!("y{:02}", start).to_string());
    queue.push_back(format!("z{:02}", start).to_string());

    // Go through the queue. We use a queue here because mermaid will make it more readable
    // if we add the gates in the order they are connected.
    while let Some(wire) = queue.pop_front() {
        // Look for the gates that include this wire.
        for gate in gates
            .iter()
            .filter(|gate| *gate.a == wire || *gate.b == wire || *gate.q == wire)
        {
            // If we've already visited this gate, skip it.
            if visited.contains(gate) {
                continue;
            }
            visited.insert(gate);

            // Write the gate to the file.
            let (x, y, z, t) = (gate.a, gate.b, gate.q, gate.t);
            writeln!(file, "{}{}{}[{}];", x, y, z, t)?;
            writeln!(file, "class {}{}{} {};", x, y, z, t)?;
            writeln!(file, "{}:::{};", x, get_class(&x))?;
            writeln!(file, "{}:::{};", y, get_class(&y))?;
            writeln!(file, "{}:::{};", z, get_class(&z))?;
            writeln!(file, "{} --> {}{}{};", x, x, y, z)?;
            writeln!(file, "{} --> {}{}{};", y, x, y, z)?;
            writeln!(file, "{}{}{} --> {};", x, y, z, z)?;

            // Add the wires from this gate to the queue.
            queue.push_back(x.to_owned());
            queue.push_back(y.to_owned());
            queue.push_back(z.to_owned());
        }
    }

    Ok(())
}

//     // Check that x00/y00 is a half adder.  We sort of assume it's OK (turns out
//     // it is for my input). For a more general solution, we'd want to check this
//     // and potentially add it to the list.
//     let mut cin = check_half_adder(&device.gates, "x00", "y00", "z00");

//     // How many x's do we have? This will determine how many full adders we have.
//     let len = device.find_bits("x").len();

//     // Now loop through all the x/y bits and find the adders that are wrong.
//     let mut errors = Vec::new();
//     for i in 1..len {
//         let x = format!("x{:02}", i);
//         let y = format!("y{:02}", i);
//         let z = format!("z{:02}", i);
//         let cout = check_full_adder(&device.gates, &x, &y, cin, &z);
//         if cout.is_none() {
//             errors.push(i);
//         }
//         cin = cout;
//     }
//     println!("errors: {:?}", errors);

//     // If there are only 4 errors but we expect 4 pairs, this implies for my input
//     // each of the bad adders have two errors. Maybe they have swapped wires?
//     mermaid_errors(&device.gates, &errors)?;

//     let half_adder = vec![
//         Gate::Xor("x00", "y00", "z00"),
//         Gate::And("x00", "y00", "c00"),
//     ];
//     mermaid(&half_adder, 0, "half_adder.mmd")?;

//     let full_adder = vec![
//         Gate::Xor("x00", "y00", "v1"),
//         Gate::Xor("c00", "v1", "z00"),
//         Gate::And("x00", "y00", "v2"),
//         Gate::And("c00", "v1", "v3"),
//         Gate::Or("v2", "v3", "c001"),
//     ];
//     mermaid(&full_adder, 0, "full_adder.mmd")?;

//     // print our actual input
//     mermaid(&device.gates, 0, "circuit.mmd")?;

//     Ok(())
// }

// fn mermaid_errors<'a>(gates: &Vec<Gate<'a>>, errors: &[usize]) -> std::io::Result<()> {
//     for i in errors {
//         println!("Error at {}", i);
//         let x = format!("x{:02}", i);
//         let y = format!("y{:02}", i);
//         let z = format!("z{:02}", i);

//         // We need to get the previous carry wire.
//         let cin = check_full_adder(
//             gates,
//             format!("x{:02}", i - 1).as_str(),
//             format!("y{:02}", i - 1).as_str(),
//             None,
//             format!("z{:02}", i).as_str(),
//         )
//         .unwrap();

//         let error_gates = gates
//             .into_iter()
//             .filter(|gate| match gate {
//                 Gate::And(a, b, s) => {
//                     a == &x
//                         || a == &y
//                         || a == &z
//                         || b == &x
//                         || b == &y
//                         || b == &z
//                         || s == &x
//                         || s == &y
//                         || s == &z
//                         || x == cin
//                         || y == cin
//                         || z == cin
//                 }
//                 Gate::Or(a, b, s) => {
//                     a == &x
//                         || a == &y
//                         || a == &z
//                         || b == &x
//                         || b == &y
//                         || b == &z
//                         || s == &x
//                         || s == &y
//                         || s == &z
//                         || x == cin
//                         || y == cin
//                         || z == cin
//                 }
//                 Gate::Xor(a, b, s) => {
//                     a == &x
//                         || a == &y
//                         || a == &z
//                         || b == &x
//                         || b == &y
//                         || b == &z
//                         || s == &x
//                         || s == &y
//                         || s == &z
//                         || x == cin
//                         || y == cin
//                         || z == cin
//                 }
//             })
//             .cloned()
//             .collect::<Vec<_>>();

//         mermaid(&error_gates, *i, &format!("errors_{}.mdd", i))?;
//     }

//     Ok(())
// }

// // The adder logic should look like this:
// // a XOR b => v1
// // c XOR v1 = z
// // a AND b => v2
// // c AND v1 => v3
// // v2 OR v3 => v4
// // Output v4
// fn check_full_adder<'a>(
//     gates: &Vec<Gate<'a>>,
//     a: &str,
//     b: &str,
//     c: Option<&str>,
//     z: &str,
// ) -> Option<&'a str> {
//     // look for a XOR b -> v1
//     let v1 = match gates.iter().find_map(|gate| match gate {
//         Gate::Xor(x, y, z) if *x == a && *y == b => Some(z),
//         Gate::Xor(x, y, z) if *x == b && *y == a => Some(z),
//         _ => None,
//     }) {
//         Some(v1) => v1,
//         None => return None,
//     };

//     // look for c XOR v1 -> z
//     let z_actual = match gates.iter().find_map(|gate| match gate {
//         Gate::Xor(x, y, z)
//             if match c {
//                 Some(c) => *x == c,
//                 None => true,
//             } && y == v1 =>
//         {
//             Some(z)
//         }
//         Gate::Xor(x, y, z)
//             if match c {
//                 Some(c) => *y == c,
//                 None => true,
//             } && x == v1 =>
//         {
//             Some(z)
//         }
//         _ => None,
//     }) {
//         Some(z) => z,
//         None => return None,
//     };

//     // If we don't have the right z, it's broken.
//     if z != *z_actual {
//         return None;
//     }

//     // look for a AND b -> v2
//     let v2 = match gates.iter().find_map(|gate| match gate {
//         Gate::And(x, y, z) if *x == a && *y == b => Some(z),
//         Gate::And(x, y, z) if *x == b && *y == a => Some(z),
//         _ => None,
//     }) {
//         Some(v2) => v2,
//         None => return None,
//     };

//     // look for c AND v1 -> v3
//     let v3 = match gates.iter().find_map(|gate| match gate {
//         Gate::And(x, y, z)
//             if match c {
//                 Some(c) => *x == c,
//                 None => true,
//             } && y == v1 =>
//         {
//             Some(z)
//         }
//         Gate::And(x, y, z)
//             if x == v1
//                 && match c {
//                     Some(c) => *y == c,
//                     None => true,
//                 } =>
//         {
//             Some(z)
//         }
//         _ => None,
//     }) {
//         Some(v3) => v3,
//         None => return None,
//     };

//     // look for v2 OR v3 -> v4
//     match gates.iter().find_map(|gate| match gate {
//         Gate::Or(x, y, z) if x == v2 && y == v3 => Some(z),
//         Gate::Or(x, y, z) if x == v3 && y == v2 => Some(z),
//         _ => None,
//     }) {
//         Some(v4) => Some(v4),
//         None => None,
//     }
// }

// fn check_half_adder<'a>(gates: &Vec<Gate<'a>>, a: &str, b: &str, z: &str) -> Option<&'a str> {
//     // look for a XOR b -> z
//     let z_actual = match gates.iter().find_map(|gate| match gate {
//         Gate::Xor(x, y, z) if *x == a && *y == b => Some(z),
//         Gate::Xor(x, y, z) if *x == b && *y == a => Some(z),
//         _ => None,
//     }) {
//         Some(z) => z,
//         None => return None,
//     };

//     // If we don't have the right z, it's broken.
//     if z != *z_actual {
//         return None;
//     }

//     // look for a AND b -> c
//     match gates.iter().find_map(|gate| match gate {
//         Gate::And(x, y, c) if *x == a && *y == b => Some(c),
//         Gate::And(x, y, c) if *x == b && *y == a => Some(c),
//         _ => None,
//     }) {
//         Some(c) => Some(c),
//         None => None,
//     }
// }
