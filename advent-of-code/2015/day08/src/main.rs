fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let mut code = 0;
    let mut encode = 0;
    let mut memory = 0;
    for line in input.lines() {
        code += 2;
        encode += 6;
        let cc = line.chars().collect::<Vec<char>>();
        let mut cur = 1;
        let max = cc.len() - 2;
        loop {
            if cur >= max {
                break;
            }
            cur += match cc[cur] {
                '\\' => match cc[cur + 1] {
                    'x' => {
                        code += 4;
                        memory += 1;
                        encode += 5;
                        4
                    }
                    '"' | '\\' => {
                        code += 2;
                        memory += 1;
                        encode += 4;
                        2
                    }
                    _ => panic!("slash with bad next val"),
                },
                _ => {
                    code += 1;
                    memory += 1;
                    encode += 1;
                    1
                }
            };
        }
    }

    println!("p1: {}", code - memory);
    println!("p2: {}", encode - code);
}
