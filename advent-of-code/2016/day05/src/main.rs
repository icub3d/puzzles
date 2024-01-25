use md5::{Digest, Md5};

fn main() {
    let input = "wtnhxymk";
    let mut password = String::new();
    let mut cur = 0;
    while password.len() < 8 {
        let mut hasher = Md5::new();
        hasher.update(format!("{}{}", input, cur));
        let result = format!("{:x}", hasher.finalize());
        if result.starts_with("00000") {
            println!("{}: {}", cur, result);
            password.push(result.chars().nth(5).unwrap());
        }
        cur += 1;
    }
    println!("p1: {}", password);

    let mut password = [' '; 8];
    let mut cur = 0;
    while password.iter().any(|&c| c == ' ') {
        let mut hasher = Md5::new();
        hasher.update(format!("{}{}", input, cur));
        let result = format!("{:x}", hasher.finalize());
        if result.starts_with("00000") {
            let pos = match result.chars().nth(5).unwrap().to_digit(10) {
                Some(d) => d,
                _ => {
                    cur += 1;
                    continue;
                }
            };
            println!("{}: {} {} ", cur, result, pos);
            if pos < 8 && password[pos as usize] == ' ' {
                password[pos as usize] = result.chars().nth(6).unwrap();
                println!("{}: {}", cur, password.iter().collect::<String>());
            }
        }
        cur += 1;
    }
    println!("p2: {}", password.iter().collect::<String>());
}
