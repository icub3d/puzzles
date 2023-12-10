use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Op {
    Add,
    Mul,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Token {
    Op(Op),
    OpenParen,
    CloseParen,
    Value(isize),
}

fn main() {
    let file = File::open("input").unwrap();
    let buf = BufReader::new(file);
    let aa: Vec<String> = buf.lines().map(|l| l.unwrap()).collect();
    let mut total = 0;
    let mut total2 = 0;
    for a in aa.iter() {
        let mut tokens = vec![];
        let mut num = vec![];
        for c in a.chars() {
            if c == ' ' {
                if num.len() > 0 {
                    tokens.push(Token::Value(
                        num.iter().collect::<String>().parse::<isize>().unwrap(),
                    ));
                }
                num = vec![];
            } else if c >= '0' && c <= '9' {
                num.push(c);
            } else if c == '+' {
                tokens.push(Token::Op(Op::Add));
            } else if c == '*' {
                tokens.push(Token::Op(Op::Mul));
            } else if c == '(' {
                tokens.push(Token::OpenParen);
            } else if c == ')' {
                if num.len() > 0 {
                    tokens.push(Token::Value(
                        num.iter().collect::<String>().parse::<isize>().unwrap(),
                    ));
                }
                num = vec![];
                tokens.push(Token::CloseParen);
            }
        }
        if num.len() > 0 {
            tokens.push(Token::Value(
                num.iter().collect::<String>().parse::<isize>().unwrap(),
            ));
        }
        let s = solve(&tokens);
        println!("{} {:?}", s, tokens);
        total += s;
        plus_paren(&mut tokens);
        let s = solve(&tokens);
        println!("{} {:?}", s, tokens);
        total2 += s;
    }
    println!("p1: {}", total);
    println!("p2: {}", total2);
}

fn plus_paren(tokens: &mut Vec<Token>) {
    let mut cur = 0;
    loop {
        if cur >= tokens.len() {
            break;
        }
        match tokens[cur] {
            Token::Op(Op::Add) => {
                // we need to put parens before this operation.
                match tokens[cur - 1] {
                    Token::CloseParen => {
                        // We need to find the associated open paren
                        // and then insert before it.
                        let mut depth = 0;
                        let mut r = cur;
                        loop {
                            match tokens[r] {
                                Token::CloseParen => depth += 1,
                                Token::OpenParen => {
                                    depth -= 1;
                                    if depth == 0 {
                                        break;
                                    }
                                }
                                _ => (),
                            }
                            r -= 1;
                        }
                        tokens.insert(r, Token::OpenParen);
                    }
                    // Default case is just to insert a paren.
                    _ => tokens.insert(cur - 1, Token::OpenParen),
                }
                cur += 1;

                // Now do the same for after.
                match tokens[cur + 1] {
                    Token::OpenParen => {
                        // We need to find the associated open paren
                        // and then insert before it.
                        let mut depth = 0;
                        let mut r = cur;
                        loop {
                            match tokens[r] {
                                Token::OpenParen => depth += 1,
                                Token::CloseParen => {
                                    depth -= 1;
                                    if depth == 0 {
                                        break;
                                    }
                                }
                                _ => (),
                            }
                            r += 1;
                        }
                        tokens.insert(r, Token::CloseParen);
                    }
                    // Default case is just to insert a paren.
                    _ => tokens.insert(cur + 2, Token::CloseParen),
                }
                cur += 1;
            }
            _ => (),
        }
        cur += 1;
    }
}

fn solve(tokens: &[Token]) -> isize {
    let mut cur = 0;
    let mut left: Option<isize> = None;
    let mut op: Op = Op::Add;
    loop {
        if cur >= tokens.len() {
            break;
        }
        match tokens[cur] {
            Token::Value(n) => {
                left = match left {
                    None => Some(n),
                    Some(m) => match op {
                        Op::Add => Some(m + n),
                        Op::Mul => Some(m * n),
                    },
                }
            }
            Token::Op(o) => op = o,
            Token::OpenParen => {
                let mut depth = 0;
                let mut r = cur;
                loop {
                    match tokens[r] {
                        Token::OpenParen => depth += 1,
                        Token::CloseParen => {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                        }
                        _ => (),
                    }
                    r += 1;
                }
                let s = solve(&tokens[cur + 1..r]);
                left = match left {
                    None => Some(s),
                    Some(m) => match op {
                        Op::Add => Some(m + s),
                        Op::Mul => Some(m * s),
                    },
                };
                cur = r;
            }
            Token::CloseParen => (),
        }
        cur += 1;
    }
    match left {
        None => 0,
        Some(n) => n,
    }
}
