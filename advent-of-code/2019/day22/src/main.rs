const INPUT: &str = include_str!("../input");

fn shuffle(deck: &mut Vec<usize>) {
    let len = deck.len();
    for line in INPUT.lines() {
        if line.starts_with("deal into new stack") {
            deck.reverse();
        } else if line.starts_with("cut") {
            let n: isize = line.split_whitespace().last().unwrap().parse().unwrap();
            if n > 0 {
                deck.rotate_left((n as usize) % len);
            } else {
                deck.rotate_right(n.unsigned_abs() % len);
            }
        } else if line.starts_with("deal with increment") {
            let n: usize = line.split_whitespace().last().unwrap().parse().unwrap();
            let mut new_deck = vec![0; deck.len()];
            let mut i = 0;
            for card in deck.iter() {
                new_deck[i] = *card;
                i = (i + n) % deck.len();
            }
            *deck = new_deck;
        }
    }
}
fn main() {
    let mut deck: Vec<usize> = (0..10007).collect();
    // let mut deck: Vec<i32> = (0..10).collect();
    shuffle(&mut deck);
    println!("p1: {}", deck.iter().position(|&x| x == 2019).unwrap());

    // part 2
    let deck_size = 119_315_717_514_047;
    let iterations = 101_741_582_076_661;
    let (mul, add) = get_mul_add_rev(deck_size);
    let mx = mod_exp(mul, iterations, deck_size);
    let pmx = (2020 * mx) % deck_size;
    let amx = (add * mx) % deck_size;
    let inv = multiplicative_inverse(mul - 1, deck_size);
    let mut p2 = (pmx + (amx - add) * inv) % deck_size;
    if p2 < 0 {
        p2 += deck_size;
    }
    println!("p2: {}", p2);
}

fn multiplicative_inverse(a: i128, m: i128) -> i128 {
    let mut t = 0;
    let mut nt = 1;
    let mut r = m;
    let mut nr = a;
    while nr != 0 {
        let q = r / nr;
        t -= q * nt;
        r -= q * nr;
        std::mem::swap(&mut t, &mut nt);
        std::mem::swap(&mut r, &mut nr);
    }
    if t < 0 {
        t += m;
    }
    t
}

fn mod_exp(mut base: i128, mut exp: i128, m: i128) -> i128 {
    if m == 1 {
        return 0;
    }
    let mut result = 1;
    base %= m;
    while exp > 0 {
        if exp % 2 == 1 {
            result = (result * base) % m;
        }
        exp >>= 1;
        base = (base * base) % m;
    }
    result
}

fn get_mul_add_rev(deck_size: i128) -> (i128, i128) {
    let mut mul = 1;
    let mut add = 0;
    for line in INPUT.lines().rev() {
        if line.starts_with("deal into new stack") {
            add += 1;
            mul = (mul * (deck_size - 1)) % deck_size;
            add = (add * (deck_size - 1)) % deck_size;
        } else if line.starts_with("cut") {
            let n: i128 = line.split_whitespace().last().unwrap().parse().unwrap();
            add =
                (add + match n < 0 {
                    true => deck_size + n,
                    false => n,
                }) % deck_size;
        } else if line.starts_with("deal with increment") {
            let n: i128 = line.split_whitespace().last().unwrap().parse().unwrap();
            let inv = multiplicative_inverse(n, deck_size);
            mul = (mul * inv) % deck_size;
            add = (add * inv) % deck_size;
        }
    }
    (mul, add)
}
