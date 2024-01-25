fn main() {
    let min = 156218;
    let max = 652527;
    let mut p1 = 0;

    for i in min..=max {
        let mut prev = b'0';
        let mut double = false;
        let mut increase = true;
        for b in i.to_string().bytes() {
            if b < prev {
                increase = false;
                break;
            }
            if b == prev {
                double = true;
            }
            prev = b;
        }
        if double && increase {
            p1 += 1;
        }
    }
    println!("p1: {}", p1);

    let mut p2 = 0;
    for i in min..=max {
        let mut prev = b'0';
        let mut double = false;
        let mut increase = true;
        let mut count = 1;
        for b in i.to_string().bytes() {
            if b < prev {
                increase = false;
                break;
            }
            if b == prev {
                count += 1;
            } else {
                if count == 2 {
                    double = true;
                }
                count = 1;
            }
            prev = b;
        }
        if increase && (double || count == 2) {
            p2 += 1;
        }
    }
    println!("p2: {}", p2);
}
