fn main() {
    let mut input = "1321131112".chars().collect::<Vec<char>>();
    for i in 0..50 {
        let mut next = vec![];
        let mut m = 0;
        let n = input.len();
        while m < n {
            let cur = input[m];
            let mut count = 1;
            m += 1;
            while m < n && input[m] == cur {
                count += 1;
                m += 1;
            }
            next.append(&mut format!("{count}").chars().collect());
            next.push(cur);
        }
        input = next;
        if i == 39 {
            println!("p1: {}", input.len());
        }
    }

    println!("p2: {}", input.len());
}
