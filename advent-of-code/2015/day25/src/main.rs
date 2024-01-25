fn main() {
    let row = 2981;
    let col = 3075;

    let mut code: usize = 20151125;
    let mut r = 1;
    let mut c = 1;
    while r != row || c != col {
        code = (code * 252533) % 33554393;
        if r == 1 {
            r = c + 1;
            c = 1;
        } else {
            r -= 1;
            c += 1;
        }
    }
    println!("p1: {}", code);
}
