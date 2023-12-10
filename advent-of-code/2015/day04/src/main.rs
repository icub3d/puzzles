use std::sync::{atomic::AtomicBool, atomic::Ordering, Arc};
use tokio::sync::Barrier;

struct Search {
    barrier: Arc<Barrier>,
    p1: Arc<AtomicBool>,
    p2: Arc<AtomicBool>,
    input: String,
    n: usize,
    increment: usize,
}

impl Search {
    fn new(
        barrier: Arc<Barrier>,
        p1: Arc<AtomicBool>,
        p2: Arc<AtomicBool>,
        input: String,
        n: usize,
        increment: usize,
    ) -> Self {
        Self {
            barrier,
            p1,
            p2,
            input,
            n,
            increment,
        }
    }

    async fn search(&self) {
        for i in (self.n..).step_by(self.increment) {
            let secret = format!("{}{}", self.input, i);
            let digest = format!("{:x}", md5::compute(secret));
            if digest.starts_with("00000") && !self.p1.load(Ordering::SeqCst) {
                println!("p1: {}", i);
                self.p1.store(true, Ordering::SeqCst);
            }
            if digest.starts_with("000000") && !self.p2.load(Ordering::SeqCst) {
                println!("p2: {}", i);
                self.p2.store(true, Ordering::SeqCst);
            }
            if self.p1.load(Ordering::SeqCst) && self.p2.load(Ordering::SeqCst) {
                break;
            }
        }
        self.barrier.wait().await;
    }
}

#[tokio::main]
async fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let input = input.trim().to_string();
    let barrier = Arc::new(Barrier::new(1));
    let p1 = Arc::new(AtomicBool::new(false));
    let p2 = Arc::new(AtomicBool::new(false));
    for i in 0..10 {
        let barrier = barrier.clone();
        let input = input.clone();
        let p1 = p1.clone();
        let p2 = p2.clone();
        tokio::spawn(async move {
            let search = Search::new(barrier, p1, p2, input, i, 10);
            search.search().await;
        });
    }
    barrier.wait().await;
}
