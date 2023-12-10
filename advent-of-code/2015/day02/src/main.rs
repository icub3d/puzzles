
struct Box {
	l: usize,
	w: usize,
	h: usize,
}

impl Box {
	fn new (s: &str) -> Self {
		let dims = s.split('x').map(|x| x.parse::<usize>().unwrap()).collect::<Vec<usize>>();
		Box{l: dims[0], w: dims[1], h: dims[2]}
	}

	fn paper_needed(&self) -> usize {
		let sides = vec![self.l *self.w, self.w * self.h, self.h*self.l];
		let extra = *sides.iter().min().unwrap();
		extra + sides.iter().map(|s| s*2).sum::<usize>()
	}

	fn ribbon_needed(&self) -> usize {
		let sides = vec![self.l+self.w, self.w + self.h, self.h+self.l];
		let sides = sides.iter().map(|s| s*2).collect::<Vec<usize>>();
		let smallest = *sides.iter().min().unwrap();
		self.l * self.w * self.h + smallest
	}
}

fn main() {
	let input = std::fs::read_to_string("input").unwrap();
	let paper = input.lines().map(Box::new).map(|b| b.paper_needed()).sum::<usize>();
	println!("p1: {}", paper);

	let ribbon = input.lines().map(Box::new).map(|b| b.ribbon_needed()).sum::<usize>();
	println!("p2: {}", ribbon);
}
