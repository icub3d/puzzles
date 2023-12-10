#[derive(Debug)]
struct Decoder {
	/// All of the bits.
	v: Vec<u8>,

	/// The current position in the vector.
	pos: usize,
}

impl Decoder {
	fn new(v: &[u8]) -> Self {
		Self {
			v: v.to_vec(),
			pos: 0,
		}
	}

	fn take(&mut self, n: usize) -> Vec<u8> {
		let v = &self.v[self.pos..self.pos + n];
		self.pos += n;
		v.to_vec()
	}

	fn take_literal(&mut self) -> usize {
		let mut pieces = vec![];
		loop {
			let tt = self.take(5);
			pieces.extend(tt.iter().skip(1));
			if tt[0] == 0 {
				// We are done
				break;
			}
		}
		pieces.numerify()
	}
}

/// Numerify simplifies turning a vector of bits into an actual number.
trait Numerify {
	fn numerify(&self) -> usize;
}

impl Numerify for Vec<u8> {
	fn numerify(&self) -> usize {
		let mut pv = 1;
		let mut n: usize = 0;
		// We basically just loop through the values and multiply them
		// by their place value.
		for x in self.iter().rev().map(|x| *x as usize) {
			n += x * pv;
			pv *= 2; // increment place value.
		}
		n
	}
}

/// These are our different packet types.
#[derive(Debug, Eq, PartialEq)]
enum PacketType {
	Literal(usize),
	Sum,
	Product,
	Minimum,
	Maximum,
	GreaterThan,
	LessThan,
	EqualTo,
	Unknown,
}

#[derive(Debug, Eq, PartialEq)]
struct Packet {
	// The version number of the packet.
	version: usize,

	// The type of packet.
	packet_type: PacketType,

	// All of this packets child packets.
	children: Vec<Packet>,
}

impl Packet {
	fn new(d: &mut Decoder) -> Self {
		// Grab the version number.
		let version = d.take(3).numerify();

		// Get the type.
		let packet_type = match d.take(3).numerify() {
			0 => PacketType::Sum,
			1 => PacketType::Product,
			2 => PacketType::Minimum,
			3 => PacketType::Maximum,
			4 => PacketType::Literal(d.take_literal()),
			5 => PacketType::GreaterThan,
			6 => PacketType::LessThan,
			7 => PacketType::EqualTo,
			_ => PacketType::Unknown,
		};

		let mut s = Self {
			version,
			packet_type,
			children: vec![],
		};
		s.get_children(d);
		s
	}

	// A helper functions to pull all the children.
	fn get_children(&mut self, d: &mut Decoder) {
		// Literal won't have children.
		if let PacketType::Literal(_) = self.packet_type {
			return;
		}

		// Get the mode and length.
		let mode = d.take(1)[0];
		let len = match mode {
			0 => d.take(15).numerify(),
			_ => d.take(11).numerify(),
		};
		if mode == 0 {
			// In this case, we want to take len bits and keep
			// decoding packets until we have exhausted all the bits.
			let bits = d.take(len);
			let mut dd = Decoder::new(&bits);
			while dd.pos < len {
				self.children.push(Packet::new(&mut dd));
			}
		} else {
			// In this case, we want to decode len packets, so we just
			// do that in a for loop.
			for _ in 0..len {
				let p = Packet::new(d);
				self.children.push(p);
			}
		}
	}

	fn version_sum(&self) -> usize {
		// The version sum is this version number plus the sum of all
		// it's children.
		self.version + self.children.iter().map(|c| c.version_sum()).sum::<usize>()
	}

	fn calculate(&self) -> usize {
		// These are all fairly straight forward operations. We can
		// use iterators the do the calculations over children and
		// then just use if blocks for the comparisons.
		match self.packet_type {
			PacketType::Literal(u) => u,
			PacketType::Sum => self.children.iter().map(|c| c.calculate()).sum::<usize>(),
			PacketType::Product => self
				.children
				.iter()
				.map(|c| c.calculate())
				.product::<usize>(),
			PacketType::Minimum => self.children.iter().map(|c| c.calculate()).min().unwrap(),
			PacketType::Maximum => self.children.iter().map(|c| c.calculate()).max().unwrap(),
			PacketType::GreaterThan => {
				if self.children[0].calculate() > self.children[1].calculate() {
					1
				} else {
					0
				}
			}
			PacketType::LessThan => {
				if self.children[0].calculate() < self.children[1].calculate() {
					1
				} else {
					0
				}
			}
			PacketType::EqualTo => {
				if self.children[0].calculate() == self.children[1].calculate() {
					1
				} else {
					0
				}
			}
			PacketType::Unknown => panic!("unknown packet type"),
		}
	}
}

fn main() {
	// Collect all the bits and ones and zeros
	let input = include_str!("../input").lines().next().unwrap();
	let bits = input
		.chars()
		.flat_map(move |c| {
			// Convert to decimal and turn into it's constituent bits. I'm
			// thinking this will be the best representation because we
			// are pulling non-standard groups of bits at a time.
			let h = c.to_digit(16).unwrap();
			let v = format!("{:0>4b}", h)
				.chars()
				.map(|c| c.to_digit(10).unwrap() as u8)
				.collect::<Vec<u8>>();
			v
		})
		.collect::<Vec<u8>>();

	// We create a decoder to track where we are in the bit stream.
	let mut decoder = Decoder::new(&bits);

	// We then create a message from that bit stream.
	let message = Packet::new(&mut decoder);

	// Print out the solutions.
	println!("p1: {:?}", message.version_sum());
	println!("p2: {:?}", message.calculate());
}

#[cfg(test)]
mod tests {
	use super::*;

	// I used these to check my work.
	#[test]
	fn numerify() {
		assert_eq!(vec![0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1].numerify(), 2021);
	}
	#[test]
	fn packet_number() {
		let mut d = Decoder::new(&vec![
			1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0,
		]);
		let exp = Packet {
			version: 6,
			packet_type: PacketType::Literal(2021),
			taken: 24,
			children: vec![],
		};
		assert_eq!(Packet::new(&mut d), exp);
	}
}
