fn GCD (mut a : i32, mut b : i32) -> i32 {
	while a != 0 {
		let t = b;
		a = b % a;
		b = t;
	}
	b
}

fn main() {
	let (a, b) = (readI32(), readI32());
	writeI32(GCD(a, b));
}
