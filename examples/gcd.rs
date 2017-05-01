fn GCD (mut a : i32, mut b : i32) -> i32 {
	while a != 0 {
		let t = a;
		a = b % a;
		b = t;
	}
	b
}

fn main() {
  (-2147483648) / (-1);
	let (a, b) = (readI32(), readI32());
	writeI32(GCD(a, b));
}
