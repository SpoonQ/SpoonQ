use std::process::exit;

fn main() {
	let ret = spoon_q::cli::main(std::env::args());
	exit(ret);
}
