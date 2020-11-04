use crate::cond::Cond;
use crate::error::Error;
use crate::token::{Token, TokenIdent, Tokenizer};

#[test]
fn dimacsgenerator_test_file_test() {
	use crate::context::Context;
	use crate::file::File;
	let mut cxt = Context::new(File::from_string("p cnf 0 0\n"));
	let mut tknzr = Tokenizer::new(&mut cxt);
	assert!(DimacsGenerator::test_file(&mut tknzr).unwrap());
	assert_eq!(
		tknzr.next_token().unwrap(),
		Some(Token::Ident(TokenIdent(b"p".to_vec(), None)))
	);
	assert_eq!(
		tknzr.next_token().unwrap(),
		Some(Token::Ident(TokenIdent(b"cnf".to_vec(), None)))
	);

	let mut cxt = Context::new(File::from_string("p abc 0 0\n"));
	let mut tknzr = Tokenizer::new(&mut cxt);
	assert!(!DimacsGenerator::test_file(&mut tknzr).unwrap());
	assert_eq!(
		tknzr.next_token().unwrap(),
		Some(Token::Ident(TokenIdent(b"p".to_vec(), None)))
	);
	assert_eq!(
		tknzr.next_token().unwrap(),
		Some(Token::Ident(TokenIdent(b"abc".to_vec(), None)))
	);
}

#[test]
fn dimacsgenerator_test() {
	use crate::context::Context;
	use crate::file::File;
	// let mut cxt = Context::new(File::from_string("p cnf 3 4\n"));
	// let mut tknzr = Tokenizer::new(&mut cxt);
	// assert_eq!(DimacsGenerator::generate_cond(&mut tknzr).unwrap(), None);
	let mut cxt = Context::new(File::from_string(
		"p cnf 4 4\n1 2 0\n-1 3 -4 0\n2 3 0\n1 -4 0\n",
	));
	let mut tknzr = Tokenizer::new(&mut cxt);
	assert_eq!(
		DimacsGenerator::generate_cond(&mut tknzr).unwrap(),
		Some(Cond::And(vec![
			Cond::Or(vec![Cond::Val(1), Cond::Val(2)]),
			Cond::Or(vec![
				Cond::Not(Box::new(Cond::Val(1))),
				Cond::Val(3),
				Cond::Not(Box::new(Cond::Val(4))),
			]),
			Cond::Or(vec![Cond::Val(2), Cond::Val(3)]),
			Cond::Or(vec![Cond::Val(1), Cond::Not(Box::new(Cond::Val(4)))])
		]))
	);
}

pub struct DimacsGenerator {}

impl DimacsGenerator {
	fn get_magic_tokens() -> [Token; 2] {
		[
			Token::Ident(TokenIdent(b"p".to_vec(), None)),
			Token::Ident(TokenIdent(b"cnf".to_vec(), None)),
		]
	}
	pub fn test_file(tknzr: &mut Tokenizer) -> Result<bool, Error> {
		let mut stack = Vec::new();
		let mut i = 0;
		let magic_tokens = Self::get_magic_tokens();
		while i < magic_tokens.len() {
			let token = tknzr.next_token()?;
			if let Some(token) = token {
				if !(magic_tokens[i] == token) {
					stack.push(token);
					break;
				}
				stack.push(token);
				i += 1;
			} else {
				break;
			}
		}
		let ret = i == magic_tokens.len();
		while let Some(token) = stack.pop() {
			tknzr.unget_token(token);
		}
		return Ok(ret);
	}

	pub fn generate_cond(tknzr: &mut Tokenizer) -> Result<Option<Cond>, Error> {
		for t in &Self::get_magic_tokens() {
			tknzr.eat(t.clone())?;
		}
		let mut nums = Vec::new();
		for _ in 0..2 {
			let n = tknzr.get_next_number()?;
			if n < 0 {
				return err!(("Invalid number"), [tknzr.get_last_tinfo()]);
			}
			nums.push(n as usize);
		}
		let [values, clauses] = [nums[0], nums[1]];
		let mut ret = Vec::new();
		for _ in 0..clauses {
			tknzr.skip_this_line()?; // go to next line
			let mut clause = Vec::new();
			loop {
				let n = tknzr.get_next_number()?;
				if n == 0 {
					break;
				} else if n.abs() > values as i32 {
					return err!(("Invalid number"), [tknzr.get_last_tinfo()]);
				}
				let mut val = Cond::Val(n.abs() as usize);
				if n < 0 {
					val = Cond::Not(Box::new(val));
				}
				clause.push(val);
			}
			if clause.len() > 0 {
				ret.push(Cond::Or(clause))
			}
		}
		if ret.len() > 0 {
			Ok(Some(Cond::And(ret)))
		} else {
			Ok(None)
		}
	}
}
