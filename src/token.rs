use crate::compile::OperatorLevel;
use crate::context::Context;
use crate::error::Error;
use crate::file::File;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Weak;

#[test]
fn skip_blank_test() {
	use crate::file::File;
	let mut cxt = Context::new(File::from_string("  a#ab  c\rd#efg\r\nh#"));
	let mut tknzr = Tokenizer::new(&mut cxt);
	tknzr.skip_blank();
	assert_eq!(tknzr.cxt.getc(), Some(b'a'));
	tknzr.skip_blank();
	assert_eq!(tknzr.cxt.getc(), Some(b'd'));
	tknzr.skip_blank();
	assert_eq!(tknzr.cxt.getc(), Some(b'h'));
	tknzr.skip_blank();
	assert_eq!(tknzr.cxt.getc(), None);
}

#[test]
fn next_ident_test() {
	use crate::file::File;
	let mut cxt = Context::new(File::from_string("@1_abc123\n"));
	let mut tknzr = Tokenizer::new(&mut cxt);
	assert_eq!(tknzr.next_ident(), None);
	assert_eq!(tknzr.cxt.getc(), Some(b'@'));
	assert_eq!(tknzr.next_ident(), None);
	assert_eq!(tknzr.cxt.getc(), Some(b'1'));
	assert_eq!(tknzr.next_ident(), Some(b"_abc123".to_vec()));
	assert_eq!(tknzr.next_ident(), None);
	assert_eq!(tknzr.cxt.getc(), Some(b'\n'));
}

#[test]
fn next_token_test() {
	use crate::file::File;
	fn get_token(s: &'static str) -> Vec<Token> {
		let mut cxt = Context::new(File::from_string(s));
		let mut tknzr = Tokenizer::new(&mut cxt);
		let mut v = Vec::new();
		while let Some(tkn) = tknzr.next_token().unwrap() {
			v.push(tkn.clear_info());
		}
		v
	}
	assert_eq!(get_token("       \r\n\t #abv\r\n"), vec![]);
	assert_eq!(
		get_token("#\r123#4567\r\n"),
		vec![Token::NumLiteral(TokenNumLiteral(123, None))]
	);
	assert_eq!(
		get_token("1234567"),
		vec![Token::NumLiteral(TokenNumLiteral(1234567, None))]
	);
	assert_eq!(
		get_token("4294967295"),
		vec![Token::NumLiteral(TokenNumLiteral(4294967295, None))]
	);
	assert_eq!(
		get_token("00000"),
		vec![Token::NumLiteral(TokenNumLiteral(0, None))]
	);
	assert_eq!(
		get_token("	0x123  "),
		vec![Token::NumLiteral(TokenNumLiteral(0x123, None))]
	);
	assert_eq!(
		get_token(" 0o_1_37   "),
		vec![Token::NumLiteral(TokenNumLiteral(0o137, None))]
	);
	assert_eq!(
		get_token("0b_1_01 "),
		vec![Token::NumLiteral(TokenNumLiteral(0b101, None))]
	);
	assert_eq!(
		get_token("123."),
		vec![Token::FloatingLiteral(TokenFloatingLiteral(123., None))]
	);
	assert_eq!(
		get_token("0.125"),
		vec![Token::FloatingLiteral(TokenFloatingLiteral(0.125, None))]
	);
	assert_eq!(
		get_token("123e3"),
		vec![Token::FloatingLiteral(TokenFloatingLiteral(123000.0, None))]
	);
	assert_eq!(
		get_token("123e+3"),
		vec![Token::FloatingLiteral(TokenFloatingLiteral(123000.0, None))]
	);
	assert_eq!(
		get_token("123E-3"),
		vec![Token::FloatingLiteral(TokenFloatingLiteral(0.123, None))]
	);
	assert_eq!(
		get_token("	@abcd#f"),
		vec![Token::Hash(TokenHash(b"abcd".to_vec(), None))]
	);
	assert_eq!(
		get_token("	define"),
		vec![Token::Keyword(TokenKeyword(Keyword::Define, None))]
	);
	assert_eq!(
		get_token("	print"),
		vec![Token::Keyword(TokenKeyword(Keyword::Print, None))]
	);
	assert_eq!(
		get_token("->"),
		vec![Token::Operator(TokenOperator(Operator::RightArrow, None))]
	);
	assert_eq!(
		get_token("<<-"),
		vec![
			Token::Operator(TokenOperator(Operator::Less, None)),
			Token::Operator(TokenOperator(Operator::LeftArrow, None))
		]
	);
	assert_eq!(
		get_token("|||"),
		vec![
			Token::Operator(TokenOperator(Operator::Or, None)),
			Token::Operator(TokenOperator(Operator::Pipe, None))
		]
	);
	assert_eq!(
		get_token("\"hello\\n\\\\\""),
		vec![Token::StringLiteral(TokenStringLiteral(
			b"hello\n\\".to_vec(),
			None
		))]
	);
}

pub struct Tokenizer<'a> {
	cxt: &'a mut Context,
	unget_stack: Vec<Token>,
	tinfo: Option<TokenInfo>,
}

impl<'a> Tokenizer<'a> {
	pub fn new(cxt: &'a mut Context) -> Self {
		Tokenizer {
			cxt,
			unget_stack: Vec::new(),
			tinfo: None,
		}
	}

	pub fn get_last_tinfo(&self) -> Option<TokenInfo> {
		self.tinfo.clone()
	}

	pub fn eat_if(&mut self, t: Token) -> Result<bool, Error> {
		if let Some(tkn) = self.next_token()? {
			let o = tkn.clone().clear_info();
			if t == o {
				return Ok(true);
			} else {
				self.unget_token(tkn);
			}
		}
		return Ok(false);
	}

	pub fn eat_ident(&mut self) -> Result<TokenIdent, Error> {
		let tkn = self.next_token()?;
		if let Some(Token::Ident(s)) = tkn {
			Ok(s)
		} else if let Some(o) = tkn {
			err!(("Expected ident, found {}", o), [self.tinfo.clone()])
		} else {
			err!(("Expected ident"), [self.tinfo.clone()])
		}
	}

	pub fn eat(&mut self, t: Token) -> Result<(), Error> {
		let r = self.eat_if(t.clone())?;
		if !r {
			if let Some(tkn) = self.next_token()? {
				err!(("Expected {}, found {}", t, tkn), [self.tinfo.clone()])
			} else {
				err!(("Expect {} in last", t))
			}
		} else {
			Ok(())
		}
	}

	pub fn skip_this_line(&mut self) -> Result<(), Error> {
		let ln = self.cxt.get_ln();
		loop {
			self.skip_blank();
			if self.cxt.get_ln() > ln {
				break;
			}
			let tkn = self.next_token()?;
			if tkn == None {
				break;
			}
		}
		Ok(())
	}

	fn skip_blank(&mut self) {
		while let Some(c) = self.cxt.getc() {
			match c {
				b' ' | b'\t' | b'\n' => continue,
				b'#' => {
					while let Some(c) = self.cxt.getc() {
						if c == b'\n' {
							break;
						}
					}
					continue;
				}
				_ => self.cxt.ungetc(c),
			}
			break;
		}
	}

	fn next_number(&mut self, base: u32) -> u32 {
		let mut val = 0;
		while let Some(c) = self.cxt.getc() {
			if c == b'_' {
				continue;
			}
			let dig = match c {
				b'0'..=b'9' => (c - b'0') as u32,
				b'A'..=b'F' => (c - b'A' + 10) as u32,
				b'a'..=b'f' => (c - b'a' + 10) as u32,
				_ => base,
			};
			if dig >= base {
				self.cxt.ungetc(c);
				break;
			}
			val = val * base + dig;
		}
		val
	}

	fn next_ident(&mut self) -> Option<Vec<u8>> {
		if let Some(c) = self.cxt.getc() {
			if let b'A'..=b'Z' | b'a'..=b'z' | b'_' = c {
				let mut s = Vec::new();
				s.push(c);
				while let Some(c) = self.cxt.getc() {
					if let b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'_' = c {
						s.push(c);
					} else {
						self.cxt.ungetc(c);
						break;
					}
				}
				return Some(s);
			}
			self.cxt.ungetc(c);
		}
		None
	}

	pub fn next_token_inline(&mut self) -> Result<Option<Token>, Error> {
		let ln = self.cxt.get_ln();
		let tkn = self.next_token()?;
		if ln != self.cxt.get_ln() {
			tkn.map(|t| self.unget_token(t));
			self.cxt.set_ln(ln);
			Ok(None)
		} else {
			Ok(tkn)
		}
	}

	fn get_tokeninfo(&mut self, loc_begin: (usize, usize, usize)) -> Option<TokenInfo> {
		let (ln0, ch0, idx0) = loc_begin;
		let (_, _, idx1) = self.cxt.get_loc();
		self.tinfo = Some(TokenInfo {
			file: self.cxt.get_weak_file(),
			ln: ln0,
			ch: ch0,
			count: idx1 - idx0,
		});
		self.tinfo.clone()
	}

	pub fn get_next_number(&mut self) -> Result<i32, Error> {
		let mut token = self.next_token()?;
		let mut is_minus = false;
		if let Some(Token::Operator(TokenOperator(Operator::Minus, _))) = token.clone() {
			is_minus = true;
			token = self.next_token()?;
		}
		if let Some(Token::NumLiteral(TokenNumLiteral(n, _))) = token {
			let n = n as i32;
			Ok([n, -n][is_minus as usize])
		} else {
			return err!(("Expected num literal"), [self.tinfo.clone()]);
		}
	}

	pub fn next_token(&mut self) -> Result<Option<Token>, Error> {
		if let Some(o) = self.unget_stack.pop() {
			Ok(Some(o))
		} else {
			self.skip_blank();
			let loc = self.cxt.get_loc();
			if let Some(c) = self.cxt.getc() {
				match c {
					b'"' => {
						let mut v = Vec::new();
						loop {
							let c = match self.cxt.getc() {
								None | Some(b'\n') => {
									return err!(
										("String literal ended unexpectedly"),
										[self.get_tokeninfo(loc)]
									);
								}
								Some(b'\\') => match self.cxt.getc() {
									None => {
										return err!(
											("String literal ended unexpectedly"),
											[self.get_tokeninfo(loc)]
										);
									}
									Some(b'n') => b'\n',
									Some(b't') => b'\t',
									Some(b'r') => b'\r',
									Some(b'"') => b'"',
									Some(o) => o,
								},
								Some(b'"') => {
									break;
								}
								Some(o) => o,
							};
							v.push(c);
						}
						Ok(Some(Token::StringLiteral(TokenStringLiteral(
							v,
							self.get_tokeninfo(loc),
						))))
					}
					b'@' => {
						let ident = self.next_ident().unwrap();
						let tinfo = self.get_tokeninfo(loc);
						Ok(Some(Token::Hash(TokenHash(ident, tinfo))))
					}
					b'0'..=b'9' => {
						let mut base = 10;
						if c == b'0' {
							if let Some(c) = self.cxt.getc() {
								match c {
									b'x' | b'X' => base = 16,
									b'b' | b'B' => base = 2,
									b'o' | b'O' => base = 8,
									_ => {
										self.cxt.ungetc(c);
									}
								}
							}
						} else {
							self.cxt.ungetc(c);
						}
						let val = self.next_number(base);
						if let Some(c) = self.cxt.getc() {
							let mut val = val as f64;
							let mut power = base;
							if let b'e' | b'E' | b'p' | b'P' = c {
								let exp_base: i32 = if let b'e' | b'E' = c { 10 } else { 16 };
								let suff = if let Some(c) = self.cxt.getc() {
									match c {
										b'+' => 1,
										b'-' => -1,
										_ => {
											self.cxt.ungetc(c);
											1
										}
									}
								} else {
									1
								};
								let exp = self.next_number(10);
								if suff == 1 {
									val = val * (exp_base.pow(exp) as f64);
								} else {
									val = val / (exp_base.pow(exp) as f64);
								}
								let tinfo = self.get_tokeninfo(loc);
								return Ok(Some(Token::FloatingLiteral(TokenFloatingLiteral(
									val, tinfo,
								))));
							} else if c == b'.' {
								while let Some(c) = self.cxt.getc() {
									if c == b'_' {
										continue;
									}
									let dig = match c {
										b'0'..=b'9' => (c - b'0') as u32,
										b'A'..=b'F' => (c - b'A' + 10) as u32,
										b'a'..=b'f' => (c - b'a' + 10) as u32,
										_ => base,
									};
									if dig >= base {
										self.cxt.ungetc(c);
										break;
									}
									val = val + dig as f64 / power as f64;
									power = power * base;
								}
								let tinfo = self.get_tokeninfo(loc);
								return Ok(Some(Token::FloatingLiteral(TokenFloatingLiteral(
									val, tinfo,
								))));
							} else {
								self.cxt.ungetc(c);
							}
						}
						let tinfo = self.get_tokeninfo(loc);
						Ok(Some(Token::NumLiteral(TokenNumLiteral(val, tinfo))))
					}
					b'A'..=b'Z' | b'a'..=b'z' | b'_' => {
						self.cxt.ungetc(c);
						let ident = self.next_ident().unwrap();
						let tinfo = self.get_tokeninfo(loc);
						if let Some(k) = crate::token::KEYWORD_TABLE.get(&ident.to_vec()) {
							Ok(Some(Token::Keyword(TokenKeyword(*k, tinfo))))
						} else {
							Ok(Some(Token::Ident(TokenIdent(ident, tinfo))))
						}
					}
					_ => {
						let mut v = vec![c];
						let mut res = None;
						let mut slen = 1;
						loop {
							let mut cnt = 0;
							for item in OPERATOR_TABLE.as_slice() {
								let (s, k, _) = item;
								let mut i = 0;
								while i < v.len() && i < s.len() && v[i] == s[i] {
									i = i + 1;
								}
								if i == v.len() {
									cnt = cnt + 1;
									if i == s.len() {
										let tinfo = self.get_tokeninfo(loc);
										res = Some(Token::Operator(TokenOperator(*k, tinfo)));
										slen = s.len();
									}
								}
							}
							if cnt == 0 {
								break;
							}
							if let Some(c) = self.cxt.getc() {
								v.push(c);
							} else {
								break;
							}
						}
						while slen < v.len() {
							self.cxt.ungetc(v.pop().unwrap());
						}
						if let Some(o) = res {
							Ok(Some(o))
						} else {
							let tinfo = self.get_tokeninfo(loc);
							err!(
								("Cannot recognize char '{}'", String::from_utf8_lossy(&v)),
								[tinfo]
							)
						}
					}
				}
			} else {
				return Ok(None);
			}
		}
	}

	pub fn unget_token(&mut self, tk: Token) {
		self.unget_stack.push(tk);
	}
}

#[derive(Clone)]
pub struct TokenInfo {
	pub file: Weak<RefCell<File>>,
	pub ln: usize,
	pub ch: usize,
	pub count: usize,
}

impl std::hash::Hash for TokenInfo {
	// All hash-implemented structs do not depend on TokenInfo
	fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
}

impl std::cmp::Eq for TokenInfo {}
impl std::cmp::PartialEq for TokenInfo {
	fn eq(&self, _other: &Self) -> bool {
		true
	}
}

impl std::fmt::Debug for TokenInfo {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		// let file = self.file.upgrade().unwrap();
		write!(f, "TokenInfo({}, {}, {})", self.ln, self.ch, self.count)
	}
}

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
	Hash(TokenHash),
	NumLiteral(TokenNumLiteral),
	FloatingLiteral(TokenFloatingLiteral),
	StringLiteral(TokenStringLiteral),
	Ident(TokenIdent),
	Keyword(TokenKeyword),
	Operator(TokenOperator),
}

impl std::fmt::Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Self::Hash(o) => write!(f, "{}", o),
			Self::NumLiteral(o) => write!(f, "{}", o),
			Self::FloatingLiteral(o) => write!(f, "{}", o),
			Self::StringLiteral(o) => write!(f, "{}", o),
			Self::Ident(o) => write!(f, "{}", o),
			Self::Keyword(o) => write!(f, "{}", o),
			Self::Operator(o) => write!(f, "{}", o),
		}
	}
}

impl Token {
	pub fn clear_info(self) -> Self {
		match self {
			Self::Hash(to) => Self::Hash(to.clear_info()),
			Self::NumLiteral(to) => Self::NumLiteral(to.clear_info()),
			Self::FloatingLiteral(to) => Self::FloatingLiteral(to.clear_info()),
			Self::StringLiteral(to) => Self::StringLiteral(to.clear_info()),
			Self::Ident(to) => Self::Ident(to.clear_info()),
			Self::Keyword(to) => Self::Keyword(to.clear_info()),
			Self::Operator(to) => Self::Operator(to.clear_info()),
		}
	}
}

#[derive(Eq, Clone)]
pub struct TokenHash(pub Vec<u8>, pub Option<TokenInfo>);
impl std::hash::Hash for TokenHash {
	// All hash-implemented structs do not depend on TokenInfo
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		self.0.hash(state);
	}
}
impl std::cmp::PartialEq for TokenHash {
	fn eq(&self, other: &Self) -> bool {
		self.0.eq(&other.0)
	}
}
#[derive(PartialEq, Clone, Debug)]
pub struct TokenNumLiteral(pub u32, pub Option<TokenInfo>);
#[derive(PartialEq, Clone, Debug)]
pub struct TokenFloatingLiteral(pub f64, pub Option<TokenInfo>);
#[derive(PartialEq, Clone, Debug)]
pub struct TokenStringLiteral(pub Vec<u8>, pub Option<TokenInfo>);
#[derive(Clone)]
pub struct TokenIdent(pub Vec<u8>, pub Option<TokenInfo>);
impl std::cmp::PartialEq for TokenIdent {
	fn eq(&self, other: &Self) -> bool {
		self.0.eq(&other.0)
	}
}
#[derive(PartialEq, Clone, Debug)]
pub struct TokenKeyword(pub Keyword, pub Option<TokenInfo>);
#[derive(PartialEq, Clone, Debug)]
pub struct TokenOperator(pub Operator, pub Option<TokenInfo>);

impl TokenHash {
	pub fn clear_info(self) -> Self {
		TokenHash(self.0, None)
	}
}
impl TokenNumLiteral {
	pub fn clear_info(self) -> Self {
		TokenNumLiteral(self.0, None)
	}
}
impl TokenFloatingLiteral {
	pub fn clear_info(self) -> Self {
		TokenFloatingLiteral(self.0, None)
	}
}
impl TokenStringLiteral {
	pub fn clear_info(self) -> Self {
		TokenStringLiteral(self.0, None)
	}
}
impl TokenIdent {
	pub fn clear_info(self) -> Self {
		TokenIdent(self.0, None)
	}
}
impl TokenKeyword {
	pub fn clear_info(self) -> Self {
		TokenKeyword(self.0, None)
	}
}
impl TokenOperator {
	pub fn clear_info(self) -> Self {
		TokenOperator(self.0, None)
	}
}

impl std::fmt::Debug for TokenHash {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"TokenHash({})",
			String::from_utf8(self.0.clone()).unwrap()
		)
	}
}

impl std::fmt::Debug for TokenIdent {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"TokenIdent({})",
			String::from_utf8(self.0.clone()).unwrap()
		)
	}
}

impl std::fmt::Display for TokenHash {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}", String::from_utf8(self.0.clone()).unwrap())
	}
}
impl std::fmt::Display for TokenNumLiteral {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}", self.0)
	}
}
impl std::fmt::Display for TokenFloatingLiteral {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}", self.0)
	}
}
impl std::fmt::Display for TokenStringLiteral {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "String")
	}
}
impl std::fmt::Display for TokenIdent {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}", String::from_utf8(self.0.clone()).unwrap())
	}
}
impl std::fmt::Display for TokenKeyword {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}", self.0)
	}
}
impl std::fmt::Display for TokenOperator {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}", self.0)
	}
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Keyword {
	Define,
	Different,
	False,
	Len,
	Let,
	Maximize,
	Minimize,
	Print,
	Solve,
	True,
	Type,
}

impl std::fmt::Display for Keyword {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		<Self as std::fmt::Debug>::fmt(self, f)
	}
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Operator {
	LeftRightArrow,
	Equal,
	NotEq,
	DefineAs,
	LeftArrow,
	RightArrow,
	LessEq,
	GreatEq,
	Or,
	And,
	Spread,
	Less,
	Great,
	Not,
	Plus,
	Minus,
	Multi,
	Divi,
	OpenSq,
	CloseSq,
	OpenParen,
	CloseParen,
	OpenBracket,
	CloseBracket,
	Pipe,
	Comma,
	Colon,
	Semicolon,
}

impl std::fmt::Display for Operator {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		<Self as std::fmt::Debug>::fmt(self, f)
	}
}

fn get_vec(a: &[u8]) -> Vec<u8> {
	let mut v = Vec::new();
	v.extend_from_slice(a);
	v
}

#[allow(unused)] // TODO:
pub enum OperatorKind {
	Pre,
	Post,
	Binary,
}

pub fn operator_check_level(op: Operator, kind: OperatorKind, level: OperatorLevel) -> bool {
	for (_, pq, a) in OPERATOR_TABLE.as_slice() {
		if op == *pq {
			let l = match kind {
				OperatorKind::Pre => a[0],
				OperatorKind::Binary => a[1],
				OperatorKind::Post => a[2],
			};
			if let Some(l) = l {
				return l == level;
			} else {
				return false;
			}
		}
	}
	unreachable!();
}

lazy_static! {
	pub static ref OPERATOR_TABLE: Vec<(Vec<u8>, Operator, [Option<OperatorLevel>; 3])> = {
		let mut l = Vec::new();
		l.push((
			get_vec(&b"<->"[..]),
			Operator::LeftRightArrow,
			[None, Some(OperatorLevel::ConditionalCompare), None],
		));
		l.push((
			get_vec(&b"=="[..]),
			Operator::Equal,
			[None, Some(OperatorLevel::ConditionalCompare), None],
		));
		l.push((
			get_vec(&b"!="[..]),
			Operator::NotEq,
			[None, Some(OperatorLevel::ConditionalCompare), None],
		));
		l.push((get_vec(&b":="[..]), Operator::DefineAs, [None, None, None]));
		l.push((
			get_vec(&b"<-"[..]),
			Operator::LeftArrow,
			[None, Some(OperatorLevel::ConditionalCompare), None],
		));
		l.push((
			get_vec(&b"->"[..]),
			Operator::RightArrow,
			[None, Some(OperatorLevel::ConditionalCompare), None],
		));
		l.push((
			get_vec(&b"<="[..]),
			Operator::LessEq,
			[None, Some(OperatorLevel::ConditionalCompare), None],
		));
		l.push((
			get_vec(&b">="[..]),
			Operator::GreatEq,
			[None, Some(OperatorLevel::ConditionalCompare), None],
		));
		l.push((
			get_vec(&b"&&"[..]),
			Operator::And,
			[None, Some(OperatorLevel::ConditionalAnd), None],
		));
		l.push((
			get_vec(&b"||"[..]),
			Operator::Or,
			[None, Some(OperatorLevel::ConditionalOr), None],
		));
		l.push((get_vec(&b".."[..]), Operator::Spread, [None, None, None]));
		l.push((
			get_vec(&b"<"[..]),
			Operator::Less,
			[None, Some(OperatorLevel::ConditionalCompare), None],
		));
		l.push((
			get_vec(&b">"[..]),
			Operator::Great,
			[None, Some(OperatorLevel::ConditionalCompare), None],
		));
		l.push((
			get_vec(&b"!"[..]),
			Operator::Not,
			[Some(OperatorLevel::ConditionalUnary), None, None],
		));
		l.push((
			get_vec(&b"+"[..]),
			Operator::Plus,
			[
				Some(OperatorLevel::AlgebraicUnary),
				Some(OperatorLevel::AlgebraicAddSub),
				None,
			],
		));
		l.push((
			get_vec(&b"-"[..]),
			Operator::Minus,
			[
				Some(OperatorLevel::AlgebraicUnary),
				Some(OperatorLevel::AlgebraicAddSub),
				None,
			],
		));
		l.push((
			get_vec(&b"*"[..]),
			Operator::Multi,
			[None, Some(OperatorLevel::AlgebraicMulDiv), None],
		));
		l.push((
			get_vec(&b"/"[..]),
			Operator::Divi,
			[None, Some(OperatorLevel::AlgebraicMulDiv), None],
		));
		l.push((get_vec(&b"["[..]), Operator::OpenSq, [None, None, None]));
		l.push((get_vec(&b"]"[..]), Operator::CloseSq, [None, None, None]));
		l.push((get_vec(&b"("[..]), Operator::OpenParen, [None, None, None]));
		l.push((get_vec(&b")"[..]), Operator::CloseParen, [None, None, None]));
		l.push((
			get_vec(&b"{"[..]),
			Operator::OpenBracket,
			[None, None, None],
		));
		l.push((
			get_vec(&b"}"[..]),
			Operator::CloseBracket,
			[None, None, None],
		));
		l.push((get_vec(&b"|"[..]), Operator::Pipe, [None, None, None]));
		l.push((get_vec(&b","[..]), Operator::Comma, [None, None, None]));
		l.push((get_vec(&b":"[..]), Operator::Colon, [None, None, None]));
		l.push((get_vec(&b";"[..]), Operator::Semicolon, [None, None, None]));
		l
	};
}

lazy_static! {
	pub static ref KEYWORD_TABLE: HashMap<Vec<u8>, Keyword> = {
		let mut m = HashMap::new();
		m.insert(get_vec(&b"define"[..]), Keyword::Define);
		m.insert(get_vec(&b"different"[..]), Keyword::Different);
		m.insert(get_vec(&b"len"[..]), Keyword::Len);
		m.insert(get_vec(&b"let"[..]), Keyword::Let);
		m.insert(get_vec(&b"maximize"[..]), Keyword::Maximize);
		m.insert(get_vec(&b"minimize"[..]), Keyword::Minimize);
		m.insert(get_vec(&b"print"[..]), Keyword::Print);
		m.insert(get_vec(&b"solve"[..]), Keyword::Solve);
		m.insert(get_vec(&b"type"[..]), Keyword::Type);
		m.insert(get_vec(&b"true"[..]), Keyword::True);
		m.insert(get_vec(&b"false"[..]), Keyword::False);
		m
	};
}
