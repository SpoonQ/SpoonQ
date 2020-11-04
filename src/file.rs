use crate::error::Error;
use std::boxed::Box;
use std::cell::RefCell;
use std::fmt;
use std::io::{Read, Seek, SeekFrom};
use std::iter::Iterator;
use std::rc::Weak;

#[allow(unused)] // TODO:
pub struct Location {
	begin_ln: usize,
	begin_ch: usize,
	end_ln: usize,
	end_ch: usize,
	file: Weak<RefCell<File>>,
}

#[test]
fn file_test() {
	let mut f = File::from_string("abcde");
	assert_eq!(f.get_ln(), 0);
	assert_eq!(f.get_ch(), 0);
	assert_eq!(f.get_index(), 0);
	assert_eq!(f.getc(), Some(b'a'));
	assert_eq!(f.getc(), Some(b'b'));
	assert_eq!(f.get_ln(), 0);
	assert_eq!(f.get_ch(), 2);
	assert_eq!(f.get_index(), 2);
	f.ungetc(b'b');
	f.ungetc(b'a');
	assert_eq!(f.getc(), Some(b'a'));
	assert_eq!(f.getc(), Some(b'b'));
	assert_eq!(f.get_ln(), 0);
	assert_eq!(f.get_ch(), 2);
	assert_eq!(f.get_index(), 2);
	assert_eq!(f.getc(), Some(b'c'));
	assert_eq!(f.getc(), Some(b'd'));
	assert_eq!(f.getc(), Some(b'e'));
	assert_eq!(f.getc(), None);
}

#[test]
fn file_test2() {
	let mut f = File::from_string("ab\r\n\n\rfg");
	assert_eq!(f.getc(), Some(b'a'));
	assert_eq!(f.ln, 0);
	assert_eq!(f.getc(), Some(b'b'));
	assert_eq!(f.ln, 0);
	assert_eq!(f.getc(), Some(b'\n'));
	assert_eq!(f.ln, 1);
	assert_eq!(f.get_lines(0).unwrap(), "ab");
	f.ungetc(b'\n');
	assert_eq!(f.ln, 0);
	assert_eq!(f.getc(), Some(b'\n'));
	assert_eq!(f.get_ch(), 0);
	assert_eq!(f.ln, 1);
	assert_eq!(f.getc(), Some(b'\n'));
	assert_eq!(f.ln, 2);
	assert_eq!(f.getc(), Some(b'\n'));
	assert_eq!(f.ln, 3);
	assert_eq!(f.getc(), Some(b'f'));
	assert_eq!(f.ln, 3);
	assert_eq!(f.getc(), Some(b'g'));
	assert_eq!(f.ln, 3);
	assert_eq!(f.getc(), None);
	assert_eq!(f.get_lines(0).unwrap(), "ab");
	assert_eq!(f.get_lines(1).unwrap(), "");
	assert_eq!(f.get_lines(2).unwrap(), "");
	assert_eq!(f.get_lines(3).unwrap(), "fg");
}

#[test]
fn file_test3() {
	let mut f = File::from_file("./Cargo.toml").unwrap();
	assert_eq!(f.getc(), Some(b'['));
	assert_eq!(f.getc(), Some(b'p'));
	assert_eq!(f.getc(), Some(b'a'));
	assert_eq!(f.getc(), Some(b'c'));
	assert_eq!(f.getc(), Some(b'k'));
	assert_eq!(f.getc(), Some(b'a'));
	assert_eq!(f.getc(), Some(b'g'));
	assert_eq!(f.getc(), Some(b'e'));
	assert_eq!(f.getc(), Some(b']'));
	assert_eq!(f.getc(), Some(b'\n'));
	assert_eq!(f.ln, 1);
	assert_eq!(f.get_lines(0).unwrap(), "[package]");
}

pub struct File {
	iter: Box<dyn Iterator<Item = u8>>,
	ln: usize,         // current line number
	heads: Vec<usize>, // index of line heads
	index: usize,      // current index
	unget_stack: Vec<u8>,
	name: String,
	raw_data: FileRawData,
}

enum FileRawData {
	Stream(std::fs::File),
	Array(Vec<u8>),
	None,
}

impl File {
	pub fn new(itr: Box<dyn Iterator<Item = u8>>) -> Self {
		File {
			iter: itr,
			index: 0,
			ln: 0,
			heads: vec![0],
			unget_stack: Vec::new(),
			name: String::new(),
			raw_data: FileRawData::None,
		}
	}

	pub fn from_string(s: &'static str) -> Self {
		let v = s.as_bytes();
		let mut ret = Self::new(Box::new(v.iter().cloned().to_owned()));
		ret.raw_data = FileRawData::Array(v.to_vec());
		ret.name = String::from("(inline)");
		ret
	}

	pub fn from_file(s: &str) -> Result<Self, Error> {
		let f = std::fs::File::open(s)
			.or_else(|e| Err(Error::from_io(format!("Opening file {}", s), e)))?;
		let k = f.try_clone();
		let mut ret = Self::new(Box::new(f.bytes().filter_map(|x| match x {
			Ok(o) => Some(o),
			_ => None,
		})));
		if let Ok(nf) = k {
			ret.raw_data = FileRawData::Stream(nf);
		}
		ret.name = String::from(s);
		Ok(ret)
	}

	pub fn set_ln(&mut self, x: usize) {
		self.ln = x
	}

	pub fn get_ln(&self) -> usize {
		self.ln
	}

	pub fn get_ch(&self) -> usize {
		self.index - self.heads[self.ln]
	}

	pub fn get_index(&self) -> usize {
		self.index
	}

	pub fn getc(&mut self) -> Option<u8> {
		let ret;
		if let Some(o) = self.unget_stack.pop() {
			ret = o;
		} else if let Some(o) = self.iter.next() {
			if o == b'\r' {
				ret = b'\n';
				if let Some(o) = self.iter.next() {
					if o == b'\n' {
						self.index += 1;
					} else {
						self.unget_stack.push(o);
					}
				}
			} else {
				ret = o;
			}
		} else {
			return None;
		}
		self.index += 1;
		if ret == b'\n' {
			self.ln += 1;
			if self.heads.len() == self.ln {
				self.heads.push(self.index);
			}
		}
		Some(ret)
	}

	pub fn ungetc(&mut self, c: u8) {
		self.unget_stack.push(c);
		assert!(self.index > 0);
		self.index -= 1;
		if c == b'\n' {
			assert!(self.ln > 0);
			self.ln -= 1;
		}
	}

	pub fn get_lines(&mut self, line: usize) -> Result<String, ()> {
		if line < self.heads.len() {
			let mut j = self.heads[line];
			let mut v = Vec::new();
			if let FileRawData::Stream(s) = &mut self.raw_data {
				if s.seek(SeekFrom::Start(j as u64)).is_err() {
					return Err(());
				}
				let mut ch: [u8; 1] = [0];
				while let Ok(_) = s.read_exact(&mut ch[..]) {
					if ch[0] == b'\r' || ch[0] == b'\n' {
						break;
					}
					v.push(ch[0]);
				}
			} else if let FileRawData::Array(a) = &self.raw_data {
				while j < a.len() {
					if a[j] == b'\r' || a[j] == b'\n' {
						break;
					}
					v.push(a[j]);
					j += 1;
				}
			} else {
				return Err(());
			}
			Ok(String::from_utf8(v).unwrap())
		} else {
			Err(())
		}
	}
}

impl fmt::Display for File {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.name)
	}
}

impl fmt::Debug for File {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.name)
	}
}
