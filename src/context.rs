use crate::file::File;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

#[allow(unused)]
pub struct Context {
	file: Rc<RefCell<File>>,
}

impl Context {
	pub fn new(f: File) -> Self {
		Self {
			file: Rc::new(RefCell::new(f)),
		}
	}

	pub fn ungetc(&mut self, c: u8) {
		self.file.borrow_mut().ungetc(c);
	}

	pub fn getc(&mut self) -> Option<u8> {
		if let Some(c) = self.file.borrow_mut().getc() {
			assert!(0x07 <= c && c <= 0x0D || c >= 0x20);
			Some(c)
		} else {
			None
		}
	}

	pub fn set_ln(&self, x: usize) {
		self.file.borrow_mut().set_ln(x)
	}

	pub fn get_ln(&self) -> usize {
		self.file.borrow_mut().get_ln()
	}

	pub fn get_loc(&self) -> (usize, usize, usize) {
		let f = self.file.borrow();
		(f.get_ln(), f.get_ch(), f.get_index())
	}

	pub fn get_weak_file(&mut self) -> Weak<RefCell<File>> {
		Rc::downgrade(&self.file)
	}
}
