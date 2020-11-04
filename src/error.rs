use crate::token::TokenInfo;
use std::boxed::Box;
use std::rc::Rc;

#[macro_export]
macro_rules! err_inner {
	($errtyp:ident, ( $fmt:expr $(, $($arg:tt)+)?) $(, [ $( $loc:expr ),+ ] )?) => (
		{
			use $crate::error::{Error, ErrorKind, ErrorDebugInfo};
			let m = std::format!($fmt$(, $($arg)+)?);
			let mut temp_err = Error::new(ErrorKind::$errtyp(m.to_string()));
			$(
				$(
					if let Some(l) = $loc {
						temp_err.tokens.push(l);
					}
				)+
			)?
			temp_err.dbginfo = std::option::Option::Some(ErrorDebugInfo {
				file: file!().to_string(),
				ln: line!() as usize,
			});
			std::result::Result::Err(temp_err)
		}
	);
}

#[macro_export]
macro_rules! err {
	(($fmt:expr $(, $($arg:tt)+)?) $(, [ $( $loc:expr ),+ ] )?) => (
		err_inner!(Error, ($fmt$(, $($arg)+)?) $(, [ $( $loc),+ ] )?)
	);
}

#[macro_export]
macro_rules! warn {
	(($fmt:expr $(, $($arg:tt)+)?) $(, [ $( $loc:expr ),+ ] )?) => (
		err_inner!(Warning, ($fmt$(, $($arg)+)?) $(, [ $( $loc),+ ] )?)
	);
}

#[macro_export]
macro_rules! info {
	(($fmt:expr $(, $($arg:tt)+)?) $(, [ $( $loc:expr ),+ ] )?) => (
		err_inner!(Info, ($fmt$(, $($arg)+)?) $(, [ $( $loc ),+ ] )?)
	);
}

#[macro_export]
macro_rules! fatal {
	(($fmt:expr $(, $($arg:tt)+)?) $(, [ $( $loc:expr ),+ ] )?) => (
		err_inner!(Fatal, ($fmt$(, $($arg)+)?) $(, [ $( $loc),+ ] )?)
	);
}

#[derive(Clone, Debug)]
pub struct Error {
	//	locations: Vec<Location>,
	pub kind: ErrorKind,
	pub tokens: Vec<TokenInfo>,
	pub dbginfo: Option<ErrorDebugInfo>,
}

#[derive(Clone, Debug)]
pub struct ErrorDebugInfo {
	pub file: String,
	pub ln: usize,
}

impl Error {
	pub fn new(etyp: ErrorKind) -> Self {
		Error {
			kind: etyp,
			tokens: Vec::new(),
			dbginfo: None,
		}
	}

	pub fn show(e: Result<(), Error>) -> Result<(), std::io::Error> {
		let e = e.unwrap_err();
		e.describe()
	}

	pub fn not_implemented_error() -> Self {
		Self::new(ErrorKind::Error("Not Implemented".to_string()))
	}

	pub fn from_io(m: String, e: std::io::Error) -> Self {
		Self::new(ErrorKind::System(m, Rc::new(Box::new(e))))
	}

	pub fn from_other(m: String, e: Box<dyn std::error::Error>) -> Self {
		Self::new(ErrorKind::System(m, Rc::new(e)))
	}

	pub fn from_empty_list() -> Self {
		Self::new(ErrorKind::List(Vec::new()))
	}

	pub fn count(&self) -> (usize, usize, usize) {
		match &self.kind {
			ErrorKind::Error(_) => (1, 0, 0),
			ErrorKind::Fatal(_) => (0, 1, 0),
			ErrorKind::System(_, _) => (0, 0, 1),
			ErrorKind::List(l) => l.into_iter().fold((0, 0, 0), |sum, item| {
				let (s_e, s_f, s_s) = sum;
				let (c_e, c_f, c_s) = item.count();
				(s_e + c_e, s_f + c_f, s_s + c_s)
			}),
			_ => (0, 0, 0),
		}
	}

	pub fn attained(&self) -> bool {
		let (c_err, c_fat, c_sys) = self.count();
		c_err >= 5 || c_fat > 0 || c_sys > 0
	}

	pub fn add(&mut self, e: Error) -> Result<(), Self> {
		if let ErrorKind::List(l) = &mut self.kind {
			l.push(e);
			if self.attained() {
				Err(self.clone())
			} else {
				Ok(())
			}
		} else {
			panic!();
		}
	}

	pub fn adderr(&mut self, e: Result<(), Self>) -> Result<(), Self> {
		if let Err(e) = e {
			self.add(e)
		} else {
			Ok(())
		}
	}

	pub fn ok_or_err<T>(self, ret: T) -> Result<T, Error> {
		if let ErrorKind::List(_) = &self.kind {
			if self.is_empty() {
				Ok(ret)
			} else {
				Err(self)
			}
		} else {
			Err(self)
		}
	}

	pub fn is_empty(&self) -> bool {
		if let ErrorKind::List(l) = &self.kind {
			l.len() == 0
		} else {
			panic!();
		}
	}

	pub fn describe(&self) -> Result<(), std::io::Error> {
		extern crate termcolor;
		use std::io::Write;
		#[allow(unused)]
		use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
		if let ErrorKind::List(l) = &self.kind {
			for item in l.iter() {
				item.describe()?;
			}
		} else {
			let mut st = termcolor::StandardStream::stdout(termcolor::ColorChoice::Always);
			let (cl, k, msg) = match &self.kind {
				ErrorKind::Error(m) => (Some(termcolor::Color::Red), "Error", m.clone()),
				ErrorKind::Fatal(m) => (Some(termcolor::Color::Red), "Fatal", m.clone()),
				ErrorKind::Warning(m) => (Some(termcolor::Color::Magenta), "Warning", m.clone()),
				ErrorKind::Info(m) => (Some(termcolor::Color::Cyan), "Info", m.clone()),
				ErrorKind::System(m, e) => (
					Some(termcolor::Color::Red),
					"System Error",
					format!("{}: {}", m, *e),
				),
				_ => panic!(),
			};
			st.set_color(ColorSpec::new().set_fg(cl))?;
			write!(&mut st, "{}: ", k)?;
			st.set_color(ColorSpec::new().set_fg(None))?;
			write!(&mut st, "{}", msg)?;
			if let Some(dbginfo) = &self.dbginfo {
				st.set_color(ColorSpec::new().set_fg(Some(termcolor::Color::Ansi256(8u8))))?;
				writeln!(&mut st, "  ({}:{})", dbginfo.file, dbginfo.ln)?;
				st.set_color(ColorSpec::new().set_fg(None))?;
			}
			writeln!(&mut st, "")?;
			for tinfo in self.tokens.iter() {
				if let Some(file) = tinfo.file.upgrade() {
					let mut file = (*file).borrow_mut();
					st.set_color(ColorSpec::new().set_fg(Some(termcolor::Color::Blue)))?;
					writeln!(&mut st, "   -> {}:{}:{}", file, tinfo.ln + 1, tinfo.ch + 1)?;
					st.set_color(ColorSpec::new().set_fg(None))?;
					let begin = if tinfo.ln > 2 { tinfo.ln - 2 } else { 0 };
					for ln in begin..(tinfo.ln + 3) {
						if let Ok(line) = file.get_lines(ln) {
							writeln!(&mut st, "{:5} | {}", ln + 1, line)?;
							if ln == tinfo.ln {
								write!(&mut st, "      | ")?;
								for i in 0..tinfo.ch {
									if line.as_bytes()[i] == b'\t' {
										write!(&mut st, "\t")?;
									} else {
										write!(&mut st, " ")?;
									}
								}
								st.set_color(ColorSpec::new().set_fg(Some(termcolor::Color::Red)))?;
								for _ in 0..tinfo.count {
									write!(&mut st, "^")?;
								}
								st.set_color(ColorSpec::new().set_fg(None))?;
								writeln!(&mut st, "")?;
							}
						}
					}
				} else {
					st.set_color(ColorSpec::new().set_fg(Some(termcolor::Color::Blue)))?;
					writeln!(&mut st, "   -> ?:{}:{}", tinfo.ln, tinfo.ch)?;
					st.set_color(ColorSpec::new().set_fg(None))?;
				}
				writeln!(&mut st, "")?;
			}
		}
		Ok(())
	}
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
	Error(String),
	Warning(String),
	Info(String),
	Fatal(String),
	System(String, Rc<Box<dyn std::error::Error>>),
	List(Vec<Error>),
}

// impl std::clone::Clone for ErrorKind {
// 	fn clone(&self) -> Self {
// 		match self {
// 			ErrorKind::Error(m) => ErrorKind::Error(m.clone()),
// 			ErrorKind::Warning(m) => ErrorKind::Warning(m.clone()),
// 			ErrorKind::Info(m) => ErrorKind::Info(m.clone()),
// 			ErrorKind::Fatal(m) => ErrorKind::Fatal(m.clone()),
// 			ErrorKind::System(m, e) => ErrorKind::System(
// 				m.clone(),
// 				Box::new(std::io::Error::new(e.kind(), e.to_string())),
// 			),
// 			ErrorKind::List(v) => ErrorKind::List(v.clone()),
// 		}
// 	}
// }
