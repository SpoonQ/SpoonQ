use crate::dimacs::DimacsGenerator;
use crate::error::Error;
use crate::resolve::Resolver;
use crate::token::Tokenizer;
use std::collections::HashMap;

#[derive(Debug)]
pub struct CliArgs {
	verbose: bool,
	infile: Option<String>,
}

impl CliArgs {
	pub fn new() -> Self {
		Self {
			verbose: false,
			infile: None,
		}
	}

	pub fn read_args<I, T>(&mut self, iter: I) -> Result<(), Error>
	where
		I: IntoIterator<Item = T>,
		T: Into<std::ffi::OsString> + Clone,
	{
		extern crate clap;
		use clap::{App, Arg};
		let matches = App::new(env!("CARGO_PKG_NAME"))
			.version(env!("CARGO_PKG_VERSION"))
			.author(env!("CARGO_PKG_AUTHORS"))
			.about(env!("CARGO_PKG_DESCRIPTION"))
			.arg(
				Arg::with_name("infile")
					.help("input filename")
					.required(false),
			)
			.arg(
				Arg::with_name("verbose")
					.help("verbose")
					.short("v")
					.long("verbose"),
			)
			.get_matches_from(iter);
		if let Some(o) = matches.value_of("infile") {
			self.infile = Some(o.to_string());
		}
		if matches.is_present("verbose") {
			self.verbose = true;
		}
		Ok(())
	}

	pub fn check_args(&self) -> Result<(), Error> {
		if let None = self.infile {
			return fatal!(("No input file specified"));
		}
		Ok(())
	}
}

fn run_dimacs(
	tknzr: &mut Tokenizer,
	use_anneal: bool,
) -> Result<Option<Vec<(usize, bool)>>, Error> {
	let cond = DimacsGenerator::generate_cond(tknzr)?;
	if let Some(cond) = cond {
		let mut resolver = Resolver::new();
		let heauristics = if use_anneal {
			Error::show(info!(("Solving using annealer..."))).unwrap();
			Some(resolver.solve_by_anneal(cond.clone(), None, None)?)
		} else {
			None
		};
		Error::show(info!(("Solving using sat solver..."))).unwrap();
		resolver.solve_by_satsolv(cond, None, heauristics)
	} else {
		err!(("No input conditionals"))
	}
}

pub fn main<I, T>(iter: I) -> i32
where
	I: IntoIterator<Item = T>,
	T: Into<std::ffi::OsString> + Clone,
{
	use crate::compile::CompoundConstraint;
	use crate::context::Context;
	use crate::file::File;
	use crate::generate::Generator;

	let mut err = Error::from_empty_list();
	let mut args = CliArgs::new();
	let _ = err.adderr(args.read_args(iter));
	let _ = err.adderr(args.check_args());
	if err.is_empty() {
		let fname = &args.infile.unwrap();
		if args.verbose {
			Error::show(info!(("Loading file {}", fname))).unwrap();
		}
		match File::from_file(fname) {
			Ok(file) => {
				if args.verbose {
					Error::show(info!(("Loaded file {}", file))).unwrap();
				}
				let mut cxt = Context::new(file);
				let mut tknzr = Tokenizer::new(&mut cxt);
				if DimacsGenerator::test_file(&mut tknzr).unwrap() {
					Error::show(info!(("Generating clauses from DIMACS file..."))).unwrap();
					match run_dimacs(&mut tknzr, true) {
						Ok(Some(v)) => {
							println!("SAT");
							let m = v.into_iter().collect::<HashMap<_, _>>();
							for (_k, v) in m.iter() {
								print!("{:} ", v);
							}
							println!("");
						}
						Ok(None) => {
							println!("UNSAT");
						}
						Err(e) => {
							let _ = err.add(e);
						}
					}
				} else {
					match CompoundConstraint::parse(&mut tknzr) {
						Ok(cc) => {
							if args.verbose {
								Error::show(info!(("Displaying AST..."))).unwrap();
								eprintln!("{:?}", &cc);
							}
							let resolver = Resolver::new();
							match Generator::new(cc, resolver) {
								Err(e) => {
									let _ = err.add(e);
								}
								Ok(_gen) => {
									if args.verbose {
										Error::show(info!(("Finished."))).unwrap();
									}
								}
							}
						}
						Err(e) => {
							let _ = err.add(e);
						}
					}
				}
				if !err.is_empty() {
					err.describe().unwrap();
					return 1;
				}
			}
			Err(e) => {
				let _ = err.add(e);
			}
		}
	}
	if !err.is_empty() {
		err.describe().unwrap();
		return 1;
	} else {
		return 0;
	}
}
