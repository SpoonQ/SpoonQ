use crate::dimacs::DimacsGenerator;
use crate::error::Error;
use crate::resolve::Resolver;
use crate::token::Tokenizer;
use std::time::Instant;

#[derive(Debug, Clone)]
pub struct CliArgs {
	verbose: bool,
	omit_anneal: bool,
	infile: Option<String>,
	pub iterations: Option<usize>,
	pub generations: Option<usize>,
	pub beta_count: Option<usize>,
	pub sweeps_per_beta: Option<usize>,
}

impl CliArgs {
	pub fn new() -> Self {
		Self {
			verbose: false,
			infile: None,
			omit_anneal: false,
			iterations: None,
			generations: None,
			beta_count: None,
			sweeps_per_beta: None,
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
			.arg(
				Arg::with_name("omit_anneal")
					.help("omit annealing before solving problem")
					.long("omit-anneal"),
			)
			.get_matches_from(iter);
		if let Some(o) = matches.value_of("infile") {
			self.infile = Some(o.to_string());
		}
		if matches.is_present("verbose") {
			self.verbose = true;
		}
		if matches.is_present("omit_anneal") {
			self.omit_anneal = true;
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
	args: &CliArgs,
) -> Result<Option<Vec<(usize, bool)>>, Error> {
	let cond = DimacsGenerator::generate_cond(tknzr)?;
	if let Some(cond) = cond {
		let mut resolver = Resolver::new();
		let heauristics = if use_anneal {
			Error::show(info!(("Annealing..."))).unwrap();
			let t_anneal = Instant::now();
			let res = resolver.solve_by_anneal(cond.clone(), None, None, args, true)?;
			Error::show(info!(("Annealing ended in {:?}", t_anneal.elapsed()))).unwrap();

			Some(res)
		} else {
			None
		};
		Error::show(info!(("Solving..."))).unwrap();
		let t_solve = Instant::now();
		let ret = resolver.solve_by_satsolv(cond, None, heauristics)?;
		Error::show(info!(("Solve ended in {:?}", t_solve.elapsed()))).unwrap();
		Ok(ret)
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
		let fname = &args.infile.clone().unwrap();
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
					match run_dimacs(&mut tknzr, !args.omit_anneal, &args) {
						Ok(Some(v)) => {
							println!("SAT");
							for (k, v) in v.iter() {
								let k = *k as i32;
								print!("{:} ", if *v { k + 1 } else { -k - 1 });
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
							match Generator::new(cc, resolver, args.clone()) {
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
