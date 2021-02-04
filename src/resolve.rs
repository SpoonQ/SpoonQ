extern crate screwsat;
use crate::cli::CliArgs;
use crate::cond::Cond;
use crate::error::Error;
use crate::optim::{Optim, OptimExpr, OptimStrategy, Qubit};
use crate::token::TokenInfo;
use rustqubo::solve::SimpleSolver;
use rustqubo::Expr;
use screwsat::solver::{Lit, LitBool, Solver, Status, Var};

pub struct Resolver;

#[test]
fn resolv_test() {
	let mut rsvr = Resolver::new();
	let cond = Cond::And(vec![
		Cond::Val(0),
		Cond::Not(Box::new(Cond::Val(1))),
		Cond::Not(Box::new(Cond::Or(vec![Cond::Val(1), Cond::Val(2)]))),
	]);
	let args = CliArgs::new();
	let ret = rsvr.resolve(cond, None, None, &args).unwrap_or_else(|e| {
		e.describe().unwrap();
		panic!();
	});
	assert_eq!(ret, vec![(0, true), (1, false), (2, false)]);
}

impl Resolver {
	pub fn new() -> Self {
		Self
	}

	pub fn resolve(
		&mut self,
		cond: Cond,
		optim: Option<&Optim>,
		tinfo: Option<TokenInfo>,
		args: &CliArgs,
	) -> Result<Vec<(usize, bool)>, Error> {
		self.solve_by_anneal(cond.clone(), optim, tinfo.clone(), args, false)
			.map(|v| v.into_iter().map(|(i, b, _)| (i, b)).collect())
	}

	pub fn solve_by_satsolv(
		&mut self,
		cond: Cond,
		_tinfo: Option<TokenInfo>,
		heauristics: Option<Vec<(usize, bool, usize)>>,
	) -> Result<Option<Vec<(usize, bool)>>, Error> {
		let cnf = cond.generate_cnf();
		let n = cnf
			.iter()
			.map(|v| v.iter().map(|i| i.abs() as usize).max().unwrap_or(0))
			.max()
			.unwrap_or(0);
		let input = cnf
			.into_iter()
			.map(|v| {
				v.into_iter()
					.map(|i| Lit::new(i.abs() as u32 - 1, i > 0))
					.collect()
			})
			.collect::<Vec<_>>();
		let mut solv = Solver::new(n, &input);
		if let Some(heauristics) = heauristics {
			for (var, b, p) in heauristics.into_iter() {
				solv.set_polarity_and_priority(Var(var as u32), b, p);
			}
		}
		if solv.solve(None) == Status::Sat {
			Ok(Some(
				solv.models
					.iter()
					.enumerate()
					.map(|(i, b)| {
						(
							i,
							match b {
								LitBool::True => true,
								LitBool::False => false,
								_ => panic!(),
							},
						)
					})
					.collect(),
			))
		} else {
			// UNSAT
			Ok(None)
		}
	}

	pub fn solve_by_anneal(
		&mut self,
		cond: Cond,
		optim: Option<&Optim>,
		_tinfo: Option<TokenInfo>,
		args: &CliArgs,
		is_sat: bool,
	) -> Result<Vec<(usize, bool, usize)>, Error> {
		let cond = cond.collect_andcond();
		let mut hmlt = cond
			//.collect_andcond()
			.iter()
			.enumerate()
			.map(|(i, c)| Expr::Constraint {
				label: format!("Constraint {:}", i),
				expr: Box::new(
					Optim::from_cond(c)
						.get_strategy(&OptimStrategy::Optimize)
						.unwrap(),
				),
			})
			.fold(OptimExpr::Number(0.0), |e1, e2| e1 + e2);
		if let Some(optim) = optim {
			hmlt += optim.get_strategy(&OptimStrategy::Optimize).unwrap();
		}
		let compiled = hmlt.compile();
		let mut solver = SimpleSolver::new(&compiled);
		if is_sat {}
		if let Some(v) = args.iterations {
			solver.iterations = v;
		} else if is_sat {
			solver.iterations = 1;
		}
		if let Some(v) = args.generations {
			solver.generations = v;
		} else if is_sat {
			solver.generations = 1;
		}
		let (_c, sol, _constraints) = solver.solve_with_constraints().unwrap();
		// if constraints.len() == 0 {
		let mut v = sol
			.keys()
			.filter_map(|k| {
				if let Qubit::Val(i) = k {
					Some((*i, sol[k], sol.local_field(k).unwrap() as usize)) // TODO: 0
				} else {
					None
				}
			})
			.collect::<Vec<_>>();
		v.sort_by_key(|(k, _, _)| *k);
		Ok(v)
		// } else {
		// 	fatal!(("Cannot find root"), [tinfo])
		// }
	}
}
