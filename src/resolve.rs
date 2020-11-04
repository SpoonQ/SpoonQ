use crate::cond::Cond;
use crate::error::Error;
use crate::optim::{Optim, OptimExpr, OptimStrategy, Qubit};
use crate::token::TokenInfo;
use rustqubo::expr::Expr;
use rustqubo::solve::SimpleSolver;

pub struct Resolver;

#[test]
fn resolv_test() {
	let mut rsvr = Resolver::new();
	let cond = Cond::And(vec![
		Cond::Val(0),
		Cond::Not(Box::new(Cond::Val(1))),
		Cond::Not(Box::new(Cond::Or(vec![Cond::Val(1), Cond::Val(2)]))),
	]);
	let ret = rsvr.resolve(cond, None, None).unwrap_or_else(|e| {
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
	) -> Result<Vec<(usize, bool)>, Error> {
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
			.fold(OptimExpr::Number(0), |e1, e2| e1 + e2);
		if let Some(optim) = optim {
			hmlt += optim.get_strategy(&OptimStrategy::Optimize).unwrap();
		}
		let compiled = hmlt.compile();
		let solver = SimpleSolver::new(&compiled);
		let (_c, qubits, constraints) = solver.solve();
		if constraints.len() == 0 {
			let mut v = Vec::new();
			for i in 0.. {
				if let Some(b) = qubits.get(&Qubit::Val(i)) {
					v.push(*b);
				} else {
					break;
				}
			}
			let mut v = qubits
				.iter()
				.filter_map(|(k, v)| {
					if let Qubit::Val(i) = k {
						Some((*i, *v))
					} else {
						None
					}
				})
				.collect::<Vec<_>>();
			v.sort_by_key(|(k, _)| *k);
			Ok(v)
		} else {
			fatal!(("Cannot find root"), [tinfo])
		}
	}
}
