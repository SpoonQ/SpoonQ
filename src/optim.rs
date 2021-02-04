use crate::cond::Cond;
pub use rustqubo::Expr;
use std::collections::HashMap;
pub type OptimExpr = Expr<(), Qubit, std::string::String, f64>;

// #[derive(Clone, PartialEq, Debug)]
// pub enum Optim {
// 	Number(i32),
// 	Val(usize),
// 	Weight(f64),
// 	//	Cond(Cond),
// 	Add(Box<Optim>, Box<Optim>),
// 	Sub(Box<Optim>, Box<Optim>),
// 	Mul(Box<Optim>, Box<Optim>),
// 	Square(Box<Optim>),
// 	Inv(Box<Optim>),
// }
//
// impl Optim {
// 	fn from_cond_zeroorone(_cond: &Cond, _vals: HashMap<usize, bool>) {
// 		unimplemented!()
// 	}
// }

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum OptimStrategy {
	ZeroOrOne,
	ZeroOrNonZero,
	InvZeroOrOne,
	InvZeroOrNonZero,
	ZeroOrPositive,
	InvZeroOrPositive,
	Optimize, // Zero or Positive
}

impl OptimStrategy {
	fn inv(&self) -> Option<Self> {
		match self {
			Self::ZeroOrOne => Some(Self::InvZeroOrOne),
			Self::ZeroOrNonZero => Some(Self::InvZeroOrNonZero),
			Self::InvZeroOrOne => Some(Self::ZeroOrOne),
			Self::InvZeroOrNonZero => Some(Self::ZeroOrNonZero),
			Self::ZeroOrPositive => Some(Self::InvZeroOrPositive),
			Self::InvZeroOrPositive => Some(Self::ZeroOrPositive),
			_ => None,
		}
	}
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum Qubit {
	Val(usize),
	Ancilla(usize),
}

#[derive(Clone, Debug)]
pub struct Optim {
	map: HashMap<OptimStrategy, OptimExpr>,
	ancillas: usize,
}

impl Optim {
	fn new() -> Self {
		Self {
			map: HashMap::new(),
			ancillas: 0,
		}
	}

	fn insert(&mut self, k: OptimStrategy, e: OptimExpr) {
		self.map.insert(k, e);
	}

	fn grow_ancilla(exp: OptimExpr, r: usize) -> OptimExpr {
		exp.map(&mut |e| match e {
			Expr::Binary(Qubit::Ancilla(n)) => Expr::Binary(Qubit::Ancilla(n + r)),
			Expr::Spin(Qubit::Ancilla(n)) => Expr::Spin(Qubit::Ancilla(n + r)),
			o => o,
		})
	}

	pub fn get_strategy(&self, key: &OptimStrategy) -> Option<OptimExpr> {
		self.map.get(key).map(|e| e.clone()).or_else(|| match key {
			OptimStrategy::ZeroOrOne => self
				.map
				.get(&OptimStrategy::InvZeroOrOne)
				.map(|e| Expr::Number(1.0) + (Expr::Number(-1.0) * e.clone())),
			OptimStrategy::InvZeroOrOne => self
				.map
				.get(&OptimStrategy::ZeroOrOne)
				.map(|e| Expr::Number(1.0) + (Expr::Number(-1.0) * e.clone())),
			OptimStrategy::ZeroOrPositive => {
				self.get_strategy(&OptimStrategy::ZeroOrOne).or_else(|| {
					self.map
						.get(&OptimStrategy::ZeroOrNonZero)
						.map(|e| e.clone() * e.clone())
				})
			}
			OptimStrategy::InvZeroOrPositive => {
				self.get_strategy(&OptimStrategy::InvZeroOrOne).or_else(|| {
					self.map
						.get(&OptimStrategy::ZeroOrNonZero)
						.map(|e| e.clone() * e.clone())
				})
			}
			OptimStrategy::ZeroOrNonZero => self.get_strategy(&OptimStrategy::ZeroOrPositive),
			OptimStrategy::InvZeroOrNonZero => self.get_strategy(&OptimStrategy::InvZeroOrPositive),
			OptimStrategy::Optimize => self.get_strategy(&OptimStrategy::ZeroOrPositive),
		})
	}

	pub fn from_expr(st: OptimStrategy, expr: OptimExpr) -> Self {
		let mut ret = Self::new();
		ret.insert(st, expr);
		ret
	}

	pub fn from_cond(c: &Cond) -> Self {
		fn get_arr_strategy(arr: &[Optim], s: OptimStrategy) -> Option<Vec<OptimExpr>> {
			let out = arr
				.iter()
				.filter_map(|o| o.get_strategy(&s)) // TODO: iter::map_while is preferred
				.collect::<Vec<_>>();
			if arr.len() == out.len() {
				Some(out)
			} else {
				None
			}
		}
		let mut ret = Self::new();
		match c {
			Cond::Val(l) => ret.insert(OptimStrategy::InvZeroOrOne, Expr::Binary(Qubit::Val(*l))),
			Cond::And(arr) => {
				let arr = arr.iter().map(Self::from_cond).collect::<Vec<_>>();
				if let Some(arr) = get_arr_strategy(&arr, OptimStrategy::InvZeroOrOne) {
					let out = arr.into_iter().fold(Expr::Number(1.0), |e1, e2| e1 * e2);
					ret.insert(OptimStrategy::InvZeroOrOne, out);
				} else if let Some(arr) = get_arr_strategy(&arr, OptimStrategy::InvZeroOrNonZero) {
					let out = arr.into_iter().fold(Expr::Number(1.0), |e1, e2| e1 * e2);
					ret.insert(OptimStrategy::InvZeroOrNonZero, out);
				}
				if let Some(arr) = get_arr_strategy(&arr, OptimStrategy::ZeroOrPositive) {
					let out = arr.into_iter().fold(Expr::Number(0.0), |e1, e2| e1 + e2);
					ret.insert(OptimStrategy::ZeroOrPositive, out);
				}
			}
			Cond::Or(arr) => {
				let arr = arr.iter().map(Self::from_cond).collect::<Vec<_>>();
				if let Some(arr) = get_arr_strategy(&arr, OptimStrategy::ZeroOrOne) {
					let out = arr.into_iter().fold(Expr::Number(1.0), |e1, e2| e1 * e2);
					ret.insert(OptimStrategy::ZeroOrOne, out);
				} else if let Some(arr) = get_arr_strategy(&arr, OptimStrategy::ZeroOrNonZero) {
					let out = arr.into_iter().fold(Expr::Number(1.0), |e1, e2| e1 * e2);
					ret.insert(OptimStrategy::ZeroOrNonZero, out);
				}
				if let Some(arr) = get_arr_strategy(&arr, OptimStrategy::InvZeroOrPositive) {
					let out = arr.into_iter().fold(Expr::Number(0.0), |e1, e2| e1 + e2);
					ret.insert(OptimStrategy::InvZeroOrPositive, out);
				}
			}
			Cond::Not(a) => {
				let inner = Self::from_cond(a);
				if let Some(e) = inner.get_strategy(&OptimStrategy::ZeroOrOne) {
					ret.insert(OptimStrategy::ZeroOrOne, Expr::Number(1.0) - e);
				} else {
					for (key, e) in inner.map.into_iter() {
						if let Some(inv) = key.inv() {
							ret.insert(inv, e);
						}
					}
				}
			}
			Cond::CountEq(arr, n) => {
				let arr = arr.iter().map(Self::from_cond).collect::<Vec<_>>();
				if let Some(arr) = get_arr_strategy(arr.as_slice(), OptimStrategy::InvZeroOrOne) {
					let out = arr
						.into_iter()
						.fold(Expr::Number(*n as f64), |e1, e2| e1 - e2);
					ret.insert(OptimStrategy::ZeroOrNonZero, out);
				}
			}
			Cond::CountLeq(arr, n) => {
				ret = Self::from_cond(&Cond::Or(
					(0..=*n)
						.into_iter()
						.map(|m| Cond::CountEq(arr.clone(), m))
						.collect(),
				));
				let arr = arr.iter().map(Self::from_cond).collect::<Vec<_>>();
				if let Some(arr) = get_arr_strategy(arr.as_slice(), OptimStrategy::InvZeroOrOne) {
					let log2n = std::mem::size_of_val(n) * 8 - n.leading_zeros() as usize;
					let rem = n - (1 << (log2n - 1));
					let exp = (1..(log2n)).fold(
						Expr::Number(rem as f64) * Expr::Binary(Qubit::Ancilla(0)),
						|e, p| {
							e + Expr::Number((1 << (p - 1)) as f64)
								* Expr::Binary(Qubit::Ancilla(p))
						},
					);
					let out = arr
						.into_iter()
						.fold(exp, |e1, e2| e1 - Self::grow_ancilla(e2, log2n));
					ret.insert(OptimStrategy::Optimize, out.clone() * out);
				}
			}
			Cond::True => {
				ret.insert(OptimStrategy::ZeroOrOne, Expr::Number(0.0));
			}
			Cond::False => {
				ret.insert(OptimStrategy::ZeroOrOne, Expr::Number(1.0));
			}
		}
		ret
	}
}
