use crate::optim::{Optim, OptimExpr, OptimStrategy};
use std::collections::HashSet;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Cond {
	Val(usize),
	And(Vec<Cond>),
	Or(Vec<Cond>),
	Not(Box<Cond>),
	CountEq(Vec<Cond>, usize),
	CountLeq(Vec<Cond>, usize),
	True,
	False,
}
#[test]
fn collect_andcond_test() {
	assert_eq!(
		Cond::Or(vec![
			Cond::And(vec![Cond::Val(1), Cond::Val(2)]),
			Cond::Val(3)
		])
		.collect_andcond(),
		vec![
			Cond::Or(vec![Cond::Val(1), Cond::Val(3)]),
			Cond::Or(vec![Cond::Val(2), Cond::Val(3)])
		]
	);
	assert_eq!(
		Cond::Or(vec![
			Cond::And(vec![Cond::Val(1), Cond::Val(2)]),
			Cond::And(vec![Cond::Val(3), Cond::Val(4)]),
			Cond::And(vec![Cond::Val(5), Cond::Val(6)])
		])
		.collect_andcond(),
		vec![
			Cond::Or(vec![Cond::Val(1), Cond::Val(3), Cond::Val(5)]),
			Cond::Or(vec![Cond::Val(1), Cond::Val(3), Cond::Val(6)]),
			Cond::Or(vec![Cond::Val(1), Cond::Val(4), Cond::Val(5)]),
			Cond::Or(vec![Cond::Val(1), Cond::Val(4), Cond::Val(6)]),
			Cond::Or(vec![Cond::Val(2), Cond::Val(3), Cond::Val(5)]),
			Cond::Or(vec![Cond::Val(2), Cond::Val(3), Cond::Val(6)]),
			Cond::Or(vec![Cond::Val(2), Cond::Val(4), Cond::Val(5)]),
			Cond::Or(vec![Cond::Val(2), Cond::Val(4), Cond::Val(6)]),
		]
	);
}

#[test]
fn simplify_test() {
	assert_eq!(
		(Cond::And(vec![Cond::False, Cond::Val(0)])).simplify(),
		Cond::False,
		"Cond: {:?}",
		Cond::And(vec![Cond::False, Cond::Val(0)])
	);
}

#[test]
fn get_cross_test() {
	assert_eq!(
		Cond::get_cross(vec![
			vec![Cond::Val(1), Cond::Val(2)],
			vec![Cond::Val(3), Cond::Val(4)]
		]),
		vec![
			vec![Cond::Val(1), Cond::Val(3)],
			vec![Cond::Val(1), Cond::Val(4)],
			vec![Cond::Val(2), Cond::Val(3)],
			vec![Cond::Val(2), Cond::Val(4)],
		]
	)
}

#[test]
fn get_relation_with_test() {
	let triples = [
		(Cond::Val(1), Cond::Val(1), Some(true)),
		(Cond::Val(1), Cond::Val(2), None),
		(Cond::True, Cond::False, Some(false)),
		(
			Cond::And(vec![Cond::Val(1), Cond::Val(2)]),
			Cond::And(vec![Cond::Val(2), Cond::True, Cond::Val(1)]),
			Some(true),
		),
		(
			Cond::And(vec![Cond::Val(1), Cond::False]),
			Cond::False,
			Some(true),
		),
		(
			Cond::Or(vec![Cond::Val(1), Cond::Val(2)]),
			Cond::Or(vec![Cond::Val(2), Cond::False, Cond::Val(1)]),
			Some(true),
		),
		(
			Cond::Or(vec![Cond::Val(1), Cond::True]),
			Cond::True,
			Some(true),
		),
		(
			Cond::CountEq(vec![Cond::Val(0), Cond::Val(1), Cond::False], 1),
			Cond::CountEq(vec![Cond::Val(1), Cond::Val(0)], 1),
			Some(true),
		),
		(
			Cond::And(vec![Cond::False, Cond::Val(0)]),
			Cond::True,
			Some(false),
		),
	];
	for (c1, c2, res) in triples.iter() {
		let c1 = &c1.clone().simplify();
		let c2 = &c2.clone().simplify();
		let res_inv = if let Some(b) = res { Some(!b) } else { None };
		assert_eq!(c1.get_relation_with(c1), Some(true), "Case {:?}", c1);
		assert_eq!(c2.get_relation_with(c2), Some(true), "Case {:?}", c1);
		assert_eq!(
			c1.get_relation_with(c2),
			res.clone(),
			"Case {:?}, {:?}",
			c1,
			c2
		);
		assert_eq!(
			c2.get_relation_with(c1),
			res.clone(),
			"Case {:?}, {:?}",
			c2,
			c1
		);
		assert_eq!(
			Cond::Not(Box::new(c1.clone())).get_relation_with(c2),
			res_inv.clone()
		);
		assert_eq!(
			Cond::Not(Box::new(c2.clone())).get_relation_with(c1),
			res_inv.clone()
		);
		assert_eq!(
			c1.get_relation_with(&Cond::Not(Box::new(c2.clone()))),
			res_inv.clone()
		);
		assert_eq!(
			c2.get_relation_with(&Cond::Not(Box::new(c1.clone()))),
			res_inv.clone()
		);
		assert_eq!(
			Cond::Not(Box::new(c1.clone())).get_relation_with(&Cond::Not(Box::new(c2.clone()))),
			res.clone()
		);
		assert_eq!(
			Cond::Not(Box::new(c2.clone())).get_relation_with(&Cond::Not(Box::new(c1.clone()))),
			res.clone()
		);
	}
}

impl Cond {
	pub fn map<F>(self, mut f: F) -> Cond
	where
		F: FnMut(Cond) -> Cond,
	{
		match self {
			Cond::And(arr) => Cond::And(arr.into_iter().map(f).collect()),
			Cond::Or(arr) => Cond::Or(arr.into_iter().map(f).collect()),
			Cond::Not(c) => Cond::Not(Box::new(f(*c))),
			Cond::CountEq(arr, n) => Cond::CountEq(arr.into_iter().map(f).collect(), n),
			Cond::CountLeq(arr, n) => Cond::CountLeq(arr.into_iter().map(f).collect(), n),
			o => o,
		}
	}

	pub fn iter<'a>(&'a self) -> std::vec::IntoIter<&'a Cond> {
		let v = match self {
			Cond::And(arr) | Cond::Or(arr) | Cond::CountEq(arr, _) | Cond::CountLeq(arr, _) => {
				arr.iter().collect::<Vec<_>>()
			}
			Cond::Not(c) => vec![c.as_ref()],
			_ => Vec::new(),
		};
		v.into_iter()
	}

	pub fn iter_mut<'a>(&'a mut self) -> std::vec::IntoIter<&'a mut Self> {
		let v = match self {
			Cond::And(arr) | Cond::Or(arr) | Cond::CountEq(arr, _) | Cond::CountLeq(arr, _) => {
				arr.into_iter().collect::<Vec<_>>()
			}
			Cond::Not(c) => vec![c.as_mut()],
			_ => Vec::new(),
		};
		v.into_iter()
	}

	pub fn get_relation_with(&self, other: &Self) -> Option<bool> {
		fn compare_arr(arr1: &Vec<Cond>, arr2: &Vec<Cond>) -> bool {
			if arr1.len() != arr2.len() {
				return false;
			}
			let mut arr1 = arr1.iter().collect::<Vec<_>>();
			let mut arr2 = arr2.iter().collect::<Vec<_>>();
			while let Some(c1) = arr1.pop() {
				if let Some(i) = arr2
					.iter()
					.enumerate()
					.filter_map(|(i, c2)| {
						if let Some(true) = c1.get_relation_with(c2) {
							Some(i)
						} else {
							None
						}
					})
					.next()
				{
					arr2.remove(i);
				} else {
					return false;
				}
			}
			return true;
		}
		if self == other {
			return Some(true);
		}
		match (self, other) {
			(Cond::Not(c1), c2) => {
				return c1.as_ref().get_relation_with(c2).map(|b| !b);
			}
			(c1, Cond::Not(c2)) => {
				return c1.get_relation_with(c2.as_ref()).map(|b| !b);
			}
			(Cond::Val(n1), Cond::Val(n2)) => {
				return if n1 == n2 { Some(true) } else { None };
			}
			(Cond::True, Cond::True) | (Cond::False, Cond::False) => {
				return Some(true);
			}
			(Cond::True, Cond::False) | (Cond::False, Cond::True) => {
				return Some(false);
			}
			(Cond::And(arr1), Cond::And(arr2)) | (Cond::Or(arr1), Cond::Or(arr2)) => {
				if compare_arr(arr1, arr2) {
					return Some(true);
				}
			}
			(Cond::CountEq(arr1, n1), Cond::CountEq(arr2, n2)) => {
				if n1 == n2 && compare_arr(arr1, arr2) {
					return Some(true);
				}
			}
			_ => (),
		}

		{
			let v1 =
				Self::filter_conds(&self.clone().collect_andcond()).unwrap_or(vec![Cond::False]);
			let v2 =
				Self::filter_conds(&other.clone().collect_andcond()).unwrap_or(vec![Cond::False]);
			if let ([item1], [item2]) = (v1.as_slice(), v2.as_slice()) {
				if item1 != self || item2 != other {
					if let Some(b) = item1.get_relation_with(item2) {
						return Some(b);
					}
				} else {
					return None;
				}
			}
			if v1.contains(&Cond::False) && v1.len() > 1 {
				if let Some(b) = other.get_relation_with(&Cond::False) {
					return Some(b);
				}
			}
			if v2.contains(&Cond::False) && v2.len() > 1 {
				if let Some(b) = self.get_relation_with(&Cond::False) {
					return Some(b);
				}
			}
			if v1
				.iter()
				.all(|c| v2.iter().any(|cc| c.get_relation_with(cc) == Some(true)))
				&& v2
					.iter()
					.all(|c| v1.iter().any(|cc| c.get_relation_with(cc) == Some(true)))
			{
				return Some(true);
			}
		}
		{
			let v1 = Self::filter_conds(&self.clone().collect_orcond()).unwrap_or(vec![Cond::True]);
			let v2 =
				Self::filter_conds(&other.clone().collect_orcond()).unwrap_or(vec![Cond::True]);
			if let ([item1], [item2]) = (v1.as_slice(), v2.as_slice()) {
				if item1 != self || item2 != other {
					if let Some(b) = item1.get_relation_with(item2) {
						return Some(b);
					}
				} else {
					return None;
				}
			}
			if v1.contains(&Cond::True) && v1.len() > 1 {
				other.get_relation_with(&Cond::True)?;
			}
			if v2.contains(&Cond::True) && v2.len() > 1 {
				self.get_relation_with(&Cond::True)?;
			}
			if v1
				.iter()
				.all(|c| v2.iter().any(|cc| c.get_relation_with(cc) == Some(true)))
				&& v2
					.iter()
					.all(|c| v1.iter().any(|cc| c.get_relation_with(cc) == Some(true)))
			{
				return Some(true);
			}
		}
		return None;
	}

	pub fn get_values(&self) -> HashSet<usize> {
		let mut ret = HashSet::new();
		match self {
			Cond::Val(n) => {
				ret.insert(n.clone());
			}
			o => {
				o.iter()
					.for_each(|c| ret = ret.union(&c.get_values()).cloned().collect());
			}
		}
		ret
	}

	pub fn from_bool(b: bool) -> Self {
		if b {
			Cond::True
		} else {
			Cond::False
		}
	}

	pub fn get_inverse(self) -> Self {
		if let Cond::Not(c) = self {
			*c
		} else {
			Cond::Not(Box::new(self))
		}
	}

	fn filter_conds(v: &[Cond]) -> Option<Vec<Cond>> {
		let mut ret = Vec::new();
		for (i, c) in v.iter().enumerate() {
			let bs = v
				.iter()
				.enumerate()
				.filter_map(|(j, cc)| {
					if i == j {
						None
					} else {
						cc.get_relation_with(&c)
					}
				})
				.collect::<Vec<bool>>();
			if bs.contains(&false) {
				return None;
			}
			if bs.len() == 0 {
				ret.push(c.clone());
			}
		}
		return Some(ret);
	}

	// It does not generate Or, CountEq, CountLeq
	pub fn simplify(self) -> Self {
		let is_and = if let Cond::And(_) = &self {
			true
		} else {
			false
		};
		let is_eq = if let Cond::CountEq(_, _) = &self {
			true
		} else {
			false
		};
		if let Some(b) = self.get_relation_with(&Cond::True) {
			Cond::from_bool(b)
		} else {
			match self {
				Cond::And(arr) | Cond::Or(arr) => {
					let v = arr
						.into_iter()
						.flat_map(|c| {
							if is_and {
								c.collect_andcond()
							} else {
								c.collect_orcond()
							}
							.into_iter()
							.map(Self::simplify)
						})
						.collect::<Vec<_>>();
					if is_and && v.contains(&Cond::False) {
						Cond::False
					} else if !is_and && v.contains(&Cond::True) {
						Cond::True
					} else {
						if let Some(mut v) = Self::filter_conds(
							&v.into_iter()
								.filter(|c| c != &Cond::True && c != &Cond::False)
								.collect::<Vec<_>>(),
						) {
							if v.len() <= 1 {
								v.pop().unwrap_or(Cond::from_bool(is_and))
							} else {
								if is_and {
									Cond::And(v)
								} else {
									Cond::Or(v)
								}
							}
						} else {
							Cond::from_bool(!is_and)
						}
					}
				}
				Cond::Not(c) => match c.simplify() {
					Cond::Not(cc) => *cc,
					o => Cond::Not(Box::new(o)),
				},
				Cond::CountEq(arr, n) | Cond::CountLeq(arr, n) => {
					let n = n as i32;
					let (arr, n) = arr.into_iter().fold((Vec::new(), n), |(mut v, n), c| {
						let c = c.simplify();
						if let Some(b) = c.get_relation_with(&Cond::True) {
							(v, if b { n - 1 } else { n })
						} else {
							v.push(c);
							(v, n)
						}
					});
					if !is_eq && n >= arr.len() as i32 {
						Cond::True
					} else if n < 0 || n > arr.len() as i32 {
						Cond::False
					} else if n == 0 {
						Cond::And(arr.into_iter().map(|c| Cond::Not(Box::new(c))).collect())
					} else if n == arr.len() as i32 {
						assert!(is_eq);
						Cond::And(arr)
					} else {
						if is_eq {
							Cond::CountEq(arr, n as usize)
						} else {
							Cond::CountLeq(arr, n as usize)
						}
					}
				}
				o => o,
			}
		}
	}

	pub fn diminish_orcond(self) -> Self {
		match self {
			Cond::Or(arr) => {
				let v = arr
					.into_iter()
					.map(|c| Cond::Not(Box::new(c.diminish_orcond())))
					.collect();
				Cond::Not(Box::new(Cond::And(v)))
			}
			Cond::Not(c) => {
				if let Cond::Not(c) = *c {
					*c
				} else {
					Cond::Not(c)
				}
			}
			o => o,
		}
	}

	pub fn generate_cnf(self) -> Vec<Vec<isize>> {
		let cond = self.break_countcond_recursive();
		cond.collect_andcond()
			.into_iter()
			.map(|c| {
				if let Cond::Or(v) = c {
					v.into_iter()
						.map(|c| match c.simplify() {
							Cond::Val(n) => n as isize,
							Cond::Not(c) => {
								if let Cond::Val(n) = *c {
									-(n as isize)
								} else {
									panic!()
								}
							}
							_ => panic!(),
						})
						.collect()
				} else {
					panic!()
				}
			})
			.collect()
	}

	// pub fn collect_count(&self) -> Self {}

	pub fn contains_or(&self) -> bool {
		match self {
			Cond::Or(_) => true,
			o => o.iter().any(|c| c.contains_or()),
		}
	}

	pub fn break_countcond(self) -> Cond {
		let is_counteq = if let Cond::CountEq(_, _) = &self {
			true
		} else {
			false
		};
		match self {
			Cond::CountEq(arr, n) | Cond::CountLeq(arr, n) => {
				let indexes_list = arr
					.iter()
					.fold(vec![(Vec::new(), n)], |outer, _| {
						outer.into_iter().fold(Vec::new(), |mut ret, (v, n)| {
							if !is_counteq || n < arr.len() - v.len() {
								let mut v = v.clone();
								v.push(false);
								ret.push((v, n));
							}
							if n > 0 {
								let mut v = v.clone();
								v.push(true);
								ret.push((v, n - 1));
							}
							ret
						})
					})
					.into_iter()
					.map(|(v, _n)| v);
				Cond::Or(
					indexes_list
						.map(|indexes| {
							Cond::And(
								indexes
									.into_iter()
									.zip(arr.iter().cloned())
									.map(|(b, c)| if b { c } else { Cond::Not(Box::new(c)) })
									.collect(),
							)
						})
						.collect(),
				)
			}
			o => o,
		}
	}

	pub fn break_countcond_recursive(self) -> Cond {
		self.break_countcond()
			.map(|c| c.break_countcond_recursive())
	}

	fn get_cross(v: Vec<Vec<Cond>>) -> Vec<Vec<Cond>> {
		v.into_iter().fold(vec![Vec::new()], |outer, inner| {
			outer
				.iter()
				.flat_map(move |v| {
					inner
						.iter()
						.map(|item| {
							let mut v = v.clone();
							v.push(item.clone());
							v
						})
						.collect::<Vec<_>>()
				})
				.collect()
		})
	}

	pub fn collect_andcond(self) -> Vec<Cond> {
		match self {
			Cond::And(arr) => arr.into_iter().flat_map(|c| c.collect_andcond()).collect(),
			Cond::Or(arr) => {
				if arr.len() == 0 {
					vec![Cond::False]
				} else {
					Self::get_cross(
						arr.into_iter()
							.map(|c| c.collect_andcond())
							.collect::<Vec<_>>(),
					)
					.into_iter()
					.map(|mut v| {
						if v.len() == 1 {
							v.swap_remove(0)
						} else {
							Cond::Or(v)
						}
					})
					.collect()
				}
			}
			Cond::Not(c) => {
				if let Cond::Or(arr) = *c {
					arr.into_iter().fold(Vec::new(), |mut arr, c| {
						arr.append(&mut Cond::Not(Box::new(c)).collect_andcond());
						arr
					})
				} else {
					vec![Cond::Not(c)]
				}
			}
			// Cond::CountEq(arr, n) => Cond::CountEq(arr, n).break_countcond().collect_andcond(),
			// Cond::CountLeq(arr, n) => Cond::CountLeq(arr, n).break_countcond().collect_andcond(),
			o => vec![o],
		}
	}

	pub fn collect_orcond(self) -> Vec<Cond> {
		match self {
			Cond::Or(arr) => arr.into_iter().fold(Vec::new(), |mut arr, c| {
				arr.append(&mut c.collect_orcond());
				arr
			}),
			Cond::And(arr) => {
				if arr.len() == 0 {
					vec![Cond::True]
				} else {
					Self::get_cross(
						arr.into_iter()
							.map(|c| c.collect_orcond())
							.collect::<Vec<_>>(),
					)
					.into_iter()
					.map(|mut v| {
						if v.len() == 1 {
							v.swap_remove(0)
						} else {
							Cond::And(v)
						}
					})
					.collect()
				}
			}
			Cond::Not(c) => {
				if let Cond::And(arr) = *c {
					arr.into_iter().fold(Vec::new(), |mut arr, c| {
						arr.append(&mut Cond::Not(Box::new(c)).collect_orcond());
						arr
					})
				} else {
					vec![Cond::Not(c)]
				}
			}
			Cond::CountEq(arr, n) => Cond::CountEq(arr, n).break_countcond().collect_orcond(),
			Cond::CountLeq(arr, n) => Cond::CountLeq(arr, n).break_countcond().collect_orcond(),
			o => vec![o],
		}
	}

	pub fn contains_count(&self) -> bool {
		match self {
			Cond::CountEq(_, _) | Cond::CountLeq(_, _) => true,
			o => o.iter().any(|c| c.contains_count()),
		}
	}

	pub fn generate_optim(self) -> OptimExpr {
		Optim::from_cond(&self)
			.get_strategy(&OptimStrategy::ZeroOrOne)
			.unwrap()
	}
}
