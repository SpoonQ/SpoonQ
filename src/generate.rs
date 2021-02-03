use crate::cli::CliArgs;
use crate::compile::CompoundConstraint;
use crate::compile::{Command, Expression, ExpressionList, LabelList, Statement};
use crate::cond::Cond;
use crate::error::Error;
use crate::optim::{Optim, OptimExpr, OptimStrategy};
use crate::resolve::Resolver;
use crate::token::{
	Keyword, Operator, TokenFloatingLiteral, TokenHash, TokenIdent, TokenInfo, TokenKeyword,
	TokenNumLiteral, TokenOperator, TokenStringLiteral,
};
use std::collections::{HashMap, HashSet};

#[test]
fn generator_test() {
	fn get_generator(s: &'static str) -> Generator {
		use crate::context::Context;
		use crate::file::File;
		use crate::token::Tokenizer;
		let mut cxt = Context::new(File::from_string(s));
		let mut tknzr = Tokenizer::new(&mut cxt);
		((|tknzr: &mut Tokenizer| -> Result<Generator, Error> {
			let cc = CompoundConstraint::parse(tknzr)?;
			Generator::new(cc, Resolver::new(), CliArgs::new())
		})(&mut tknzr))
		.unwrap_or_else(|e| {
			e.describe().unwrap();
			panic!();
		})
	}
	let _g = get_generator(
		r#"
			type h := a | b
			h [@a, @b]
			@a != b;
			@a == a;
			@b != a;
			@b == b;
			# solve([@a, @b]);
			# solve([@a, @b]);
			# print([@a, @b])
		"#,
	);
	// let th = TokenHash(b"a".to_vec(), None);
	// let ti = Expression::Ident(TokenIdent(b"a".to_vec(), None));
	// assert_eq!(g.result.unwrap().get(&th), Some(&ti));
}

#[allow(unused)]
#[derive(PartialEq, Clone)]
struct TypeSpec {
	pub ident: TokenIdent,
	pub list: LabelList,
}

#[allow(unused)]
#[derive(PartialEq, Clone)]
struct ValSpec {
	hash: TokenHash,
	typ: TypeSpec,
	pub index: usize,
}

#[allow(unused)]
#[derive(PartialEq, Clone)]
struct DeclSpec {
	ident: TokenIdent,
	hlist: Vec<TokenIdent>,
	exp: Expression,
}

pub struct Generator {
	decls: HashMap<Vec<u8>, DeclSpec>,
	types: HashMap<Vec<u8>, TypeSpec>,
	vals: HashMap<Vec<u8>, ValSpec>,
	resolver: Resolver,
	result: Option<HashMap<TokenHash, Expression>>,
	conds: Vec<Cond>,
	optim: Option<OptimExpr>,
	indexed_vals: Vec<ValSpec>,
	args: CliArgs,
}

impl Generator {
	pub fn new(ast: CompoundConstraint, resolver: Resolver, args: CliArgs) -> Result<Self, Error> {
		let mut ret = Generator {
			decls: HashMap::new(),
			types: HashMap::new(),
			vals: HashMap::new(),
			resolver: resolver,
			result: None,
			conds: Vec::new(),
			optim: None,
			indexed_vals: Vec::new(),
			args,
		};
		let mut err = Error::from_empty_list();
		for st in ast.children.iter() {
			if let Err(e) = ret.proc_statement(st) {
				err.add(e)?;
			}
		}
		err.ok_or_err(ret)
	}

	fn search_decl(&self, s: &Vec<u8>) -> Option<DeclSpec> {
		// TODO: also search on parents if any
		self.decls.get(s).map(|o| o.clone())
	}

	fn search_typ(&self, s: &Vec<u8>) -> Option<TypeSpec> {
		self.types.get(s).map(|o| o.clone())
	}

	fn search_val(&self, s: &Vec<u8>) -> Option<ValSpec> {
		self.vals.get(s).map(|o| o.clone())
	}

	pub fn add_cond(&mut self, c: Cond) {
		self.conds.push(c);
	}

	fn expand_expression(&self, exp: &Expression) -> Result<Expression, Error> {
		match exp {
			Expression::UserFunction(to, ExpressionList(l)) => {
				if let Some(DeclSpec {
					ident: _,
					hlist,
					exp,
				}) = self.search_decl(&to.0)
				{
					let mut it = l.iter();
					let mut tbl = HashMap::new();
					for id in hlist.iter() {
						if let Some(e) = it.next() {
							tbl.insert(id.0.clone(), e.clone());
						} else {
							return err!(
								(
									"Argument {} is not specified",
									String::from_utf8(id.0.clone()).unwrap()
								),
								[to.1.clone()]
							);
						}
					}
					if let Some(_) = it.next() {
						err!(("Too many arguments"), [to.1.clone()])
					} else {
						Ok(exp.clone().replace(&tbl))
					}
				} else {
					err!(
						(
							"Cannot find decl {}",
							String::from_utf8(to.0.clone()).unwrap()
						),
						[to.1.clone()]
					)
				}
			}
			Expression::InternalFunction(TokenKeyword(key, tinfo), ExpressionList(l)) => {
				match key {
					Keyword::Len => {
						if let [exp] = l.as_slice() {
							let exp = self.expand_expression(exp)?;
							let v = exp.expand_hashlist()?;
							Ok(Expression::Number(TokenNumLiteral(v.len() as u32, None)))
						} else {
							err!(("Bad arguments of len()"), [tinfo.clone()])
						}
					}
					Keyword::Different => {
						if let [exp] = l.as_slice() {
							let mut ret = None;
							let exp = self.expand_expression(exp)?;
							let list = exp.expand_hashlist()?;
							let mut itr1 = list.iter();
							while let Some(h1) = itr1.next() {
								let mut itr2 = itr1.clone();
								while let Some(h2) = itr2.next() {
									let e = Expression::BinaryOperator(
										TokenOperator(Operator::NotEq, None),
										Box::new(Expression::ObjectiveVariable((*h1).clone())),
										Box::new(Expression::ObjectiveVariable((*h2).clone())),
									);
									if let Some(eo) = ret {
										ret = Some(Expression::BinaryOperator(
											TokenOperator(Operator::And, None),
											Box::new(eo),
											Box::new(e),
										));
									} else {
										ret = Some(e);
									}
								}
							}
							if let Some(ret) = ret {
								Ok(ret)
							} else {
								err!(( "Number of bjective values in different() must be more than two" ), [tinfo.clone()])
							}
						} else {
							err!(("Bad arguments of different()"), [tinfo.clone()])
						}
					}
					o => err!(("Bad internal function {}", o), [tinfo.clone()]),
				}
			}
			Expression::Ident(to) => {
				if let Some(DeclSpec {
					ident: _,
					hlist,
					exp,
				}) = self.search_decl(&to.0)
				{
					if hlist.len() > 0 {
						err!(
							(
								"{} is function, arguments required",
								String::from_utf8(to.0.clone()).unwrap()
							),
							[to.1.clone()]
						)
					} else {
						Ok(exp.clone())
					}
				} else {
					Ok(Expression::Ident(to.clone()))
				}
			}
			Expression::List(ExpressionList(l)) => Ok(Expression::List(ExpressionList({
				let mut v = Vec::new();
				for item in l.iter() {
					v.push(self.expand_expression(item)?);
				}
				v
			}))),
			Expression::BinaryOperator(TokenOperator(op, tinfo), e1, e2) => {
				let e1 = self.expand_expression(&*e1)?;
				let e2 = self.expand_expression(&*e2)?;
				if let Expression::Number(TokenNumLiteral(a, _)) = e1 {
					if let Expression::Number(TokenNumLiteral(b, _)) = e2 {
						match op {
							Operator::Plus => {
								return Ok(Expression::Number(TokenNumLiteral(a + b, None)))
							}
							Operator::Minus => {
								return Ok(Expression::Number(TokenNumLiteral(a - b, None)))
							}
							Operator::Multi => {
								return Ok(Expression::Number(TokenNumLiteral(a * b, None)))
							}
							Operator::Divi => {
								return Ok(Expression::Number(TokenNumLiteral(a / b, None)))
							}
							_ => {}
						}
					}
				}
				Ok(Expression::BinaryOperator(
					TokenOperator(*op, tinfo.clone()),
					Box::new(e1),
					Box::new(e2),
				))
			}
			o => Ok(o.clone()),
		}
	}

	fn parse_type_result(
		&self,
		list: &LabelList,
		index: usize,
		v: &Vec<bool>,
	) -> Result<Expression, Error> {
		match list {
			LabelList::List(l) => {
				let mut ans = Vec::new();
				for (i, label) in l.iter().enumerate() {
					if v[index + i] {
						ans.push(label);
					}
				}
				if ans.len() == 1 {
					Ok(Expression::Ident(ans.pop().unwrap().clone()))
				} else {
					fatal!(("Answer error"))
				}
			}
			LabelList::Range(a, b) => {
				let a = self.expand_expression(&a)?;
				let b = self.expand_expression(&b)?;
				if let Expression::Number(TokenNumLiteral(a, _)) = a {
					if let Expression::Number(TokenNumLiteral(b, _)) = b {
						let mut ans = Vec::new();
						for i in a..(b + 1) {
							if v[(index as u32 + i - a as u32) as usize] {
								ans.push(i);
							}
						}
						if ans.len() == 1 {
							return Ok(Expression::Number(TokenNumLiteral(
								ans.pop().unwrap(),
								None,
							)));
						}
					}
				}
				fatal!(("Answer error"))
			}
		}
	}

	fn get_type_len(&self, list: &LabelList, tinfo: Option<TokenInfo>) -> Result<usize, Error> {
		match list {
			LabelList::List(v) => Ok(v.len()),
			LabelList::Range(a, b) => {
				let a = self.expand_expression(&a)?;
				let b = self.expand_expression(&b)?;
				if let Expression::Number(TokenNumLiteral(a, _)) = a {
					if let Expression::Number(TokenNumLiteral(b, _)) = b {
						if b > a {
							Ok((b - a + 1) as usize)
						} else {
							err!(("a .. b should be a < b"), [tinfo])
						}
					} else {
						err!(("Right side of .. must be const int"), [tinfo])
					}
				} else {
					err!(("Left side of .. must be const int"), [tinfo])
				}
			}
		}
	}

	fn generate_cond_binaryop(
		&mut self,
		op: Operator,
		left: &Expression,
		right: &Expression,
		tinfo: Option<TokenInfo>,
	) -> Result<Cond, Error> {
		match op {
			Operator::Equal => {
				if let Expression::ObjectiveVariable(TokenHash(s, tinfo_left)) = left {
					if let Some(o) = self.search_val(s) {
						let TypeSpec { ident, list } = o.typ;
						match list {
							LabelList::List(l) => {
								if let Expression::Ident(TokenIdent(s, tinfo_l)) = right {
									if let Some(offset) = l.iter().position(|to| {
										if to.0.len() != s.len() {
											return false;
										}
										for i in 0..to.0.len() {
											if to.0[i] != s[i] {
												return false;
											}
										}
										return true;
									}) {
										Ok(Cond::Val(o.index + offset))
									} else {
										err!(
											(
												"Label {} cannot found in type {}",
												String::from_utf8(s.clone()).unwrap(),
												String::from_utf8(ident.0.clone()).unwrap()
											),
											[tinfo_l.clone()]
										)
									}
								} else {
									err!(
										(
											"Type {} should be compared with an ident",
											String::from_utf8(ident.0.clone()).unwrap()
										),
										[tinfo_left.clone()]
									)
								}
							}
							LabelList::Range(_a, _b) => err!(("Not Implemented")),
						}
					} else {
						err!(
							(
								"Variable {} not found",
								String::from_utf8(s.clone()).unwrap()
							),
							[tinfo_left.clone()]
						)
					}
				} else {
					err!(("not implemented"))
				}
			}
			Operator::NotEq => Ok(Cond::Not(Box::new(self.generate_cond_binaryop(
				Operator::Equal,
				left,
				right,
				tinfo,
			)?))),
			Operator::LeftArrow | Operator::RightArrow | Operator::LeftRightArrow => {
				err!(("not implemented"))
			}
			Operator::LessEq | Operator::GreatEq | Operator::Less | Operator::Great => {
				err!(("not implemented"))
			}
			Operator::Or => Ok(Cond::Not(Box::new(Cond::And(vec![
				Cond::Not(Box::new(self.generate_cond(left)?)),
				Cond::Not(Box::new(self.generate_cond(right)?)),
			])))),
			Operator::And => Ok(Cond::And(vec![
				self.generate_cond(left)?,
				self.generate_cond(right)?,
			])),
			Operator::Plus | Operator::Minus | Operator::Multi | Operator::Divi => {
				err!(("not implemented"))
			}
			o => err!(("not implemented {}", o), [tinfo.clone()]),
		}
	}

	#[allow(unused)] // TODO:
	fn generate_cond(&mut self, exp: &Expression) -> Result<Cond, Error> {
		match exp {
			Expression::BinaryOperator(TokenOperator(op, tinfo), e1, e2) => {
				self.generate_cond_binaryop(*op, e1, e2, tinfo.clone())
			}
			// Expression::PrefixOperator(TokenOperator(op, tinfo), e) => match op {
			// 	Operator::Not => err!(("not implemented")),
			// 	Operator::Minus => err!(("not implemented")),
			// },
			_ => err!(("not implemented {:?}", exp)),
		}
	}

	fn filter_cond(&self, cond: Cond, hlist: &HashSet<TokenHash>) -> (Cond, HashSet<TokenHash>) {
		match cond {
			Cond::Val(val) => {
				let h = &self.indexed_vals[val];
				if hlist.contains(&h.hash) {
					(Cond::Val(val), {
						let mut ret = HashSet::new();
						ret.insert(h.hash.clone());
						ret
					})
				} else {
					(Cond::True, HashSet::new())
				}
			}
			o => {
				let mut nhlist = HashSet::new();
				let c = o.map(|cond| {
					let (c, l) = self.filter_cond(cond, hlist);
					if hlist.iter().any(|h| l.contains(h)) {
						nhlist = nhlist.union(&l).map(|o| o.clone()).collect();
						c
					} else {
						Cond::True
					}
				});
				(c, nhlist)
			}
		}
	}

	fn cond_cram_val(cond: Cond, v: Vec<Option<usize>>) -> Cond {
		match cond {
			Cond::Val(val) => Cond::Val(v[val].unwrap()),
			o => o,
		}
	}

	fn command_print(&self, args: &Vec<Expression>) -> Result<(), Error> {
		for exp in args.iter() {
			match self.expand_expression(&exp)? {
				Expression::String(TokenStringLiteral(s, _)) => {
					print!("{}", String::from_utf8_lossy(&s));
				}
				Expression::Number(TokenNumLiteral(num, _)) => {
					print!("{}", num);
				}
				Expression::ObjectiveVariable(h) => {
					if let Some(r) = &self.result {
						if let Some(o) = r.get(&h) {
							print!("{}", o);
						} else {
							return err!(
								("Variable {} is not solved.", String::from_utf8_lossy(&h.0)),
								[h.1]
							);
						}
					} else {
						return err!(("Not solved."));
					}
				}
				o => {
					if let Some(r) = &self.result {
						let e = self.expand_expression(&o)?;
						let l = e.expand_hashlist()?;
						for (i, th) in l.iter().enumerate() {
							if i > 0 {
								println!("");
							}
							if let Some(o) = r.get(th) {
								print!("@{} = {}", String::from_utf8_lossy(&th.0), o);
							} else {
								let th = (*th).clone();
								return err!(
									("Variable {} is not solved.", String::from_utf8_lossy(&th.0)),
									[th.1]
								);
							}
						}
					} else {
						return err!(("Not solved."));
					}
				}
			}
		}
		println!("");
		Ok(())
	}

	fn command_solve(
		&mut self,
		hlist: &HashSet<TokenHash>,
		tinfo: Option<TokenInfo>,
	) -> Result<(), Error> {
		if self.conds.len() > 0 {
			let mut filtered_cond = Cond::And(self.conds.clone());
			let mut filtered_list = hlist.clone();
			loop {
				let (cond, list) = self.filter_cond(filtered_cond.clone(), &filtered_list);
				if list != filtered_list {
					filtered_list = list;
					filtered_cond = cond;
				} else {
					break;
				}
			}
			for h in filtered_list.iter() {
				let val = self.vals.get(&h.0).unwrap();
				let len = self.get_type_len(&val.typ.list, None)?;
				let c = Cond::CountEq((0..len).map(|i| Cond::Val(val.index + i)).collect(), 1);
				filtered_cond = Cond::And(vec![filtered_cond, c]);
			}
			let mut cramed_indices = Vec::with_capacity(self.indexed_vals.len());
			let mut hash_ind_map = HashMap::new();
			let mut i = 0_usize;
			for item in self.indexed_vals.iter() {
				if filtered_list.contains(&item.hash) {
					if !hash_ind_map.contains_key(&item.hash) {
						hash_ind_map.insert(item.hash.clone(), i);
					}
					cramed_indices.push(Some(i));
					i += 1;
				} else {
					cramed_indices.push(None);
				}
			}
			let cond = Self::cond_cram_val(filtered_cond, cramed_indices);
			let optim = if let Some(optim) = &self.optim {
				Some(Optim::from_expr(OptimStrategy::Optimize, optim.clone()))
			} else {
				None
			};
			let res = self
				.resolver
				.resolve(cond, optim.as_ref(), tinfo, &self.args)?;
			let res = res.into_iter().map(|(_, v)| v).collect::<Vec<_>>();
			let mut ret = HashMap::new();
			for hash in filtered_list.iter() {
				let index = hash_ind_map.get(hash).unwrap();
				let list = &self.vals.get(&hash.0).unwrap().typ.list;
				let ans = self.parse_type_result(list, *index, &res)?;
				ret.insert((*hash).clone(), ans);
			}
			self.result = Some(ret);
			Ok(())
		} else {
			fatal!(("No constraints"), [tinfo])
		}
	}

	pub fn command_optimize(
		&mut self,
		is_maximize: bool,
		list: &Vec<(Expression, Expression)>,
		tinfo: Option<TokenInfo>,
	) -> Result<(), Error> {
		let mut optim = None;
		for (weight, exp) in list.iter() {
			let weight = self.expand_expression(weight)?;
			let w = match weight {
				Expression::Number(TokenNumLiteral(num, _)) => num as f64,
				Expression::Float(TokenFloatingLiteral(f, _)) => f,
				o => {
					return err!(("Require weight, found {}", o), [tinfo]);
				}
			};
			let cond = self.generate_cond(exp)?;
			let o = OptimExpr::Float(w)
				* Optim::from_cond(&cond)
					.get_strategy(&OptimStrategy::InvZeroOrOne)
					.unwrap();
			if let Some(o2) = optim {
				optim = Some(o + o2);
			} else {
				optim = Some(o);
			}
		}
		if let Some(optim) = optim {
			if let Some(ret) = &self.optim {
				if is_maximize {
					self.optim = Some(ret.clone() - optim);
				} else {
					self.optim = Some(ret.clone() + optim);
				}
			} else {
				if is_maximize {
					self.optim = Some(OptimExpr::Number(-1) * optim);
				} else {
					self.optim = Some(optim);
				}
			}
			Ok(())
		} else {
			err!(("Blank optimize function"), [tinfo])
		}
	}

	pub fn proc_statement(&mut self, st: &Statement) -> Result<(), Error> {
		match st {
			Statement::CompoundConstraint(cs) => err!(
				("Nesting compound constraint is not implemented."),
				[cs.tinfo.clone()]
			),
			Statement::Command(cmd) => match cmd {
				Command::Let(to, l, exp) => {
					self.decls.insert(
						to.0.clone(),
						DeclSpec {
							ident: to.clone(),
							hlist: l.clone(),
							exp: self.expand_expression(exp)?,
						},
					);
					Ok(())
				}
				Command::Maximize(l) => self.command_optimize(true, l, None),
				Command::Minimize(l) => self.command_optimize(false, l, None),
				Command::Print(v) => self.command_print(&v.0),
				Command::Solve(v) => {
					let mut hlist: HashSet<TokenHash> = HashSet::new();
					for exp in v.0.iter() {
						let exp = self.expand_expression(exp)?;
						let nhlist: HashSet<TokenHash> =
							exp.expand_hashlist()?.into_iter().cloned().collect();
						hlist = hlist.union(&nhlist).cloned().collect();
					}
					self.command_solve(&hlist, None)
				}
				Command::Different(v) => {
					let mut hlist: HashSet<TokenHash> = HashSet::new();
					for exp in v.0.iter() {
						let exp = self.expand_expression(exp)?;
						let nhlist: HashSet<TokenHash> =
							exp.expand_hashlist()?.into_iter().cloned().collect();
						hlist = hlist.union(&nhlist).cloned().collect();
					}
					let mut it = hlist
						.iter()
						.map(|h| self.vals.get(&h.0).unwrap().typ.clone());
					if let Some(typ) = it.next() {
						if it.all(|t| t == typ) {
							for i in 0..self.get_type_len(&typ.list, typ.ident.1)? {
								self.add_cond(Cond::CountEq(
									hlist
										.iter()
										.map(|h| Cond::Val(self.vals.get(&h.0).unwrap().index + i))
										.collect(),
									1,
								));
							}
						} else {
							return err!(("Type mismatch in Different"));
						}
					}
					Ok(())
				}
				Command::Type(to, l) => {
					if self.types.contains_key(&to.0) {
						err!(
							(
								"Type {} already declared in this compound constraint",
								String::from_utf8(to.0.clone()).unwrap()
							),
							[to.1.clone()]
						)
					} else {
						self.types.insert(
							to.0.clone(),
							TypeSpec {
								ident: to.clone(),
								list: l.clone(),
							},
						);
						Ok(())
					}
				}
				Command::TypeDecl(to, exp, id) => {
					if let Some(typ) = self.search_typ(&to.0) {
						let mut err = Error::from_empty_list();
						let exp = self.expand_expression(exp)?;
						for th in exp.expand_hashlist()? {
							if self.vals.contains_key(&th.0) {
								err.adderr(err!(
									(
										"Hash {} already declared in this compound constraint",
										String::from_utf8(th.0.clone()).unwrap()
									),
									[th.1.clone()]
								))?;
							} else {
								let tlen = self.get_type_len(&typ.list, th.1.clone())?;
								let vs = ValSpec {
									hash: th.clone(),
									typ: typ.clone(),
									index: self.indexed_vals.len(),
								};
								for _ in 0..tlen {
									self.indexed_vals.push(vs.clone());
								}
								self.vals.insert(th.0.clone(), vs);
							}
						}
						if let Some(id) = id {
							self.decls.insert(
								id.0.clone(),
								DeclSpec {
									ident: id.clone(),
									hlist: Vec::new(),
									exp: self.expand_expression(&exp)?,
								},
							);
						}
						err.ok_or_err(())
					} else {
						err!(
							(
								"Type {} not found",
								String::from_utf8(to.0.clone()).unwrap()
							),
							[to.1.clone()]
						)
					}
				}
			},
			Statement::Expression(exp) => {
				let c = self.generate_cond(exp)?;
				self.add_cond(c);
				Ok(())
			}
		}
	}
}
