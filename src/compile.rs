#[allow(unused)]
use crate::error::Error;
#[allow(unused)]
use crate::token::{
	operator_check_level, Keyword, Operator, OperatorKind, Token, TokenFloatingLiteral, TokenHash,
	TokenIdent, TokenInfo, TokenKeyword, TokenNumLiteral, TokenOperator, TokenStringLiteral,
	Tokenizer,
};
use std::boxed::Box;
use std::collections::{HashMap, HashSet};
use std::vec::Vec;

#[allow(unused)] // TODO:
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
	CompoundConstraint(CompoundConstraint),
	Expression(Expression),
	Command(Command),
}

impl Statement {
	pub fn clear_info(self) -> Self {
		match self {
			Self::CompoundConstraint(cc) => Self::CompoundConstraint(cc.clear_info()),
			Self::Expression(e) => Self::Expression(e.clear_info()),
			Self::Command(c) => Self::Command(c.clear_info()),
		}
	}
}

#[allow(unused)] // TODO:
#[derive(Debug, Clone, PartialEq)]
pub enum Command {
	Let(TokenIdent, Vec<TokenIdent>, Expression),
	Maximize(Vec<(Expression, Expression)>),
	Minimize(Vec<(Expression, Expression)>),
	Print(ExpressionList),
	Solve(ExpressionList),
	Different(ExpressionList),
	Type(TokenIdent, LabelList),
	TypeDecl(TokenIdent, Expression, Option<TokenIdent>),
}

impl Command {
	pub fn clear_info(self) -> Self {
		match self {
			Self::Let(to, v, e) => Self::Let(
				to.clear_info(),
				v.into_iter().map(|th| th.clear_info()).collect(),
				e.clear_info(),
			),
			Self::Maximize(v) => Self::Maximize(
				v.into_iter()
					.map(|(e1, e2)| (e1.clear_info(), e2.clear_info()))
					.collect(),
			),
			Self::Minimize(v) => Self::Minimize(
				v.into_iter()
					.map(|(e1, e2)| (e1.clear_info(), e2.clear_info()))
					.collect(),
			),
			Self::Print(e) => Self::Print(ExpressionList(
				e.0.into_iter().map(|o| o.clear_info()).collect(),
			)),
			Self::Solve(e) => Self::Solve(ExpressionList(
				e.0.into_iter().map(|o| o.clear_info()).collect(),
			)),
			Self::Different(e) => Self::Different(ExpressionList(
				e.0.into_iter().map(|o| o.clear_info()).collect(),
			)),
			Self::Type(to, ll) => Self::Type(to.clear_info(), ll.clear_info()),
			Self::TypeDecl(ti, e, to) => Self::TypeDecl(
				ti.clear_info(),
				e.clear_info(),
				to.map(|to| to.clear_info()),
			),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum LabelList {
	List(Vec<TokenIdent>),
	Range(Expression, Expression),
}

impl LabelList {
	pub fn clear_info(self) -> Self {
		match self {
			Self::List(v) => Self::List(v.into_iter().map(|to| to.clear_info()).collect()),
			Self::Range(e1, e2) => Self::Range(e1.clear_info(), e2.clear_info()),
		}
	}
}

#[allow(unused)] // TODO:
#[derive(Debug, Clone, PartialEq)]
pub struct CompoundConstraint {
	pub children: Vec<Statement>,
	pub types: HashMap<Vec<u8>, ()>,
	pub tinfo: Option<TokenInfo>,
}

impl CompoundConstraint {
	pub fn clear_info(self) -> Self {
		let mut ret = self.clone();
		ret.children = self
			.children
			.into_iter()
			.map(|st| st.clear_info())
			.collect();
		ret
	}
}

#[allow(unused)] // TODO:
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
	BinaryOperator(TokenOperator, Box<Expression>, Box<Expression>),
	PrefixOperator(TokenOperator, Box<Expression>),
	PostfixOperator(TokenOperator, Box<Expression>),
	Number(TokenNumLiteral),
	Float(TokenFloatingLiteral),
	List(ExpressionList),
	ObjectiveVariable(TokenHash),
	String(TokenStringLiteral),
	Ident(TokenIdent),
	Keyword(TokenKeyword),
	UserFunction(TokenIdent, ExpressionList),
	InternalFunction(TokenKeyword, ExpressionList),
}

impl Expression {
	pub fn expand_hashlist(&self) -> Result<HashSet<&TokenHash>, Error> {
		let mut ret = HashSet::new();
		let mut err = Error::from_empty_list();
		if let Expression::List(ExpressionList(v)) = self {
			for item in v.iter() {
				if let Expression::ObjectiveVariable(to) = item {
					ret.insert(to);
				} else {
					err.adderr(err!(("Only hashes are allowed in this list")))?;
				}
			}
		}
		return err.ok_or_err(ret);
	}

	pub fn replace(self, table: &HashMap<Vec<u8>, Expression>) -> Expression {
		match self {
			Expression::Ident(TokenIdent(s, tinfo)) => {
				if let Some(e) = table.get(&s) {
					e.clone()
				} else {
					Expression::Ident(TokenIdent(s, tinfo))
				}
			}
			Expression::InternalFunction(tk, ExpressionList(v)) => Expression::InternalFunction(
				tk,
				ExpressionList(v.into_iter().map(|e| e.replace(table)).collect()),
			),
			Expression::UserFunction(tk, ExpressionList(v)) => Expression::UserFunction(
				tk,
				ExpressionList(v.into_iter().map(|e| e.replace(table)).collect()),
			),
			Expression::List(ExpressionList(l)) => Expression::List(ExpressionList(
				l.into_iter().map(|e| e.replace(table)).collect(),
			)),
			Expression::PrefixOperator(to, e) => {
				Expression::PrefixOperator(to, Box::new((*e).replace(table)))
			}
			Expression::PostfixOperator(to, e) => {
				Expression::PostfixOperator(to, Box::new((*e).replace(table)))
			}
			Expression::BinaryOperator(to, e1, e2) => Expression::BinaryOperator(
				to,
				Box::new((*e1).replace(table)),
				Box::new((*e2).replace(table)),
			),
			o => o,
		}
	}

	pub fn clear_info(self) -> Self {
		match self {
			Self::BinaryOperator(to, e1, e2) => Self::BinaryOperator(
				to.clear_info(),
				Box::new(e1.clear_info()),
				Box::new(e2.clear_info()),
			),
			Self::PrefixOperator(to, e1) => {
				Self::PrefixOperator(to.clear_info(), Box::new(e1.clear_info()))
			}
			Self::PostfixOperator(to, e1) => {
				Self::PostfixOperator(to.clear_info(), Box::new(e1.clear_info()))
			}
			Self::Number(to) => Self::Number(to.clear_info()),
			Self::Float(to) => Self::Float(to.clear_info()),
			Self::List(el) => Self::List(el.clear_info()),
			Self::ObjectiveVariable(to) => Self::ObjectiveVariable(to.clear_info()),
			Self::Ident(to) => Self::Ident(to.clear_info()),
			Self::Keyword(to) => Self::Keyword(to.clear_info()),
			Self::String(s) => Self::String(s.clear_info()),
			Self::UserFunction(to, el) => Self::UserFunction(to.clear_info(), el.clear_info()),
			Self::InternalFunction(to, el) => {
				Self::InternalFunction(to.clear_info(), el.clear_info())
			}
		}
	}
}

impl std::fmt::Display for Expression {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Self::BinaryOperator(to, _, _) => write!(f, "{}", to),
			Self::PrefixOperator(to, _) => write!(f, "{}", to),
			Self::PostfixOperator(to, _) => write!(f, "{}", to),
			Self::Number(o) => write!(f, "{}", o),
			Self::Float(o) => write!(f, "{}", o),
			Self::List(o) => write!(f, "{}", o),
			Self::ObjectiveVariable(o) => write!(f, "{}", o),
			Self::Ident(o) => write!(f, "{}", o),
			Self::Keyword(o) => write!(f, "{}", o),
			Self::String(o) => write!(f, "{}", String::from_utf8_lossy(&o.0)),
			Self::UserFunction(o, _) => write!(f, "{}", o),
			Self::InternalFunction(o, _) => write!(f, "{}", o),
		}
	}
}

#[allow(unused)] // TODO:
#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionList(pub Vec<Expression>);

impl std::fmt::Display for ExpressionList {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "ExpressionList")
	}
}

impl ExpressionList {
	fn clear_info(self) -> Self {
		ExpressionList(self.0.into_iter().map(|e| e.clear_info()).collect())
	}
}

#[derive(Copy, Clone, PartialEq)]
pub enum OperatorLevel {
	ConditionalOr,
	ConditionalAnd,
	ConditionalUnary,
	ConditionalCompare,
	AlgebraicAddSub,
	AlgebraicMulDiv,
	AlgebraicUnary,
}

impl OperatorLevel {
	fn new() -> Option<Self> {
		Some(Self::ConditionalOr)
	}

	fn next(&self) -> Option<Self> {
		match self {
			Self::ConditionalOr => Some(Self::ConditionalAnd),
			Self::ConditionalAnd => Some(Self::ConditionalUnary),
			Self::ConditionalUnary => Some(Self::ConditionalCompare),
			Self::ConditionalCompare => Some(Self::AlgebraicAddSub),
			Self::AlgebraicAddSub => Some(Self::AlgebraicMulDiv),
			Self::AlgebraicMulDiv => Some(Self::AlgebraicUnary),
			Self::AlgebraicUnary => None,
		}
	}
}

#[allow(dead_code)]
fn parse_expression(s: &'static str) -> Expression {
	use crate::context::Context;
	use crate::file::File;
	let mut cxt = Context::new(File::from_string(s));
	let mut tknzr = Tokenizer::new(&mut cxt);
	let mut cc = CompoundConstraint::new();
	match cc.expression(&mut tknzr, OperatorLevel::new()) {
		Ok(o) => o.clear_info(),
		Err(e) => {
			e.describe().unwrap();
			panic!();
		}
	}
}

#[test]
fn compiler_test_algebraic() {
	assert_eq!(
		parse_expression("123 + 456 - 789"),
		Expression::BinaryOperator(
			TokenOperator(Operator::Minus, None),
			Box::new(Expression::BinaryOperator(
				TokenOperator(Operator::Plus, None),
				Box::new(Expression::Number(TokenNumLiteral(123, None))),
				Box::new(Expression::Number(TokenNumLiteral(456, None)))
			)),
			Box::new(Expression::Number(TokenNumLiteral(789, None)))
		)
	);
	assert_eq!(
		parse_expression("4+(1+2)*-3--5"),
		Expression::BinaryOperator(
			TokenOperator(Operator::Minus, None),
			Box::new(Expression::BinaryOperator(
				TokenOperator(Operator::Plus, None),
				Box::new(Expression::Number(TokenNumLiteral(4, None))),
				Box::new(Expression::BinaryOperator(
					TokenOperator(Operator::Multi, None),
					Box::new(Expression::BinaryOperator(
						TokenOperator(Operator::Plus, None),
						Box::new(Expression::Number(TokenNumLiteral(1, None))),
						Box::new(Expression::Number(TokenNumLiteral(2, None)))
					)),
					Box::new(Expression::PrefixOperator(
						TokenOperator(Operator::Minus, None),
						Box::new(Expression::Number(TokenNumLiteral(3, None)))
					))
				))
			)),
			Box::new(Expression::PrefixOperator(
				TokenOperator(Operator::Minus, None),
				Box::new(Expression::Number(TokenNumLiteral(5, None)))
			))
		)
	);
}

#[test]
fn compiler_test_conditional() {
	assert_eq!(
		parse_expression("true || true && false"),
		Expression::BinaryOperator(
			TokenOperator(Operator::Or, None),
			Box::new(Expression::Keyword(TokenKeyword(Keyword::True, None))),
			Box::new(Expression::BinaryOperator(
				TokenOperator(Operator::And, None),
				Box::new(Expression::Keyword(TokenKeyword(Keyword::True, None))),
				Box::new(Expression::Keyword(TokenKeyword(Keyword::False, None)))
			))
		)
	);
	assert_eq!(
		parse_expression("!true && false || ( 3 + 2 == 5 || true )"),
		Expression::BinaryOperator(
			TokenOperator(Operator::Or, None),
			Box::new(Expression::BinaryOperator(
				TokenOperator(Operator::And, None),
				Box::new(Expression::PrefixOperator(
					TokenOperator(Operator::Not, None),
					Box::new(Expression::Keyword(TokenKeyword(Keyword::True, None)))
				)),
				Box::new(Expression::Keyword(TokenKeyword(Keyword::False, None)))
			)),
			Box::new(Expression::BinaryOperator(
				TokenOperator(Operator::Or, None),
				Box::new(Expression::BinaryOperator(
					TokenOperator(Operator::Equal, None),
					Box::new(Expression::BinaryOperator(
						TokenOperator(Operator::Plus, None),
						Box::new(Expression::Number(TokenNumLiteral(3, None))),
						Box::new(Expression::Number(TokenNumLiteral(2, None)))
					)),
					Box::new(Expression::Number(TokenNumLiteral(5, None)))
				)),
				Box::new(Expression::Keyword(TokenKeyword(Keyword::True, None)))
			))
		)
	);
	assert_eq!(
		parse_expression("( 3 + 2 ) == 5"),
		Expression::BinaryOperator(
			TokenOperator(Operator::Equal, None),
			Box::new(Expression::BinaryOperator(
				TokenOperator(Operator::Plus, None),
				Box::new(Expression::Number(TokenNumLiteral(3, None))),
				Box::new(Expression::Number(TokenNumLiteral(2, None)))
			)),
			Box::new(Expression::Number(TokenNumLiteral(5, None)))
		)
	)
}

#[allow(dead_code)]
fn parse(s: &'static str) -> Vec<Statement> {
	use crate::context::Context;
	use crate::file::File;
	let mut cxt = Context::new(File::from_string(s));
	let mut tknzr = Tokenizer::new(&mut cxt);
	match CompoundConstraint::parse(&mut tknzr) {
		Ok(o) => o.children.into_iter().map(|st| st.clear_info()).collect(),
		Err(e) => {
			e.describe().unwrap();
			panic!();
		}
	}
}

#[test]
fn compiler_test_statements() {
	assert_eq!(
		parse("let a := 123"),
		vec![Statement::Command(Command::Let(
			TokenIdent(b"a".to_vec(), None),
			vec![],
			Expression::Number(TokenNumLiteral(123, None))
		))]
	);
	assert_eq!(
		parse("type a := 1 .. 5"),
		vec![Statement::Command(Command::Type(
			TokenIdent(b"a".to_vec(), None),
			LabelList::Range(
				Expression::Number(TokenNumLiteral(1, None)),
				Expression::Number(TokenNumLiteral(5, None))
			)
		))]
	);
}

#[test]
fn compiler_test_longprog() {
	// use std::fs::File;
	// use std::io::BufRead;
	// use std::io::BufReader;
	// use std::io::Write;
	// let mut file = File::create("out.txt").unwrap();
	// write!(file, "{:?}", parse()).unwrap();
	// file.flush().unwrap();
	assert_eq!(
		parse(
			r#"
let K := 4
type bool := yes | no
bool [ @v1, @v2, @v3, @v4, @v5, @v6, @v7, @v8 ]  : vertices

minimize {
	-K : @v1 == @v1;
	1 : @v1 == yes;
	1 : @v2 == yes;
	1 : @v3 == yes;
	1 : @v4 == yes;
	1 : @v5 == yes;
	1 : @v6 == yes;
	1 : @v7 == yes;
	1 : @v8 == yes;
}

# O : joint, X : disjoint
# 1 2 3 4 5 6 7 8 v
#   X X O X O X X 1
#     O X O X X O 2
#       X O X O O 3
#         X X X O 4
#           X O O 5
#             X X 6
#               X 7
minimize {
	K * (K - 1) / 2 : @v1 == @v1;
	-1 : @v1 == yes && @v4 == yes;
	-1 : @v1 == yes && @v6 == yes;
	-1 : @v2 == yes && @v3 == yes;
	-1 : @v2 == yes && @v5 == yes;
	-1 : @v2 == yes && @v8 == yes;
	-1 : @v3 == yes && @v5 == yes;
	-1 : @v3 == yes && @v7 == yes;
	-1 : @v3 == yes && @v8 == yes;
	-1 : @v4 == yes && @v8 == yes;
	-1 : @v5 == yes && @v7 == yes;
	-1 : @v5 == yes && @v8 == yes;
}

solve(vertices)
print(vertices)

			"#
		),
		vec![
			Statement::Command(Command::Let(
				TokenIdent(b"K".to_vec(), None),
				vec![],
				Expression::Number(TokenNumLiteral(4, None))
			)),
			Statement::Command(Command::Type(
				TokenIdent(b"bool".to_vec(), None),
				LabelList::List(vec![
					TokenIdent(b"yes".to_vec(), None),
					TokenIdent(b"no".to_vec(), None)
				])
			)),
			Statement::Command(Command::TypeDecl(
				TokenIdent(b"bool".to_vec(), None),
				Expression::List(ExpressionList(vec![
					Expression::ObjectiveVariable(TokenHash(b"v1".to_vec(), None)),
					Expression::ObjectiveVariable(TokenHash(b"v2".to_vec(), None)),
					Expression::ObjectiveVariable(TokenHash(b"v3".to_vec(), None)),
					Expression::ObjectiveVariable(TokenHash(b"v4".to_vec(), None)),
					Expression::ObjectiveVariable(TokenHash(b"v5".to_vec(), None)),
					Expression::ObjectiveVariable(TokenHash(b"v6".to_vec(), None)),
					Expression::ObjectiveVariable(TokenHash(b"v7".to_vec(), None)),
					Expression::ObjectiveVariable(TokenHash(b"v8".to_vec(), None))
				])),
				Some(TokenIdent(b"vertices".to_vec(), None))
			)),
			Statement::Command(Command::Minimize(vec![
				(
					Expression::PrefixOperator(
						TokenOperator(Operator::Minus, None),
						Box::new(Expression::Ident(TokenIdent(b"K".to_vec(), None)))
					),
					Expression::BinaryOperator(
						TokenOperator(Operator::Equal, None),
						Box::new(Expression::ObjectiveVariable(TokenHash(
							b"v1".to_vec(),
							None
						))),
						Box::new(Expression::ObjectiveVariable(TokenHash(
							b"v1".to_vec(),
							None
						)))
					)
				),
				(
					Expression::Number(TokenNumLiteral(1, None)),
					Expression::BinaryOperator(
						TokenOperator(Operator::Equal, None),
						Box::new(Expression::ObjectiveVariable(TokenHash(
							b"v1".to_vec(),
							None
						))),
						Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
					)
				),
				(
					Expression::Number(TokenNumLiteral(1, None)),
					Expression::BinaryOperator(
						TokenOperator(Operator::Equal, None),
						Box::new(Expression::ObjectiveVariable(TokenHash(
							b"v2".to_vec(),
							None
						))),
						Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
					)
				),
				(
					Expression::Number(TokenNumLiteral(1, None)),
					Expression::BinaryOperator(
						TokenOperator(Operator::Equal, None),
						Box::new(Expression::ObjectiveVariable(TokenHash(
							b"v3".to_vec(),
							None
						))),
						Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
					)
				),
				(
					Expression::Number(TokenNumLiteral(1, None)),
					Expression::BinaryOperator(
						TokenOperator(Operator::Equal, None),
						Box::new(Expression::ObjectiveVariable(TokenHash(
							b"v4".to_vec(),
							None
						))),
						Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
					)
				),
				(
					Expression::Number(TokenNumLiteral(1, None)),
					Expression::BinaryOperator(
						TokenOperator(Operator::Equal, None),
						Box::new(Expression::ObjectiveVariable(TokenHash(
							b"v5".to_vec(),
							None
						))),
						Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
					)
				),
				(
					Expression::Number(TokenNumLiteral(1, None)),
					Expression::BinaryOperator(
						TokenOperator(Operator::Equal, None),
						Box::new(Expression::ObjectiveVariable(TokenHash(
							b"v6".to_vec(),
							None
						))),
						Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
					)
				),
				(
					Expression::Number(TokenNumLiteral(1, None)),
					Expression::BinaryOperator(
						TokenOperator(Operator::Equal, None),
						Box::new(Expression::ObjectiveVariable(TokenHash(
							b"v7".to_vec(),
							None
						))),
						Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
					)
				),
				(
					Expression::Number(TokenNumLiteral(1, None)),
					Expression::BinaryOperator(
						TokenOperator(Operator::Equal, None),
						Box::new(Expression::ObjectiveVariable(TokenHash(
							b"v8".to_vec(),
							None
						))),
						Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
					)
				)
			])),
			Statement::Command(Command::Minimize(vec![
				(
					Expression::BinaryOperator(
						TokenOperator(Operator::Divi, None),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Multi, None),
							Box::new(Expression::Ident(TokenIdent(b"K".to_vec(), None))),
							Box::new(Expression::BinaryOperator(
								TokenOperator(Operator::Minus, None),
								Box::new(Expression::Ident(TokenIdent(b"K".to_vec(), None))),
								Box::new(Expression::Number(TokenNumLiteral(1, None)))
							))
						)),
						Box::new(Expression::Number(TokenNumLiteral(2, None)))
					),
					Expression::BinaryOperator(
						TokenOperator(Operator::Equal, None),
						Box::new(Expression::ObjectiveVariable(TokenHash(
							b"v1".to_vec(),
							None
						))),
						Box::new(Expression::ObjectiveVariable(TokenHash(
							b"v1".to_vec(),
							None
						)))
					)
				),
				(
					Expression::PrefixOperator(
						TokenOperator(Operator::Minus, None),
						Box::new(Expression::Number(TokenNumLiteral(1, None)))
					),
					Expression::BinaryOperator(
						TokenOperator(Operator::And, None),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v1".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						)),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v4".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						))
					)
				),
				(
					Expression::PrefixOperator(
						TokenOperator(Operator::Minus, None),
						Box::new(Expression::Number(TokenNumLiteral(1, None)))
					),
					Expression::BinaryOperator(
						TokenOperator(Operator::And, None),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v1".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						)),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v6".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						))
					)
				),
				(
					Expression::PrefixOperator(
						TokenOperator(Operator::Minus, None),
						Box::new(Expression::Number(TokenNumLiteral(1, None)))
					),
					Expression::BinaryOperator(
						TokenOperator(Operator::And, None),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v2".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						)),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v3".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						))
					)
				),
				(
					Expression::PrefixOperator(
						TokenOperator(Operator::Minus, None),
						Box::new(Expression::Number(TokenNumLiteral(1, None)))
					),
					Expression::BinaryOperator(
						TokenOperator(Operator::And, None),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v2".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						)),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v5".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						))
					)
				),
				(
					Expression::PrefixOperator(
						TokenOperator(Operator::Minus, None),
						Box::new(Expression::Number(TokenNumLiteral(1, None)))
					),
					Expression::BinaryOperator(
						TokenOperator(Operator::And, None),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v2".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						)),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v8".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						))
					)
				),
				(
					Expression::PrefixOperator(
						TokenOperator(Operator::Minus, None),
						Box::new(Expression::Number(TokenNumLiteral(1, None)))
					),
					Expression::BinaryOperator(
						TokenOperator(Operator::And, None),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v3".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						)),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v5".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						))
					)
				),
				(
					Expression::PrefixOperator(
						TokenOperator(Operator::Minus, None),
						Box::new(Expression::Number(TokenNumLiteral(1, None)))
					),
					Expression::BinaryOperator(
						TokenOperator(Operator::And, None),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v3".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						)),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v7".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						))
					)
				),
				(
					Expression::PrefixOperator(
						TokenOperator(Operator::Minus, None),
						Box::new(Expression::Number(TokenNumLiteral(1, None)))
					),
					Expression::BinaryOperator(
						TokenOperator(Operator::And, None),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v3".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						)),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v8".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						))
					)
				),
				(
					Expression::PrefixOperator(
						TokenOperator(Operator::Minus, None),
						Box::new(Expression::Number(TokenNumLiteral(1, None)))
					),
					Expression::BinaryOperator(
						TokenOperator(Operator::And, None),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v4".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						)),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v8".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						))
					)
				),
				(
					Expression::PrefixOperator(
						TokenOperator(Operator::Minus, None),
						Box::new(Expression::Number(TokenNumLiteral(1, None)))
					),
					Expression::BinaryOperator(
						TokenOperator(Operator::And, None),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v5".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						)),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v7".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						))
					)
				),
				(
					Expression::PrefixOperator(
						TokenOperator(Operator::Minus, None),
						Box::new(Expression::Number(TokenNumLiteral(1, None)))
					),
					Expression::BinaryOperator(
						TokenOperator(Operator::And, None),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v5".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						)),
						Box::new(Expression::BinaryOperator(
							TokenOperator(Operator::Equal, None),
							Box::new(Expression::ObjectiveVariable(TokenHash(
								b"v8".to_vec(),
								None
							))),
							Box::new(Expression::Ident(TokenIdent(b"yes".to_vec(), None)))
						))
					)
				)
			])),
			Statement::Command(Command::Solve(ExpressionList(vec![Expression::Ident(
				TokenIdent(b"vertices".to_vec(), None)
			)]))),
			Statement::Command(Command::Print(ExpressionList(vec![Expression::Ident(
				TokenIdent(b"vertices".to_vec(), None)
			)])))
		]
	);
}

#[allow(unused)] // TODO:
impl CompoundConstraint {
	pub fn new() -> Self {
		Self {
			children: Vec::new(),
			types: HashMap::new(),
			tinfo: None,
		}
	}

	pub fn parse(tknzr: &mut Tokenizer) -> Result<Self, Error> {
		let mut ret = Self::new();
		let mut err = Error::from_empty_list();
		ret.tinfo = tknzr.get_last_tinfo();
		'l_while: while let Some(o) = tknzr.next_token()? {
			match o {
				Token::Operator(TokenOperator(Operator::CloseBracket, _)) => {
					tknzr.unget_token(o);
					break 'l_while;
				}
				o => {
					tknzr.unget_token(o);
					match ret.statement(tknzr) {
						Ok(st) => ret.children.push(st),
						Err(e) => err.add(e)?,
					}
					let _ = tknzr.eat_if(Token::Operator(TokenOperator(Operator::Semicolon, None)));
				}
			}
		}
		if err.is_empty() {
			Ok(ret)
		} else {
			Err(err)
		}
	}

	fn statement(&mut self, tknzr: &mut Tokenizer) -> Result<Statement, Error> {
		fn read_convolution(
			ret: &mut CompoundConstraint,
			tknzr: &mut Tokenizer,
		) -> Result<Vec<(Expression, Expression)>, Error> {
			let mut v = Vec::new();
			tknzr.eat(Token::Operator(TokenOperator(Operator::OpenBracket, None)))?;
			while !(tknzr.eat_if(Token::Operator(TokenOperator(Operator::CloseBracket, None)))?) {
				let e = ret.expression(tknzr, OperatorLevel::new())?;
				tknzr.eat(Token::Operator(TokenOperator(Operator::Colon, None)));
				let c = ret.expression(tknzr, OperatorLevel::new())?;
				let _ = tknzr.eat_if(Token::Operator(TokenOperator(Operator::Semicolon, None)));
				v.push((e, c));
			}
			Ok(v)
		}
		if let Some(tkn) = tknzr.next_token()? {
			match tkn {
				Token::Keyword(TokenKeyword(Keyword::Let, _)) => {
					let s = tknzr.eat_ident()?;
					let mut v = Vec::new();
					if tknzr.eat_if(Token::Operator(TokenOperator(Operator::OpenParen, None)))? {
						loop {
							v.push(tknzr.eat_ident()?);
							if !(tknzr
								.eat_if(Token::Operator(TokenOperator(Operator::Comma, None)))?)
							{
								break;
							}
						}
						tknzr.eat(Token::Operator(TokenOperator(Operator::CloseParen, None)))?;
					}
					tknzr.eat(Token::Operator(TokenOperator(Operator::DefineAs, None)))?;
					let e = self.expression(tknzr, OperatorLevel::new())?;
					return Ok(Statement::Command(Command::Let(s, v, e)));
				}
				Token::Keyword(TokenKeyword(Keyword::Maximize, _)) => {
					let r = read_convolution(self, tknzr)?;
					return Ok(Statement::Command(Command::Maximize(r)));
				}
				Token::Keyword(TokenKeyword(Keyword::Minimize, _)) => {
					let r = read_convolution(self, tknzr)?;
					return Ok(Statement::Command(Command::Minimize(r)));
				}
				Token::Keyword(TokenKeyword(Keyword::Print, _)) => {
					let v = self.read_expression_list(tknzr)?;
					return Ok(Statement::Command(Command::Print(v)));
				}
				Token::Keyword(TokenKeyword(Keyword::Solve, _)) => {
					let v = self.read_expression_list(tknzr)?;
					return Ok(Statement::Command(Command::Solve(v)));
				}
				Token::Keyword(TokenKeyword(Keyword::Different, _)) => {
					let v = self.read_expression_list(tknzr)?;
					return Ok(Statement::Command(Command::Different(v)));
				}
				Token::Keyword(TokenKeyword(Keyword::Type, tinfo)) => {
					let id = tknzr.eat_ident()?;
					tknzr.eat(Token::Operator(TokenOperator(Operator::DefineAs, None)))?;
					let e = self.expression(tknzr, OperatorLevel::new())?;
					let ll =
						if tknzr.eat_if(Token::Operator(TokenOperator(Operator::Spread, None)))? {
							let e2 = self.expression(tknzr, OperatorLevel::new())?;
							LabelList::Range(e, e2)
						} else {
							if let Expression::Ident(e) = e {
								let mut v = vec![e];
								while tknzr
									.eat_if(Token::Operator(TokenOperator(Operator::Pipe, None)))?
								{
									let id = tknzr.eat_ident()?;
									v.push(id);
								}
								LabelList::List(v)
							} else {
								return err!(("Require ident, found {}", e), [tinfo]);
							}
						};
					self.types.insert(id.clone().0, ());
					return Ok(Statement::Command(Command::Type(id, ll)));
				}
				Token::Operator(TokenOperator(Operator::OpenBracket, _)) => {
					let o = CompoundConstraint::parse(tknzr)?;
					tknzr.eat(Token::Operator(TokenOperator(Operator::CloseBracket, None)))?;
					return Ok(Statement::CompoundConstraint(o));
				}
				o => {
					if let Token::Ident(o) = o {
						let TokenIdent(id, _) = &o;
						if self.types.contains_key(id) {
							let e = self.expression(tknzr, OperatorLevel::new())?;
							let id2 = if tknzr
								.eat_if(Token::Operator(TokenOperator(Operator::Colon, None)))?
							{
								Some(tknzr.eat_ident()?)
							} else {
								None
							};
							return Ok(Statement::Command(Command::TypeDecl(o, e, id2)));
						} else {
							tknzr.unget_token(Token::Ident(o));
						}
					} else {
						tknzr.unget_token(o);
					}
					let e = self.expression(tknzr, OperatorLevel::new())?;
					return Ok(Statement::Expression(e));
				}
			}
		} else {
			unreachable!();
		}
	}

	fn read_expression_list(&mut self, tknzr: &mut Tokenizer) -> Result<ExpressionList, Error> {
		let mut v = Vec::new();
		tknzr.eat(Token::Operator(TokenOperator(Operator::OpenParen, None)))?;
		if !tknzr.eat_if(Token::Operator(TokenOperator(Operator::CloseParen, None)))? {
			loop {
				let e = self.expression(tknzr, OperatorLevel::new())?;
				v.push(e);
				if (!tknzr.eat_if(Token::Operator(TokenOperator(Operator::Comma, None)))?) {
					break;
				}
			}
			tknzr.eat(Token::Operator(TokenOperator(Operator::CloseParen, None)))?;
		}
		Ok(ExpressionList(v))
	}

	fn expression(
		&mut self,
		tknzr: &mut Tokenizer,
		level: Option<OperatorLevel>,
	) -> Result<Expression, Error> {
		if let Some(level) = level {
			if let Some(tkn) = tknzr.next_token()? {
				if let Token::Operator(op) = &tkn {
					if operator_check_level(op.0, OperatorKind::Pre, level) {
						let exp = self.expression(tknzr, Some(level))?;
						return Ok(Expression::PrefixOperator(op.clone(), Box::new(exp)));
					}
				}
				tknzr.unget_token(tkn);
			}
			let mut ret = self.expression(tknzr, level.next())?;
			loop {
				if let Some(tkn) = tknzr.next_token()? {
					if let Token::Operator(op) = &tkn {
						if operator_check_level(op.0, OperatorKind::Binary, level) {
							let exp = self.expression(tknzr, level.next())?;
							ret = Expression::BinaryOperator(
								op.clone(),
								Box::new(ret),
								Box::new(exp),
							);
							continue;
						}
					}
					tknzr.unget_token(tkn);
				}
				break;
			}
			if let Some(tkn) = tknzr.next_token()? {
				if let Token::Operator(op) = &tkn {
					if operator_check_level(op.0, OperatorKind::Post, level) {
						ret = Expression::PostfixOperator(op.clone(), Box::new(ret));
					}
				}
				tknzr.unget_token(tkn);
			}
			Ok(ret)
		} else {
			match tknzr.next_token()? {
				Some(Token::Keyword(tk)) => {
					let TokenKeyword(k, tinfo) = &tk;
					match *k {
						Keyword::Len => {
							let v = self.read_expression_list(tknzr)?;
							Ok(Expression::InternalFunction(tk, v))
						}
						Keyword::True | Keyword::False => Ok(Expression::Keyword(tk)),
						_ => err!(("Unexpected keyword {}", *k), [tinfo.clone()]),
					}
				}
				Some(Token::Operator(TokenOperator(Operator::OpenParen, _))) => {
					let res = self.expression(tknzr, OperatorLevel::new())?;
					tknzr.eat(Token::Operator(TokenOperator(Operator::CloseParen, None)));
					Ok(res)
				}
				Some(Token::NumLiteral(val)) => Ok(Expression::Number(val)),
				Some(Token::StringLiteral(s)) => Ok(Expression::String(s)),
				Some(Token::Operator(TokenOperator(Operator::OpenSq, _))) => {
					let mut l = Vec::new();
					loop {
						l.push(self.expression(tknzr, OperatorLevel::new())?);
						if let Some(tkn) = tknzr.next_token()? {
							if let Token::Operator(TokenOperator(Operator::Comma, _)) = tkn {
								continue;
							} else {
								tknzr.unget_token(tkn);
							}
						}
						break;
					}
					tknzr.eat(Token::Operator(TokenOperator(Operator::CloseSq, None)));
					Ok(Expression::List(ExpressionList(l)))
				}
				Some(Token::Hash(s)) => Ok(Expression::ObjectiveVariable(s)),
				Some(Token::Ident(s)) => {
					if let Some(tkn) = tknzr.next_token()? {
						if let Token::Operator(TokenOperator(Operator::OpenParen, _)) = &tkn {
							tknzr.unget_token(tkn);
							let v = self.read_expression_list(tknzr)?;
							Ok(Expression::UserFunction(s, v))
						} else {
							tknzr.unget_token(tkn);
							Ok(Expression::Ident(s))
						}
					} else {
						Ok(Expression::Ident(s))
					}
				}
				o => err!(("unexpected token"), [tknzr.get_last_tinfo()]),
			}
		}
	}
}
