//! Ast `.zb`.

#![expect(clippy::string_slice, reason = "We carefully check all indexes in this module")]
#![expect(
	unused_results,
	reason = "Many methods return tokens that we don't care about, only that they were consumed"
)]
#![expect(meta_variable_misuse, reason = "False positive")]

// Imports
use {
	crate::AppError,
	std::{fmt::Write, ptr, str::pattern::Pattern},
	zutil_app_error::Context,
};

/// Zbuild ast
#[derive(Clone, Debug)]
pub struct Ast<'a> {
	/// Aliases
	pub aliases: Vec<AliasStmt<'a>>,

	/// Patterns
	pub pats: Vec<PatStmt<'a>>,

	/// Default targets
	pub defaults: Vec<DefaultStmt<'a>>,

	/// Rules
	pub rules: Vec<RuleStmt<'a>>,
}

impl<'a> Ast<'a> {
	/// Parses a full ast from `input`.
	pub fn parse_full(input: &'a str) -> Result<Self, AppError> {
		let mut parser = Parser::new(input);
		let ast = Self::parse_from(&mut parser).with_context(|| {
			let remaining = parser.remaining();
			format!("Error at:\n'''\n{}\n'''", self::at_most(remaining, 100))
		})?;
		zutil_app_error::ensure!(parser.is_finished()?, "Unexpected tokens at the end");

		Ok(ast)
	}
}

impl<'a> Parsable<'a> for Ast<'a> {
	fn parse_from(parser: &mut Parser<'a>) -> Result<Self, AppError> {
		let mut aliases = vec![];
		let mut pats = vec![];
		let mut defaults = vec![];
		let mut rules = vec![];
		while !parser.is_finished()? {
			match parser
				.peek::<AnyOf4<TokenAlias<'a>, TokenPat<'a>, TokenDefault<'a>, TokenRule<'a>>>()
				.context("Expected an alias, default or rule statement")?
			{
				AnyOf4::T0(_) => aliases.push(parser.parse::<AliasStmt<'a>>()?),
				AnyOf4::T1(_) => pats.push(parser.parse::<PatStmt<'a>>()?),
				AnyOf4::T2(_) => defaults.push(parser.parse::<DefaultStmt<'a>>()?),
				AnyOf4::T3(_) => rules.push(parser.parse::<RuleStmt<'a>>()?),
			}
		}

		let ast = Self {
			aliases,
			pats,
			defaults,
			rules,
		};
		Ok(ast)
	}
}

/// Alias statement
#[derive(Clone, Debug)]
pub struct AliasStmt<'a> {
	/// Alias name
	pub name: Ident<'a>,

	/// Alias value
	pub value: Expr<'a>,
}

impl<'a> Parsable<'a> for AliasStmt<'a> {
	fn parse_from(parser: &mut Parser<'a>) -> Result<Self, AppError> {
		parser.parse::<TokenAlias<'a>>()?;
		let name = parser.parse::<Ident<'a>>().context("Expected alias name")?;
		parser.parse::<TokenEq<'a>>()?;
		let value = parser.parse::<Expr<'a>>().context("Expected alias value")?;
		parser.parse::<TokenSemi<'a>>()?;

		Ok(Self { name, value })
	}
}

/// Pattern statement
#[derive(Clone, Debug)]
pub struct PatStmt<'a> {
	/// Pattern name
	pub name: Ident<'a>,

	/// Non empty
	pub non_empty: bool,
}

impl<'a> Parsable<'a> for PatStmt<'a> {
	fn parse_from(parser: &mut Parser<'a>) -> Result<Self, AppError> {
		parser.parse::<TokenPat<'a>>()?;

		let non_empty = parser.try_parse::<TokenNonEmpty<'a>>().is_ok();

		let name = parser.parse::<Ident<'a>>().context("Expected pattern name")?;
		parser.parse::<TokenSemi<'a>>()?;


		Ok(Self { name, non_empty })
	}
}

/// Default statement
#[derive(Clone, Debug)]
pub struct DefaultStmt<'a> {
	/// Default
	pub default: Expr<'a>,
}

impl<'a> Parsable<'a> for DefaultStmt<'a> {
	fn parse_from(parser: &mut Parser<'a>) -> Result<Self, AppError> {
		parser.parse::<TokenDefault<'a>>()?;
		let default = parser.parse::<Expr<'a>>().context("Expected default expression")?;
		parser.parse::<TokenSemi<'a>>()?;

		Ok(Self { default })
	}
}

/// Rule statement
#[derive(Clone, Debug)]
pub struct RuleStmt<'a> {
	/// Rule name
	pub name: Ident<'a>,

	/// Aliases
	pub aliases: Vec<AliasStmt<'a>>,

	/// Patterns
	pub pats: Vec<PatStmt<'a>>,

	/// Output
	pub out: Array<Expr<'a>>,

	/// Dependencies
	pub deps: Array<Expr<'a>>,

	/// Execution
	pub exec: Array<Command<'a>>,
}

impl<'a> Parsable<'a> for RuleStmt<'a> {
	fn parse_from(parser: &mut Parser<'a>) -> Result<Self, AppError> {
		parser.parse::<TokenRule<'a>>()?;
		let name = parser.parse::<Ident<'a>>().context("Expected rule name")?;
		parser.parse::<TokenBracesOpen<'a>>()?;

		let mut aliases = vec![];
		let mut pats = vec![];
		let mut out = Array(vec![]);
		let mut deps = Array(vec![]);
		let mut exec = Array(vec![]);

		while parser.try_parse::<TokenBracesClose<'a>>().is_err() {
			match parser
				.peek::<AnyOf5<TokenAlias<'a>, TokenPat<'a>, TokenOut<'a>, TokenDeps<'a>, TokenExec<'a>>>()
				.context("Expected an alias, default or rule statement")?
			{
				AnyOf5::T0(_) => aliases.push(parser.parse::<AliasStmt<'_>>()?),
				AnyOf5::T1(_) => pats.push(parser.parse::<PatStmt<'_>>()?),
				AnyOf5::T2(_) => {
					parser.parse::<TokenOut<'a>>()?;
					out.0.extend(parser.parse::<Array<Expr<'a>>>()?.0);
					parser.parse::<TokenSemi<'a>>()?;
				},
				AnyOf5::T3(_) => {
					parser.parse::<TokenDeps<'a>>()?;
					deps.0.extend(parser.parse::<Array<Expr<'a>>>()?.0);
					parser.parse::<TokenSemi<'a>>()?;
				},
				AnyOf5::T4(_) => {
					parser.parse::<TokenExec<'a>>()?;
					exec.0.extend(parser.parse::<Array<Command<'a>>>()?.0);
					parser.parse::<TokenSemi<'a>>()?;
				},
			}
		}

		Ok(Self {
			name,
			aliases,
			pats,
			out,
			deps,
			exec,
		})
	}
}

/// Command
#[derive(Clone, Debug)]
pub struct Command<'a> {
	/// Working directory
	pub cwd: Option<Expr<'a>>,

	/// Arguments
	pub args: Array<Expr<'a>>,
}

impl<'a> Parsable<'a> for Command<'a> {
	fn parse_from(parser: &mut Parser<'a>) -> Result<Self, AppError> {
		let mut cwd = None;
		let mut args = None;

		type TokenBracketOpen<'a> = TokenBracesOpen<'a>;
		type TokenBracketClose<'a> = TokenBracesClose<'a>;

		match parser.try_parse::<TokenBracketOpen<'a>>() {
			// If it starts with a `{`, it's a full
			Ok(_) => loop {
				let key = parser.try_parse::<Ident<'a>>()?;
				parser.parse::<TokenColon<'a>>()?;

				match key.0 {
					"cwd" => cwd = Some(parser.parse::<Expr<'a>>()?),
					"args" => args = Some(parser.parse::<Array<Expr<'a>>>()?),
					key => zutil_app_error::bail!("Unknown key: `{key:?}`"),
				}

				match parser.parse::<AnyOf2<TokenComma<'a>, TokenBracketClose<'a>>>()? {
					// `,` or `,}`
					AnyOf2::T0(_) =>
						if parser.try_parse::<TokenBracketClose<'a>>().is_ok() {
							break;
						},
					// `}`
					AnyOf2::T1(_) => break,
				}
			},

			// Otherwise, just parse an array of expressions
			Err(_) => {
				args = Some(parser.parse::<Array<Expr<'a>>>()?);
			},
		};

		let args = args.context("Missing `args:` for command")?;

		Ok(Self { cwd, args })
	}
}

/// Array
#[derive(Clone, Debug)]
pub struct Array<T>(pub Vec<T>);

impl<'a, T> Parsable<'a> for Array<T>
where
	T: Parsable<'a>,
{
	fn parse_from(parser: &mut Parser<'a>) -> Result<Self, AppError> {
		let mut values = vec![];

		match parser.try_parse::<TokenBracketOpen<'a>>() {
			Ok(_) => {
				loop {
					let value = parser.parse::<T>()?;
					values.push(value);
					match parser.parse::<AnyOf2<TokenComma<'a>, TokenBracketClose<'a>>>()? {
						// `,` or `,]`
						AnyOf2::T0(_) =>
							if parser.try_parse::<TokenBracketClose<'a>>().is_ok() {
								break;
							},
						// `]`
						AnyOf2::T1(_) => break,
					}
				}
			},
			Err(_) => {
				// Parse at least one value
				let first_value = parser.parse::<T>()?;
				values.push(first_value);

				// Then keep parsing until we can't anymore
				while let Ok(value) = parser.try_parse::<T>() {
					values.push(value);
				}
			},
		}

		Ok(Self(values))
	}
}

/// Expression
#[derive(Clone, Debug)]
pub struct Expr<'a> {
	/// Dependencies file
	pub is_deps_file: bool,

	/// Static
	pub is_static: bool,

	/// Optional
	pub is_opt: bool,

	/// Components
	pub cmpts: Vec<ExprCmpt<'a>>,
}

impl<'a> Parsable<'a> for Expr<'a> {
	fn parse_from(parser: &mut Parser<'a>) -> Result<Self, AppError> {
		let mut is_deps_file = false;
		let mut is_static = false;
		let mut is_opt = false;

		// Parse modifiers
		while let Ok(modifier) = parser.parse::<AnyOf3<TokenDepsFile<'a>, TokenStatic<'a>, TokenOpt<'a>>>() {
			match modifier {
				AnyOf3::T0(_) => is_deps_file = true,
				AnyOf3::T1(_) => is_static = true,
				AnyOf3::T2(_) => is_opt = true,
			}
		}

		// Then parse the components
		let mut cmpts = vec![];
		ExprCmpt::parse_many(parser, &mut cmpts)?;

		Ok(Self {
			is_deps_file,
			is_static,
			is_opt,
			cmpts,
		})
	}
}

/// Expression component
#[derive(Clone, Debug)]
pub enum ExprCmpt<'a> {
	/// Identifier
	Ident { ident: Ident<'a>, ops: Vec<ExprOp> },

	/// String literal
	String(&'a str),
}

impl<'a> ExprCmpt<'a> {
	/// Parses a list of expression components from a parser
	pub fn parse_many(parser: &mut Parser<'a>, cmpts: &mut Vec<Self>) -> Result<(), AppError> {
		match parser
			.peek::<AnyOf2<TokenXIDStart<'a>, TokenDoubleQuote<'a>>>()
			.context("Expected an identifier, or literal")?
		{
			// If we get a sole identifier, that's the only component
			AnyOf2::T0(_) => {
				let ident = parser.parse::<Ident<'a>>()?;

				// Parse all of the operators
				let mut ops = vec![];
				while parser.try_parse::<TokenDot<'a>>().is_ok() {
					let op = parser.parse::<Ident<'a>>().context("Expected identifier after `.`")?;
					match op.0 {
						"dir_name" => ops.push(ExprOp::DirName),
						op => zutil_app_error::bail!("Unknown expression operator: {op:?}"),
					}
				}

				cmpts.push(ExprCmpt::Ident { ident, ops });
			},
			// If we get a literal, split all format strings inside of it.
			AnyOf2::T1(_) => {
				parser.parse::<TokenDoubleQuote<'a>>()?;
				while parser.try_parse::<TokenDoubleQuote<'a>>().is_err() {
					let Some(end_idx) = parser.remaining().find(['{', '"']) else {
						zutil_app_error::bail!("Expected closing `\"` after `\"`");
					};
					let prefix = parser.advance_by(end_idx);
					if !prefix.is_empty() {
						cmpts.push(ExprCmpt::String(prefix));
					}

					match parser
						.parse::<AnyOf2<TokenBracesOpen<'a>, TokenDoubleQuote<'a>>>()
						.expect("Just checked that one should parse")
					{
						AnyOf2::T0(_) => {
							Self::parse_many(parser, cmpts)?;
							parser.parse::<TokenBracesClose<'a>>()?;
						},
						// If the next special character we got was a `"`, we're done
						AnyOf2::T1(_) => break,
					}
				}
			},
		}

		Ok(())
	}
}

/// Expression operators
#[derive(Clone, Copy, Debug)]
pub enum ExprOp {
	/// Directory name, `.dir_name`.
	DirName,
}

/// Identifier
#[derive(Clone, Debug)]
pub struct Ident<'a>(pub &'a str);

impl<'a> Parsable<'a> for Ident<'a> {
	fn parse_from(parser: &mut Parser<'a>) -> Result<Self, AppError> {
		let orig_input = parser.remaining();
		parser
			.strip_prefix(unicode_ident::is_xid_start)
			.context("Expected `XID_START`")?;
		parser.trim_start_matches(unicode_ident::is_xid_continue);

		let ident = &orig_input[..orig_input.len() - parser.remaining().len()];
		Ok(Self(ident))
	}
}

pub macro decl_tokens($($TokenName:ident = $Token:expr;)*) {
	$(
		#[expect(dead_code, reason = "We don't need the token, but it's useful to have it.")]
		pub struct $TokenName<'a>(pub &'a str);

		impl<'a> Parsable<'a> for $TokenName<'a> {
			fn parse_from(parser: &mut Parser<'a>) -> Result<Self, AppError> {
				match parser.strip_prefix($Token) {
					Some(value) => Ok(Self(value)),
					None => zutil_app_error::bail!("Expected {:?}", $Token),
				}
			}
		}
	)*
}

decl_tokens! {
	TokenAlias = "alias";
	TokenDefault = "default";
	TokenDeps = "deps";
	TokenDepsFile = "deps_file";
	TokenExec = "exec";
	TokenNonEmpty = "non_empty";
	TokenOpt = "opt";
	TokenOut = "out";
	TokenPat = "pat";
	TokenRule = "rule";
	TokenStatic = "static";

	TokenBracesOpen = '{';
	TokenBracesClose = '}';
	TokenBracketOpen = '[';
	TokenBracketClose = ']';

	TokenColon = ':';
	TokenComma = ',';
	TokenDot = '.';
	TokenDoubleQuote = '"';
	TokenEq = '=';
	TokenSemi = ';';
}

#[expect(dead_code, reason = "We don't need the token, but it's useful to have it.")]
struct TokenXIDStart<'a>(pub &'a str);

impl<'a> Parsable<'a> for TokenXIDStart<'a> {
	fn parse_from(parser: &mut Parser<'a>) -> Result<Self, AppError> {
		parser
			.strip_prefix(unicode_ident::is_xid_start)
			.map(Self)
			.context("Expected `XID_START`")
	}
}

pub trait Parsable<'a>: Sized {
	/// Parses this type from `input`, mutating it in-place.
	fn parse_from(parser: &mut Parser<'a>) -> Result<Self, AppError>;
}

/// Parser
#[derive(Clone, Debug)]
pub struct Parser<'a> {
	/// Input
	input: &'a str,
}

impl<'a> Parser<'a> {
	/// Creates a new parser
	pub const fn new(input: &'a str) -> Self {
		Self { input }
	}

	/// Returns the remaining string for the parser
	pub const fn remaining(&self) -> &'a str {
		self.input
	}

	/// Returns the character at `idx`.
	///
	/// Panics if `idx` isn't a utf-8 codepoint boundary.
	/// Panics if `idx` is out of bounds.
	pub fn _ch_at(&self, idx: usize) -> char {
		self.input[idx..].chars().next().expect("Index was out of bounds")
	}

	/// Returns if the parser is finished
	pub fn is_finished(&mut self) -> Result<bool, AppError> {
		self.trim()?;

		Ok(self.input.is_empty())
	}

	/// Advances the parser by `len` bytes.
	///
	/// Panics if `idx` isn't a utf-8 codepoint boundary.
	pub fn advance_by(&mut self, len: usize) -> &'a str {
		let value = &self.input[..len];
		self.input = &self.input[len..];
		value
	}

	/// Trims non-parsable input, such as:
	///
	/// - Whitespace
	/// - Comments
	pub fn trim(&mut self) -> Result<(), AppError> {
		while self
			.input
			.starts_with(|ch: char| ch.is_whitespace() || matches!(ch, '#'))
		{
			// Trim whitespace
			self.input = self.input.trim_start();

			// Then trim comments
			if let Some(rest) = self.input.strip_prefix("###") {
				let end_idx = rest.find("###").context("Expected `###` after `###`")?;
				self.input = &rest[end_idx + 3..];
			} else if let Some(rest) = self.input.strip_prefix('#') {
				let end_idx = rest.find('\n').unwrap_or(rest.len());
				self.input = &rest[end_idx + 1..];
			}
		}

		Ok(())
	}

	/// Strips a prefix from the parser
	pub fn strip_prefix<P: Pattern>(&mut self, prefix: P) -> Option<&'a str> {
		let rest = self.input.strip_prefix(prefix)?;
		let value = &self.input[..self.input.len() - rest.len()];
		self.input = rest;
		Some(value)
	}

	/// Trims all matching prefixes from the parser
	pub fn trim_start_matches<P: Pattern>(&mut self, pat: P) {
		self.input = self.input.trim_start_matches(pat);
	}

	/// Parses `T` from this parser
	pub fn parse<T: Parsable<'a>>(&mut self) -> Result<T, AppError> {
		self.trim()?;
		T::parse_from(self)
	}

	/// Tries to parses `T` from this parser.
	///
	/// On error, nothing is modified.
	pub fn try_parse<T: Parsable<'a>>(&mut self) -> Result<T, AppError> {
		let mut parser = self.clone();
		let value = parser.parse::<T>()?;

		*self = parser;
		Ok(value)
	}

	/// Peeks `T` from this parser, without advancing it
	pub fn peek<T: Parsable<'a>>(&self) -> Result<T, AppError> {
		self.clone().parse::<T>()
	}
}

macro decl_any_of($Name:ident, $($T:ident),* $(,)?) {
	pub enum $Name< $($T),* > {
		$( $T($T) ),*
	}

	impl<'a, $($T),*> Parsable<'a> for $Name<$($T),*>
	where
		$(
			$T: Parsable<'a>,
		)*
	{
		fn parse_from(parser: &mut Parser<'a>) -> Result<Self, AppError> {
			#![expect(non_snake_case, reason = "Macro generated")]

			$(
				let mut ${concat(parser_, $T)} = parser.clone();
				let ${concat(err_, $T)} = match ${concat(parser_, $T)}.parse::<$T>() {
					Ok(value) => {
						*parser = ${concat(parser_, $T)};
						return Ok(Self::$T(value));
					},
					Err(err) => err,
				};
			)*

			let mut err = format!("Expected one of the following {} matches:", ${count($T)});
			$(
				match ptr::eq(${concat(parser_, $T)}.remaining(), parser.remaining()) {
					// If the parser hasn't moved, don't print the postion
					true => write!(err, "\n{}", ${concat(err_, $T)}),
					// Otherwise, print the error and position
					false => write!(err, "\n{} at {:?}", ${concat(err_, $T)}, self::at_most(${concat(parser_, $T)}.remaining(), 50)),
				}.expect("Failed to write into string");
			)*

			Err(zutil_app_error::AppError::msg(err))
		}
	}
}

decl_any_of!(AnyOf2, T0, T1);
decl_any_of!(AnyOf3, T0, T1, T2);
decl_any_of!(AnyOf4, T0, T1, T2, T3);
decl_any_of!(AnyOf5, T0, T1, T2, T3, T4);

fn at_most(s: &str, max: usize) -> String {
	match s.len() > max {
		true => format!("{}[...]", &s[..max]),
		false => s.to_owned(),
	}
}
