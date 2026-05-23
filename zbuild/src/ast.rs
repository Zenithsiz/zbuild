//! Ast `.zb`.

#![expect(
	unused_results,
	reason = "Many methods return tokens that we don't care about, only that they were consumed"
)]
#![expect(meta_variable_misuse, reason = "False positive")]

// Imports
use {
	crate::{AppError, util::ArcStr},
	app_error::Context,
	std::{fmt::Write, fs, mem, path::Path, ptr, str::pattern::Pattern},
};

/// Zbuild ast
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Ast {
	/// Aliases
	pub aliases: Vec<AliasStmt>,

	/// Patterns
	pub pats: Vec<PatStmt>,

	/// Default targets
	pub defaults: Vec<DefaultStmt>,

	/// Rules
	pub rules: Vec<RuleStmt>,

	/// Includes
	pub includes: Vec<IncludeStmt>,
}

impl Parsable for Ast {
	fn parse_from(parser: &mut Parser) -> Result<Self, AppError> {
		let mut aliases = vec![];
		let mut pats = vec![];
		let mut defaults = vec![];
		let mut rules = vec![];
		let mut includes = vec![];
		while !parser.is_finished()? {
			match parser.peek::<AnyOf5<TokenAlias, TokenPat, TokenDefault, TokenRule, TokenInclude>>()? {
				AnyOf5::T0(_) => aliases.push(parser.parse::<AliasStmt>()?),
				AnyOf5::T1(_) => pats.push(parser.parse::<PatStmt>()?),
				AnyOf5::T2(_) => defaults.push(parser.parse::<DefaultStmt>()?),
				AnyOf5::T3(_) => rules.push(parser.parse::<RuleStmt>()?),
				AnyOf5::T4(_) => includes.push(parser.parse::<IncludeStmt>()?),
			}
		}

		let ast = Self {
			aliases,
			pats,
			defaults,
			rules,
			includes,
		};
		Ok(ast)
	}
}

/// Alias statement
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct AliasStmt {
	/// Alias name
	pub name: Ident,

	/// Alias value
	pub value: Expr,
}

impl Parsable for AliasStmt {
	fn parse_from(parser: &mut Parser) -> Result<Self, AppError> {
		parser.parse::<TokenAlias>()?;
		let name = parser.parse::<Ident>().context("Expected alias name")?;
		parser.parse::<TokenEq>()?;
		let value = parser.parse::<Expr>().context("Expected alias value")?;
		parser.parse::<TokenSemi>()?;

		Ok(Self { name, value })
	}
}

/// Pattern statement
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct PatStmt {
	/// Pattern name
	pub name: Ident,

	/// Non empty
	pub non_empty: bool,
}

impl Parsable for PatStmt {
	fn parse_from(parser: &mut Parser) -> Result<Self, AppError> {
		parser.parse::<TokenPat>()?;

		let non_empty = parser.try_parse::<TokenNonEmpty>().is_ok();

		let name = parser.parse::<Ident>().context("Expected pattern name")?;
		parser.parse::<TokenSemi>()?;


		Ok(Self { name, non_empty })
	}
}

/// Default statement
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct DefaultStmt {
	/// Default
	pub default: Expr,
}

impl Parsable for DefaultStmt {
	fn parse_from(parser: &mut Parser) -> Result<Self, AppError> {
		parser.parse::<TokenDefault>()?;
		let default = parser.parse::<Expr>().context("Expected default expression")?;
		parser.parse::<TokenSemi>()?;

		Ok(Self { default })
	}
}

/// Rule statement
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct RuleStmt {
	/// Rule name
	pub name: Ident,

	/// Aliases
	pub aliases: Vec<AliasStmt>,

	/// Patterns
	pub pats: Vec<PatStmt>,

	/// Output
	pub out: Vec<Expr>,

	/// Dependencies
	pub deps: Vec<DepStmt>,

	/// Execution
	pub exec: Vec<Command>,
}

impl Parsable for RuleStmt {
	fn parse_from(parser: &mut Parser) -> Result<Self, AppError> {
		parser.parse::<TokenRule>()?;
		let name = parser.parse::<Ident>().context("Expected rule name")?;
		parser.parse::<TokenBracesOpen>()?;

		let mut aliases = vec![];
		let mut pats = vec![];
		let mut out = vec![];
		let mut deps = vec![];
		let mut exec = vec![];

		while parser.try_parse::<TokenBracesClose>().is_err() {
			match parser
				.peek::<AnyOf5<TokenAlias, TokenPat, TokenOut, TokenDep, TokenExec>>()
				.context("Expected an alias, default or rule statement")?
			{
				AnyOf5::T0(_) => aliases.push(parser.parse::<AliasStmt>()?),
				AnyOf5::T1(_) => pats.push(parser.parse::<PatStmt>()?),
				AnyOf5::T2(_) => {
					parser.parse::<TokenOut>()?;
					out.push(parser.parse::<Expr>()?);
					parser.parse::<TokenSemi>()?;
				},
				AnyOf5::T3(_) => deps.push(parser.parse::<DepStmt>()?),
				AnyOf5::T4(_) => {
					parser.parse::<TokenExec>()?;
					exec.push(parser.parse::<Command>()?);
					parser.parse::<TokenSemi>()?;
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

/// Dependency statement
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum DepStmt {
	/// File
	File(Expr),

	/// Rule
	Rule(Ident),
}

impl Parsable for DepStmt {
	fn parse_from(parser: &mut Parser) -> Result<Self, AppError> {
		parser.parse::<TokenDep>()?;

		let dep = match parser.try_parse::<TokenRule>() {
			Ok(_) => Self::Rule(parser.parse::<Ident>()?),
			Err(_) => Self::File(parser.parse::<Expr>()?),
		};
		parser.parse::<TokenSemi>()?;

		Ok(dep)
	}
}

/// Command
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Command {
	/// Working directory
	pub cwd: Option<Expr>,

	/// Stdout
	pub stdout: Option<Ident>,

	/// Arguments
	pub args: Array<Expr>,
}

impl Parsable for Command {
	fn parse_from(parser: &mut Parser) -> Result<Self, AppError> {
		let mut cwd = None;
		let mut stdout = None;
		let mut args = None;

		match parser.try_parse::<TokenBracesOpen>() {
			// If it starts with a `{`, it's a full
			Ok(_) =>
				while parser.try_parse::<TokenBracesClose>().is_err() {
					match parser
						.peek::<AnyOf3<TokenCwd, TokenStdout, TokenArgs>>()
						.context("Expected an alias, default or rule statement")?
					{
						AnyOf3::T0(_) => {
							parser.parse::<TokenCwd>()?;
							app_error::ensure!(cwd.is_none(), "Working directory was already specified");
							cwd = Some(parser.parse::<Expr>()?);
							parser.parse::<TokenSemi>()?;
						},
						AnyOf3::T1(_) => {
							parser.parse::<TokenStdout>()?;
							app_error::ensure!(stdout.is_none(), "Stdout was already specified");
							stdout = Some(parser.parse::<Ident>()?);
							parser.parse::<TokenSemi>()?;
						},
						AnyOf3::T2(_) => {
							parser.parse::<TokenArgs>()?;
							app_error::ensure!(args.is_none(), "Arguments were already specified");
							args = Some(parser.parse::<Array<Expr>>()?);
							parser.parse::<TokenSemi>()?;
						},
					}
				},

			// Otherwise, just parse an array of expressions
			Err(_) => {
				args = Some(parser.parse::<Array<Expr>>()?);
			},
		}

		let args = args.context("Missing command `args`")?;

		Ok(Self { cwd, stdout, args })
	}
}

/// Include statement
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct IncludeStmt {
	/// Path
	pub path: StringLiteral,
}

impl Parsable for IncludeStmt {
	fn parse_from(parser: &mut Parser) -> Result<Self, AppError> {
		parser.parse::<TokenInclude>()?;
		let path = parser.parse::<StringLiteral>().context("Expected include path")?;
		parser.parse::<TokenSemi>()?;

		Ok(Self { path })
	}
}

/// Array
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Array<T>(pub Vec<T>);

impl<T> Parsable for Array<T>
where
	T: Parsable,
{
	fn parse_from(parser: &mut Parser) -> Result<Self, AppError> {
		let mut values = vec![];

		match parser.try_parse::<TokenBracketOpen>() {
			Ok(_) => {
				loop {
					let value = parser.parse::<T>()?;
					values.push(value);
					match parser.parse::<AnyOf2<TokenComma, TokenBracketClose>>()? {
						// `,` or `,]`
						AnyOf2::T0(_) =>
							if parser.try_parse::<TokenBracketClose>().is_ok() {
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
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Expr {
	/// Dependencies file
	pub is_deps_file: bool,

	/// Static
	pub is_static: bool,

	/// Optional
	pub is_opt: bool,

	/// Components
	pub cmpts: Vec<ExprCmpt>,
}

impl Parsable for Expr {
	fn parse_from(parser: &mut Parser) -> Result<Self, AppError> {
		let mut is_deps_file = false;
		let mut is_static = false;
		let mut is_opt = false;

		// Parse modifiers
		while let Ok(modifier) = parser.parse::<AnyOf3<TokenDepsFile, TokenStatic, TokenOpt>>() {
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
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ExprCmpt {
	/// Identifier
	Ident { ident: Ident, ops: Vec<ExprOp> },

	/// String literal
	String(ArcStr),
}

impl ExprCmpt {
	/// Parses a list of expression components from a parser
	pub fn parse_many(parser: &mut Parser, cmpts: &mut Vec<Self>) -> Result<(), AppError> {
		match parser
			.peek::<AnyOf2<TokenXIDStart, TokenDoubleQuote>>()
			.context("Expected an identifier or literal")?
		{
			// If we get a sole identifier, that's the only component
			AnyOf2::T0(_) => {
				let ident = parser.parse::<Ident>()?;

				// Parse all of the operators
				let mut ops = vec![];
				while parser.try_parse::<TokenDot>().is_ok() {
					let op = parser.parse::<Ident>().context("Expected identifier after `.`")?;
					match &*op.0 {
						"dir_name" => ops.push(ExprOp::DirName),
						op => app_error::bail!("Unknown expression operator: {op:?}"),
					}
				}

				cmpts.push(Self::Ident { ident, ops });
			},
			// If we get a literal, split all format strings inside of it.
			AnyOf2::T1(_) => {
				parser.parse::<TokenDoubleQuote>()?;
				while parser.try_parse::<TokenDoubleQuote>().is_err() {
					let Some(end_idx) = parser.remaining().find(['{', '"']) else {
						app_error::bail!("Expected closing `\"` after `\"`");
					};
					let prefix = parser.advance_by(end_idx);
					if !prefix.is_empty() {
						cmpts.push(Self::String(prefix));
					}

					match parser
						.parse::<AnyOf2<TokenBracesOpen, TokenDoubleQuote>>()
						.expect("Just checked that one should parse")
					{
						AnyOf2::T0(_) => {
							Self::parse_many(parser, cmpts)?;
							parser.parse::<TokenBracesClose>()?;
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
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ExprOp {
	/// Directory name, `.dir_name`.
	DirName,
}

/// Identifier
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Ident(pub ArcStr);

impl Parsable for Ident {
	fn parse_from(parser: &mut Parser) -> Result<Self, AppError> {
		let orig_input = parser.remaining();
		parser
			.strip_prefix(unicode_ident::is_xid_start)
			.context("Expected `XID_START`")?;
		parser.trim_start_matches(unicode_ident::is_xid_continue);

		let ident = orig_input.slice(..orig_input.len() - parser.remaining().len());
		Ok(Self(ident))
	}
}

/// String literal
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct StringLiteral(ArcStr);

impl Parsable for StringLiteral {
	fn parse_from(parser: &mut Parser) -> Result<Self, AppError> {
		parser.parse::<TokenDoubleQuote>()?;
		let len = parser.remaining().find('"').context("Expected `\"` after `\"`")?;
		let s = parser.advance_by(len);
		parser.parse::<TokenDoubleQuote>()?;

		Ok(Self(s))
	}
}

pub macro decl_tokens($($TokenName:ident = $Token:expr;)*) {
	$(
		#[expect(dead_code, reason = "We don't need the token, but it's useful to have it.")]
		pub struct $TokenName(pub ArcStr);

		impl Parsable for $TokenName {
			fn parse_from(parser: &mut Parser) -> Result<Self, AppError> {
				match parser.strip_prefix($Token) {
					Some(value) => Ok(Self(value.into())),
					None => app_error::bail!("Expected {:?}", $Token),
				}
			}
		}
	)*
}

decl_tokens! {
	TokenAlias = "alias";
	TokenArgs = "args";
	TokenCwd = "cwd";
	TokenDefault = "default";
	TokenDep = "dep";
	TokenDepsFile = "deps_file";
	TokenExec = "exec";
	TokenInclude = "include";
	TokenNonEmpty = "non_empty";
	TokenOpt = "opt";
	TokenOut = "out";
	TokenPat = "pat";
	TokenRule = "rule";
	TokenStatic = "static";
	TokenStdout = "stdout";

	TokenBracesOpen = '{';
	TokenBracesClose = '}';
	TokenBracketOpen = '[';
	TokenBracketClose = ']';

	TokenComma = ',';
	TokenDot = '.';
	TokenDoubleQuote = '"';
	TokenEq = '=';
	TokenSemi = ';';
}

#[expect(dead_code, reason = "We don't need the token, but it's useful to have it.")]
struct TokenXIDStart(pub ArcStr);

impl Parsable for TokenXIDStart {
	fn parse_from(parser: &mut Parser) -> Result<Self, AppError> {
		parser
			.strip_prefix(unicode_ident::is_xid_start)
			.map(Self)
			.context("Expected `XID_START`")
	}
}

pub trait Parsable: Sized {
	/// Parses this type from `input`, mutating it in-place.
	fn parse_from(parser: &mut Parser) -> Result<Self, AppError>;
}

/// Parser
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Parser {
	/// Input
	input: ArcStr,

	/// Current position
	cur_pos: usize,
}

impl Parser {
	/// Creates a new parser
	pub const fn new(input: ArcStr) -> Self {
		Self { input, cur_pos: 0 }
	}

	/// Returns the remaining string for the parser
	pub fn remaining(&self) -> ArcStr {
		self.input.slice(self.cur_pos..)
	}

	/// Returns the current line of the parser, not including the end
	pub fn cur_line(&self) -> ArcStr {
		let start = self.input[..self.cur_pos].rfind('\n').map_or(0, |idx| idx + 1);
		let end = self.cur_pos +
			self.input[self.cur_pos..]
				.find('\n')
				.unwrap_or(self.input.len() - self.cur_pos);

		self.input.slice(start..end)
	}

	/// Gets the current line (0-indexed) of the parser
	// TODO: Make this less expensive?
	pub fn cur_line_pos(&self) -> usize {
		self.input[..self.cur_pos].chars().filter(|&ch| ch == '\n').count()
	}

	/// Gets the current column (0-indexed) of the parser
	pub fn cur_col_pos(&self) -> usize {
		match self.input[..self.cur_pos].rfind('\n') {
			Some(newline_pos) => self.cur_pos - newline_pos - 1,
			None => self.cur_pos,
		}
	}

	/// Returns if the parser is finished
	pub fn is_finished(&mut self) -> Result<bool, AppError> {
		self.trim()?;

		Ok(self.remaining().is_empty())
	}

	/// Advances the parser by `len` bytes.
	///
	/// Panics if `idx` isn't a utf-8 codepoint boundary.
	pub fn advance_by(&mut self, len: usize) -> ArcStr {
		let prev_pos = self.cur_pos;
		self.cur_pos += len;
		assert!(self.input.is_char_boundary(self.cur_pos));

		self.input.slice(prev_pos..self.cur_pos)
	}

	/// Updates this parser from a string.
	///
	/// The output must be a substring of the input.
	/// Regardless of it's length, everything from it's
	/// start to the end will be set as the remaining.
	pub fn update_with<F>(&mut self, f: F) -> ArcStr
	where
		F: FnOnce(&str) -> &str,
	{
		self.try_update_with(|remaining| Ok::<_, !>(f(remaining))).into_ok()
	}

	/// Updates this parser from a string.
	///
	/// See [`Self::update_with`] for more details.
	pub fn try_update_with<F, E>(&mut self, f: F) -> Result<ArcStr, E>
	where
		// TODO: Make this GATs once we can create a `TryFnOnce(&str) -> &str` that
		//       we can change the output type of.
		F: FnOnce(&str) -> Result<&str, E>,
	{
		let remaining = self.remaining();
		let output = f(&remaining)?;

		let range = remaining
			.substr_range(output)
			.expect("Result was not a substring of the input");
		self.cur_pos += range.start;

		Ok(self.input.slice_from_str(output))
	}

	/// Trims non-parsable input, such as:
	///
	/// - Whitespace
	/// - Comments
	pub fn trim(&mut self) -> Result<(), AppError> {
		while self
			.remaining()
			.starts_with(|ch: char| ch.is_whitespace() || matches!(ch, '#'))
		{
			// Trim whitespace
			self.update_with(|remaining| remaining.trim_start());

			// Then trim comments
			self.try_update_with(|remaining| {
				if let Some(rest) = remaining.strip_prefix("###") {
					let end_idx = rest.find("###").context("Expected `###` after `###`")?;
					Ok(&rest[end_idx + 3..])
				} else if let Some(rest) = remaining.strip_prefix('#') {
					match rest.find('\n') {
						Some(end_idx) => Ok(&rest[end_idx + 1..]),
						None => Ok(&rest[rest.len()..]),
					}
				} else {
					Ok::<_, AppError>(remaining)
				}
			})?;
		}

		Ok(())
	}

	/// Strips a prefix from the parser
	pub fn strip_prefix<P: Pattern>(&mut self, prefix: P) -> Option<ArcStr> {
		self.try_update_with(|remaining| remaining.strip_prefix(prefix).ok_or(()))
			.ok()
	}

	/// Trims all matching prefixes from the parser
	pub fn trim_start_matches<P: Pattern>(&mut self, pat: P) {
		self.update_with(|remaining| remaining.trim_start_matches(pat));
	}

	/// Parses `T` from this parser
	pub fn parse<T: Parsable>(&mut self) -> Result<T, AppError> {
		self.trim()?;
		T::parse_from(self)
	}

	/// Tries to parses `T` from this parser.
	///
	/// On error, nothing is modified.
	pub fn try_parse<T: Parsable>(&mut self) -> Result<T, AppError> {
		self.trim()?;
		let mut parser = self.clone();
		let value = parser.parse::<T>()?;

		*self = parser;
		Ok(value)
	}

	/// Peeks `T` from this parser, without advancing it
	pub fn peek<T: Parsable>(&mut self) -> Result<T, AppError> {
		self.trim()?;
		self.clone().parse::<T>()
	}
}

macro decl_any_of($Name:ident, $($T:ident),* $(,)?) {
	pub enum $Name< $($T),* > {
		$( $T($T) ),*
	}

	impl<$($T),*> Parsable for $Name<$($T),*>
	where
		$(
			$T: Parsable,
		)*
	{
		fn parse_from(parser: &mut Parser) -> Result<Self, AppError> {
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
				match ptr::eq(&*${concat(parser_, $T)}.remaining(), &*parser.remaining()) {
					// If the parser hasn't moved, don't print the position
					true => write!(err, "\n{}", ${concat(err_, $T)}),
					// Otherwise, print the error and position
					false => write!(err, "\n{} at {}:{}",
						${concat(err_, $T)},
						${concat(parser_, $T)}.cur_line_pos(),
						${concat(parser_, $T)}.cur_col_pos(),
					),
				}.expect("Failed to write into string");
			)*

			Err(AppError::fmt(err))
		}
	}
}

decl_any_of!(AnyOf2, T0, T1);
decl_any_of!(AnyOf3, T0, T1, T2);
decl_any_of!(AnyOf5, T0, T1, T2, T3, T4);

/// Fully parses the ast from `path`.
pub fn parse(path: &Path) -> Result<Ast, AppError> {
	let input = fs::read_to_string(path).with_context(|| format!("Unable to read zbuild file {path:?}"))?;
	let mut parser = Parser::new(ArcStr::from(input));

	// Parse the initial AST
	let mut ast = Ast::parse_from(&mut parser).with_context(|| {
		// TODO: Deal with tabs better here?

		let line = parser.cur_line().replace('\t', "    ");
		let line_pos = parser.cur_line_pos() + 1;
		let col_pos = parser.cur_col_pos() + 1;

		let tabs = parser.cur_line().chars().filter(|&ch| ch == '\t').count();
		let ident = " ".repeat(parser.cur_col_pos() + tabs * 3);
		format!("Error at {}:{line_pos}:{col_pos}:\n{line}\n{ident}^", path.display())
	})?;

	// Then recursively parse any imports and merge them into the ast
	// TODO: Should these be scoped?
	for include in mem::take(&mut ast.includes) {
		let include_path = path
			.parent()
			.expect("File had no parent directory")
			.join(&*include.path.0);
		let mut include_ast =
			self::parse(&include_path).with_context(|| format!("Unable to parse {include_path:?}"))?;

		ast.aliases.append(&mut include_ast.aliases);
		ast.pats.append(&mut include_ast.pats);
		ast.defaults.append(&mut include_ast.defaults);
		ast.rules.append(&mut include_ast.rules);
		assert!(ast.includes.is_empty(), "Includes were not recursively handled");
	}

	Ok(ast)
}

#[cfg(test)]
#[coverage(off)]
mod tests {
	use super::*;

	const EMPTY_AST: Ast = Ast {
		aliases:  vec![],
		pats:     vec![],
		defaults: vec![],
		rules:    vec![],
		includes: vec![],
	};
	const EMPTY_EXPR: Expr = Expr {
		is_deps_file: false,
		is_static:    false,
		is_opt:       false,
		cmpts:        vec![],
	};
	const fn ident(s: &'static str) -> Ident {
		Ident(ArcStr::from_static(s))
	}
	fn expr_string(s: &str) -> Expr {
		Expr {
			is_deps_file: false,
			is_static:    false,
			is_opt:       false,
			cmpts:        vec![ExprCmpt::String(s.into())],
		}
	}
	fn alias_string(name: &'static str, value: &str) -> AliasStmt {
		AliasStmt {
			name:  ident(name),
			value: expr_string(value),
		}
	}
	fn pat(name: &'static str) -> PatStmt {
		PatStmt {
			name:      ident(name),
			non_empty: false,
		}
	}
	fn pat_non_empty(name: &'static str) -> PatStmt {
		PatStmt {
			name:      ident(name),
			non_empty: true,
		}
	}
	fn default(s: &str) -> DefaultStmt {
		DefaultStmt {
			default: expr_string(s),
		}
	}
	fn array_expr_string<const N: usize>(s: [&'static str; N]) -> Array<Expr> {
		Array(s.into_iter().map(expr_string).collect())
	}
	fn expr_cmpt_string(s: &str) -> ExprCmpt {
		ExprCmpt::String(s.into())
	}
	fn expr_cmpt_ident(s: &'static str, ops: Vec<ExprOp>) -> ExprCmpt {
		ExprCmpt::Ident { ident: ident(s), ops }
	}

	#[track_caller]
	fn check_cases_fail<T: Parsable + std::fmt::Debug>(cases: impl IntoIterator<Item = (&'static str, &'static str)>) {
		for (input, expected_err) in cases {
			let mut parser = Parser::new(input.into());
			let err = match T::parse_from(&mut parser) {
				Ok(value) => panic!("Input was unexpectedly valid {input:?}: {value:?}"),
				Err(err) => err,
			};
			assert!(
				err.pretty().to_string().contains(expected_err),
				"Error did not contain expected output for {input:?} ({expected_err:?}): {}",
				err.pretty()
			);
		}
	}

	#[track_caller]
	fn check_cases<T: Parsable + PartialEq + std::fmt::Debug>(cases: impl IntoIterator<Item = (&'static str, T)>) {
		for (input, expected) in cases {
			let mut parser = Parser::new(input.into());
			let value = T::parse_from(&mut parser)
				.unwrap_or_else(|err| panic!("Unable to parse input {input:?}: {}", err.pretty()));
			assert!(
				value == expected,
				"Output differed for {input:?}\n  Expected: {expected:?}\n  Found   : {value:?}"
			);
			assert!(
				parser.remaining().is_empty(),
				"Parser had remaining tokens for input {input:?}: {:?}",
				parser.remaining()
			);
		}
	}

	#[test]
	fn parse_ast() {
		self::check_cases([
			// Whitespace
			("", EMPTY_AST),
			("  ", EMPTY_AST),
			// Comments
			("#Comment\n", EMPTY_AST),
			("#Comment", EMPTY_AST),
			("###Comment###", EMPTY_AST),
			// Global aliases
			(r#"alias a = "test"; alias b = "test2";"#, Ast {
				aliases: vec![alias_string("a", "test"), alias_string("b", "test2")],
				..EMPTY_AST
			}),
			// Global patterns
			("pat a; pat non_empty b;", Ast {
				pats: vec![pat("a"), pat_non_empty("b")],
				..EMPTY_AST
			}),
			// Default
			(r#"default "a";"#, Ast {
				defaults: vec![default("a")],
				..EMPTY_AST
			}),
			// Rules
			(
				r#"rule a { alias a = "test"; pat b; out "out"; dep "dep"; exec "cmd"; }"#,
				Ast {
					rules: vec![RuleStmt {
						name:    ident("a"),
						aliases: vec![alias_string("a", "test")],
						pats:    vec![pat("b")],
						out:     vec![expr_string("out")],
						deps:    vec![DepStmt::File(expr_string("dep"))],
						exec:    vec![Command {
							cwd:    None,
							stdout: None,
							args:   Array(vec![expr_string("cmd")]),
						}],
					}],
					..EMPTY_AST
				},
			),
			// Includes
			(r#"include "a/b.zb";"#, Ast {
				includes: vec![IncludeStmt {
					path: StringLiteral(ArcStr::from_static("a/b.zb")),
				}],
				..EMPTY_AST
			}),
		]);

		self::check_cases_fail::<Ast>([
			("unknown", "Expected one of the following"),
			//
		]);
	}

	#[test]
	fn parse_alias() {
		check_cases_fail::<AliasStmt>([("alias ", "Expected alias name")]);
	}

	#[test]
	fn parse_dep() {
		check_cases([
			(r#"dep "a";"#, DepStmt::File(expr_string("a"))),
			("dep rule a;", DepStmt::Rule(ident("a"))),
		]);
	}

	#[test]
	fn parse_cmd() {
		self::check_cases([
			(r#""cmd""#, Command {
				cwd:    None,
				stdout: None,
				args:   Array(vec![expr_string("cmd")]),
			}),
			(r#""cmd" "a" "b""#, Command {
				cwd:    None,
				stdout: None,
				args:   Array(vec![expr_string("cmd"), expr_string("a"), expr_string("b")]),
			}),
			(r#"{ cwd "a/b/c"; stdout output; args "cmd" "a" "b"; }"#, Command {
				cwd:    Some(expr_string("a/b/c")),
				stdout: Some(ident("output")),
				args:   Array(vec![expr_string("cmd"), expr_string("a"), expr_string("b")]),
			}),
		]);
	}

	#[test]
	fn parse_array() {
		self::check_cases([
			(r#""a" "b" "c""#, array_expr_string(["a", "b", "c"])),
			(r#"["a", "b", "c"]"#, array_expr_string(["a", "b", "c"])),
			(r#"["a", "b", "c",]"#, array_expr_string(["a", "b", "c"])),
		]);
	}

	#[test]
	fn parse_expr() {
		self::check_cases([
			(r#""""#, EMPTY_EXPR),
			(r#"deps_file static opt "a""#, Expr {
				is_deps_file: true,
				is_static: true,
				is_opt: true,
				..expr_string("a")
			}),
			(r#""a{b}c""#, Expr {
				cmpts: vec![
					expr_cmpt_string("a"),
					expr_cmpt_ident("b", vec![]),
					expr_cmpt_string("c"),
				],
				..EMPTY_EXPR
			}),
			(r#""a{b.dir_name}c""#, Expr {
				cmpts: vec![
					expr_cmpt_string("a"),
					expr_cmpt_ident("b", vec![ExprOp::DirName]),
					expr_cmpt_string("c"),
				],
				..EMPTY_EXPR
			}),
		]);

		self::check_cases_fail::<Expr>([
			(r#""abc"#, r#"Expected closing `"` after `"`"#),
			(r#""{a.unknown}""#, r#"Unknown expression operator: "unknown""#),
		]);
	}
}
