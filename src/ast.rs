//! Ast

// Imports
use {serde::de::Error, std::collections::HashMap};

/// Ast
#[derive(Clone, Debug)]
#[derive(serde::Deserialize)]
pub struct Ast {
	/// Aliases
	#[serde(rename = "alias")]
	pub aliases: HashMap<String, Expr>,

	/// Default target
	pub default: Vec<Target>,

	/// Rules
	pub rules: HashMap<String, Rule>,
}

/// Item
#[derive(Clone, Debug)]
#[derive(serde::Deserialize)]
#[serde(untagged)]
pub enum Item {
	/// File
	File(Expr),

	/// Dependencies file
	DepsFile { deps_file: Expr },
}

/// Target
#[derive(Clone, Debug)]
#[derive(serde::Deserialize)]
#[serde(untagged)]
pub enum Target {
	/// File
	File(Expr),

	/// Rule
	Rule { rule: Expr },
}

/// Expression
#[derive(Clone, Debug)]
pub struct Expr {
	/// Components
	pub cmpts: Vec<ExprCmpt>,
}

/// Expression component
#[derive(Clone, Debug)]
pub enum ExprCmpt {
	/// String
	String(String),

	/// Pattern
	Pattern { name: String, ops: Vec<PatternOp> },

	/// Alias
	Alias { name: String, ops: Vec<AliasOp> },
}

/// Pattern operator
#[derive(Clone, Debug)]
pub enum PatternOp {
	/// Non-empty
	NonEmpty,
}

/// Alias operator
#[derive(Clone, Debug)]
pub enum AliasOp {
	/// Directory name
	DirName,
}

impl<'de> serde::Deserialize<'de> for Expr {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: serde::Deserializer<'de>,
	{
		// Parse the string
		// TODO: Allow arrays and concat them?
		// TODO: Deserialize a `Cow<str>`?
		let inner = String::deserialize(deserializer)?;

		// Then parse all components
		let mut cmpts = vec![];
		let mut rest = inner.as_str();
		loop {
			// Try to find the next pattern / alias
			match rest.find(['$', '^']) {
				// If we found it
				Some(idx) => {
					// Add the string until the pattern / alias, if it isn't empty
					if !rest[..idx].is_empty() {
						cmpts.push(ExprCmpt::String(rest[..idx].to_owned()));
					}

					// Then check if it was an alias or pattern
					enum Kind {
						Alias,
						Pattern,
					}
					let mut chars = rest[idx..].chars();
					let kind = match chars.next() {
						Some('$') => Kind::Alias,
						Some('^') => Kind::Pattern,
						_ => unreachable!(),
					};

					// Ensure it starts with `(`
					match chars.next() {
						Some('(') => (),
						Some(ch) => return Err(D::Error::custom(format!("Expected `(` after `$`, found {ch:?}"))),
						None => return Err(D::Error::custom("Expected `(` after `$`")),
					};

					// Then read until `)`
					rest = chars.as_str();
					let inner;
					(inner, rest) = rest
						.split_once(')')
						.ok_or_else(|| D::Error::custom("Alias `$(` has no closing brace"))?;

					// And split all operations
					let (name, ops) = match inner.split_once("::") {
						Some((name, ops)) => (name, ops.split("::").collect()),
						None => (inner, vec![]),
					};


					let cmpt = match kind {
						Kind::Alias => ExprCmpt::Alias {
							name: name.to_owned(),
							ops:  ops
								.into_iter()
								.map(|op| match op.trim() {
									"dir_name" => Ok(AliasOp::DirName),
									op => Err(D::Error::custom(format!("Unknown alias operator {op:?}"))),
								})
								.collect::<Result<_, _>>()?,
						},
						Kind::Pattern => ExprCmpt::Pattern {
							name: name.to_owned(),
							ops:  ops
								.into_iter()
								.map(|op| match op.trim() {
									"non_empty" => Ok(PatternOp::NonEmpty),
									op => Err(D::Error::custom(format!("Unknown pattern operator {op:?}"))),
								})
								.collect::<Result<_, _>>()?,
						},
					};
					cmpts.push(cmpt);
				},

				// If we didn't find any, the rest of the expression if a string
				None => {
					// Add the rest only if it isn't empty
					if !rest.is_empty() {
						cmpts.push(ExprCmpt::String(rest.to_owned()))
					}
					break;
				},
			}
		}

		Ok(Expr { cmpts })
	}
}

/// Rule
#[derive(Clone, Debug)]
#[derive(serde::Deserialize)]
pub struct Rule {
	/// Aliases
	#[serde(rename = "alias")]
	#[serde(default)]
	pub aliases: HashMap<String, Expr>,

	/// Output items
	#[serde(rename = "out")]
	#[serde(default)]
	pub output: Vec<Item>,

	/// Dependencies
	#[serde(default)]
	pub deps: Vec<Item>,

	/// Static dependencies
	#[serde(default)]
	pub static_deps: Vec<Item>,

	/// Execution working directory
	#[serde(default)]
	pub exec_cwd: Option<Expr>,

	/// Execution
	pub exec: Vec<Command>,
}

/// Command
#[derive(Clone, Debug)]
#[derive(serde::Deserialize)]
#[serde(transparent)]
pub struct Command {
	/// All arguments
	pub args: Vec<Expr>,
}
