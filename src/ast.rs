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
pub enum Expr {
	/// Operation
	Op {
		/// Operation
		op: ExprOp,

		/// Expr
		expr: Box<Self>,
	},

	/// String
	String(Vec<ExprCmpt>),
}

/// Expression operator
#[derive(Clone, Debug)]
pub enum ExprOp {
	/// Dir name
	DirName,
}

/// Expression component
#[derive(Clone, Debug)]
pub enum ExprCmpt {
	/// String
	String(String),

	/// Pattern
	Pattern(String),

	/// Alias
	Alias(String),
}

impl<'de> serde::Deserialize<'de> for Expr {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: serde::Deserializer<'de>,
	{
		struct Visitor;

		impl<'de> serde::de::Visitor<'de> for Visitor {
			type Value = Expr;

			fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
				write!(formatter, "Map with single key for operation or string")
			}

			fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
			where
				A: serde::de::MapAccess<'de>,
			{
				// Get the entry
				let (op, expr) = map
					.next_entry::<String, Expr>()?
					.ok_or_else(|| A::Error::custom("Unexpected empty map"))?;

				// Make sure there isn't another one
				if let Some((key, _)) = map.next_entry::<String, Expr>()? {
					return Err(A::Error::custom(format!("Unexpected second map entry: {key:?}")));
				}

				// Then parse the operation
				let op = match op.as_str() {
					"dir_name" => ExprOp::DirName,
					_ => return Err(A::Error::custom(format!("Unknown expression operation: {op:?}"))),
				};

				Ok(Expr::Op {
					op,
					expr: Box::new(expr),
				})
			}

			fn visit_str<E>(self, mut s: &str) -> Result<Self::Value, E>
			where
				E: serde::de::Error,
			{
				let mut components = vec![];
				loop {
					// Try to find the next pattern / alias
					match s.find(['$', '^']) {
						// If we found it
						Some(idx) => {
							// Add the string until it, if it isn't empty
							if !s[..idx].is_empty() {
								components.push(ExprCmpt::String(s[..idx].to_owned()));
							}

							// Then check which one we got
							let mut chars = s[idx..].chars();
							let cmpt_fn = match chars.next() {
								Some('$') => ExprCmpt::Alias,
								Some('^') => ExprCmpt::Pattern,
								_ => unreachable!(),
							};

							// Make sure it's valid
							match chars.next() {
								Some('(') => (),
								Some(ch) => return Err(E::custom(format!("Expected `(` after `$`, found {ch:?}"))),
								None => return Err(E::custom("Expected `(` after `$`")),
							};

							// Then read until `)`
							s = chars.as_str();
							let alias;
							(alias, s) = s
								.split_once(')')
								.ok_or_else(|| E::custom("Alias `$(` has no closing brace"))?;

							components.push(cmpt_fn(alias.to_owned()));
						},

						// If we didn't find any, the rest of the expression if a string
						None => {
							// Add the rest only if it isn't empty
							if !s.is_empty() {
								components.push(ExprCmpt::String(s.to_owned()))
							}
							break;
						},
					}
				}

				Ok(Expr::String(components))
			}
		}

		// TODO: Not use `_any`?
		deserializer.deserialize_any(Visitor)
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
