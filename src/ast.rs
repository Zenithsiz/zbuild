//! Ast

// Imports
use {itertools::Itertools, serde::de::Error, std::collections::HashMap};

/// Zbuild ast
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Ast<'a> {
	/// Aliases
	#[serde(rename = "alias")]
	#[serde(default)]
	#[serde(borrow)]
	pub aliases: HashMap<&'a str, Expr<'a>>,

	/// Default target
	#[serde(default)]
	#[serde(borrow)]
	pub default: Vec<Target<'a>>,

	/// Rules
	#[serde(borrow)]
	pub rules: HashMap<&'a str, Rule<'a>>,
}

/// Output Item
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum OutItem<'a> {
	/// File
	File(#[serde(borrow)] Expr<'a>),

	/// Dependencies file
	DepsFile {
		/// The dependency file's path
		#[serde(borrow)]
		deps_file: Expr<'a>,
	},
}

/// Dependency Item
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum DepItem<'a> {
	/// File
	File(#[serde(borrow)] Expr<'a>),

	/// Rule
	Rule {
		/// Rule name
		#[serde(borrow)]
		rule: Expr<'a>,

		/// All patterns to expand the rule with
		#[serde(default)]
		#[serde(borrow)]
		pats: HashMap<Expr<'a>, Expr<'a>>,
	},

	/// Dependencies file
	DepsFile {
		/// The dependency file's path
		#[serde(borrow)]
		deps_file: Expr<'a>,
	},

	// TODO: Implement these some other way? We get a quadratic
	//       blowup if we do them separately like this
	/// Static dependency
	Static {
		/// Inner item
		#[serde(rename = "static")]
		#[serde(borrow)]
		item: StaticDepItem<'a>,
	},

	/// Optional dependency
	Opt {
		/// Inner item
		#[serde(rename = "opt")]
		#[serde(borrow)]
		item: OptDepItem<'a>,
	},
}

/// Static Dependency Item
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum StaticDepItem<'a> {
	/// File
	File(#[serde(borrow)] Expr<'a>),

	/// Dependencies file
	DepsFile {
		/// The dependency file's path
		#[serde(borrow)]
		deps_file: Expr<'a>,
	},
}

/// Optional Dependency Item
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum OptDepItem<'a> {
	/// File
	File(#[serde(borrow)] Expr<'a>),

	/// Dependencies file
	DepsFile {
		/// The dependency file's path
		#[serde(borrow)]
		deps_file: Expr<'a>,
	},
}

/// Target
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum Target<'a> {
	/// File
	File(#[serde(borrow)] Expr<'a>),

	/// Rule
	Rule {
		/// Rule name
		#[serde(borrow)]
		rule: Expr<'a>,
	},
}

/// Expression
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct Expr<'a> {
	/// Components
	pub cmpts: Vec<ExprCmpt<'a>>,
}

/// Expression component
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum ExprCmpt<'a> {
	/// String
	String(&'a str),

	/// Pattern
	Pattern(Pattern<'a>),

	/// Alias
	Alias(Alias<'a>),
}

/// Pattern
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
/// Pattern
pub struct Pattern<'a> {
	/// Pattern name
	pub name: &'a str,

	/// Pattern operators
	pub ops: Vec<PatternOp>,
}

/// Pattern operator
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum PatternOp {
	/// Non-empty
	NonEmpty,
}

/// Alias
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
/// Alias
pub struct Alias<'a> {
	/// Alias name
	pub name: &'a str,

	/// Alias operators
	pub ops: Vec<AliasOp>,
}

/// Alias operator
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum AliasOp {
	/// Directory name
	DirName,
}

impl serde::Serialize for Expr<'_> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		// TODO: Less allocations
		self.cmpts
			.iter()
			.map(|cmpt| match *cmpt {
				ExprCmpt::String(s) => s.to_owned(),
				ExprCmpt::Pattern(Pattern { name, ref ops }) => format!(
					"^({name}{})",
					ops.iter()
						.map(|op| format!("::{}", match op {
							PatternOp::NonEmpty => "non_empty",
						}))
						.join("")
				),
				ExprCmpt::Alias(Alias { name, ref ops }) => format!(
					"$({name}{})",
					ops.iter()
						.map(|op| format!("::{}", match op {
							AliasOp::DirName => "dir_name",
						}))
						.join("")
				),
			})
			.join("")
			.serialize(serializer)
	}
}

impl<'a, 'de: 'a> serde::Deserialize<'de> for Expr<'a> {
	#[expect(clippy::indexing_slicing, clippy::string_slice)] // We verify the indexes are correct
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: serde::Deserializer<'de>,
	{
		// Parse the string
		let expr_str = <&'a str>::deserialize(deserializer)?;

		// Then parse all components
		let mut cmpts = vec![];
		let mut rest = expr_str;
		loop {
			// Try to find the next pattern / alias
			match rest.find(['$', '^']) {
				// If we found it
				Some(idx) => {
					// Add the string until the pattern / alias, if it isn't empty
					if !rest[..idx].is_empty() {
						cmpts.push(ExprCmpt::String(&rest[..idx]));
					}

					// Then check if it was an alias or pattern
					#[expect(clippy::missing_docs_in_private_items)]
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

					// Finally check what it was originally and parse all operations
					let cmpt = match kind {
						Kind::Alias => ExprCmpt::Alias(Alias {
							name,
							ops: ops
								.into_iter()
								.map(|op| match op.trim() {
									"dir_name" => Ok(AliasOp::DirName),
									op => Err(D::Error::custom(format!("Unknown alias operator {op:?}"))),
								})
								.collect::<Result<_, _>>()?,
						}),
						Kind::Pattern => ExprCmpt::Pattern(Pattern {
							name,
							ops: ops
								.into_iter()
								.map(|op| match op.trim() {
									"non_empty" => Ok(PatternOp::NonEmpty),
									op => Err(D::Error::custom(format!("Unknown pattern operator {op:?}"))),
								})
								.collect::<Result<_, _>>()?,
						}),
					};
					cmpts.push(cmpt);
				},

				// If we didn't find any, the rest of the expression if a string
				None => {
					// Add the rest only if it isn't empty
					if !rest.is_empty() {
						cmpts.push(ExprCmpt::String(rest));
					}
					break;
				},
			}
		}

		Ok(Self { cmpts })
	}
}

/// Rule
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Rule<'a> {
	/// Aliases
	#[serde(rename = "alias")]
	#[serde(default)]
	#[serde(borrow)]
	pub aliases: HashMap<&'a str, Expr<'a>>,

	/// Output items
	#[serde(default)]
	#[serde(borrow)]
	pub out: Vec<OutItem<'a>>,

	/// Dependencies
	#[serde(default)]
	#[serde(borrow)]
	pub deps: Vec<DepItem<'a>>,

	/// Execution
	#[serde(default)]
	#[serde(borrow)]
	pub exec: Exec<'a>,
}

/// Execution
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum Exec<'a> {
	/// Only commands
	OnlyCmds(Vec<Command<'a>>),

	/// Full
	Full {
		/// working directory
		#[serde(default)]
		#[serde(borrow)]
		cwd: Option<Expr<'a>>,

		/// Commands
		cmds: Vec<Command<'a>>,
	},
}

impl Default for Exec<'_> {
	fn default() -> Self {
		Self::OnlyCmds(vec![])
	}
}


/// Command
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct Command<'a> {
	/// All arguments
	#[serde(borrow)]
	pub args: Vec<Expr<'a>>,
}
