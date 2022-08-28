//! Expression matching

// Imports
use {crate::rules::ExprCmpt, std::collections::HashMap};

/// Returns if `value` matches all `cmpts` and returns all patterns resolved
///
/// Returns error if any aliases are found, or if more than 1 pattern is found.
pub fn match_expr(mut cmpts: &[ExprCmpt], mut value: &str) -> Result<Option<HashMap<String, String>>, anyhow::Error> {
	let mut patterns = HashMap::new();

	// Until `rhs` has anything to match left
	loop {
		match cmpts {
			// If we start with a string, strip the prefix off
			[ExprCmpt::String(lhs), rest @ ..] => match value.strip_prefix(lhs) {
				Some(new_rhs) => {
					cmpts = rest;
					value = new_rhs;
				},
				None => return Ok(None),
			},

			// If we end with a string, strip the suffix off
			[rest @ .., ExprCmpt::String(lhs)] => match value.strip_suffix(lhs) {
				Some(new_rhs) => {
					cmpts = rest;
					value = new_rhs;
				},
				None => return Ok(None),
			},

			// If we're a single pattern, we fully match anything on the right
			[ExprCmpt::Pattern(pat)] => {
				patterns.insert(pat.name.clone(), value.to_owned());
				cmpts = &[];
				value = "";
			},

			// If we have patterns on both sides, reject
			[ExprCmpt::Pattern(lhs), .., ExprCmpt::Pattern(rhs)] =>
				anyhow::bail!("Expression had more than 2 patterns: {lhs:?} and {rhs:?}"),
			// If we have aliases on any side, reject
			[ExprCmpt::Alias(alias), ..] | [.., ExprCmpt::Alias(alias)] =>
				anyhow::bail!("Cannot match unexpanded alias: {alias:?}"),

			// If we're empty, we match an empty string
			[] => match value {
				"" => return Ok(Some(patterns)),
				_ => return Ok(None),
			},
		}
	}
}
