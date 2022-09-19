//! Expression matching

// Imports
use {
	crate::{
		rules::{Expr, ExprCmpt, PatternOp},
		AppError,
	},
	std::collections::HashMap,
};

/// Returns if `value` matches all `cmpts` and returns all patterns resolved
///
/// Returns error if more than 1 pattern is found.
///
/// Panics if any aliases are found
pub fn match_expr(
	expr: &Expr,
	cmpts: &[ExprCmpt],
	mut value: &str,
) -> Result<Option<HashMap<String, String>>, AppError> {
	let mut patterns = HashMap::new();

	// Until `rhs` has anything to match left
	let mut cur_cmpts = cmpts;
	loop {
		match cur_cmpts {
			// If we start with a string, strip the prefix off
			[ExprCmpt::String(lhs), rest @ ..] => match value.strip_prefix(lhs) {
				Some(new_rhs) => {
					cur_cmpts = rest;
					value = new_rhs;
				},
				None => return Ok(None),
			},

			// If we end with a string, strip the suffix off
			[rest @ .., ExprCmpt::String(lhs)] => match value.strip_suffix(lhs) {
				Some(new_rhs) => {
					cur_cmpts = rest;
					value = new_rhs;
				},
				None => return Ok(None),
			},

			// If we're a single pattern, check for operators
			[ExprCmpt::Pattern(pat)] => {
				let mut ops = pat.ops.as_slice();
				loop {
					match ops {
						// If we're empty, match everything
						[] => break,

						// On non-empty check if the rest of the value is empty
						[PatternOp::NonEmpty, rest @ ..] => match value.is_empty() {
							// If so, we don't match anything
							true => return Ok(None),

							// Else continue checking the rest of the operators
							false => ops = rest,
						},
					}
				}

				// If we get here, match everything
				patterns.insert(pat.name.clone(), value.to_owned());
				cur_cmpts = &[];
				value = "";
			},

			// If we have patterns on both sides, reject
			[ExprCmpt::Pattern(_), .., ExprCmpt::Pattern(_)] =>
				return Err(AppError::MatchExprTooManyPats {
					expr_fmt:       expr.to_string(),
					expr_cmpts_fmt: cmpts.iter().map(ExprCmpt::to_string).collect(),
				}),
			// If we have aliases on any side, reject
			[ExprCmpt::Alias(alias), ..] | [.., ExprCmpt::Alias(alias)] =>
				unreachable!("Cannot match unexpanded alias: {alias:?}"),

			// If we're empty, we match an empty string
			[] => match value {
				"" => return Ok(Some(patterns)),
				_ => return Ok(None),
			},
		}
	}
}
