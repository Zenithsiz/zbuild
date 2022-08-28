//! Expanding targets

// Imports
use {
	super::{expand_expr, expand_expr_string},
	crate::rules::{Expr, Target},
	anyhow::Context,
};

/// Expands a target
pub fn expand_target(
	target: &Target<Expr>,
	mut global_expr_visitor: expand_expr::GlobalVisitor,
) -> Result<Target<String>, anyhow::Error> {
	let target = match target {
		// If we got a file, check which rule can make it
		Target::File { file } => {
			// Expand the file
			let file = self::expand_expr_string(file, &mut global_expr_visitor)
				.with_context(|| format!("Unable to expand expression {file:?}"))?;

			Target::File { file }
		},

		Target::Rule { rule, pats } => {
			// Expand the rule
			let rule = self::expand_expr_string(rule, &mut global_expr_visitor)
				.with_context(|| format!("Unable to expand expression {rule:?}"))?;

			// Expand all patterns
			let pats = pats
				.iter()
				.map(|(pat, expr)| {
					let value = self::expand_expr_string(expr, &mut global_expr_visitor)
						.with_context(|| format!("Unable to expand expression {expr:?}"))?;

					Ok((pat.to_owned(), value))
				})
				.collect::<Result<_, anyhow::Error>>()?;

			Target::Rule { rule, pats }
		},
	};

	Ok(target)
}
