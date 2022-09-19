//! Expanding targets

// Imports
use {
	super::{expand_expr, expand_expr_string},
	crate::{
		rules::{Expr, Target},
		AppError,
	},
};

/// Expands a target expression
pub fn expand_target(
	target: &Target<Expr>,
	mut global_expr_visitor: expand_expr::GlobalVisitor,
) -> Result<Target<String>, AppError> {
	let target = match target {
		Target::File { file } => Target::File {
			file: self::expand_expr_string(file, &mut global_expr_visitor).map_err(AppError::expand_expr(file))?,
		},

		Target::Rule { rule, pats } => Target::Rule {
			rule: self::expand_expr_string(rule, &mut global_expr_visitor).map_err(AppError::expand_expr(rule))?,
			pats: pats
				.iter()
				.map(|(pat, expr)| {
					Ok((
						pat.to_owned(),
						self::expand_expr_string(expr, &mut global_expr_visitor)
							.map_err(AppError::expand_expr(expr))?,
					))
				})
				.collect::<Result<_, AppError>>()?,
		},
	};

	Ok(target)
}
