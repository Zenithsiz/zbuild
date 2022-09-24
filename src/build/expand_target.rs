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
	let target = match *target {
		Target::File { ref file, is_static } => Target::File {
			file: self::expand_expr_string(file, &mut global_expr_visitor).map_err(AppError::expand_expr(file))?,
			is_static,
		},

		Target::Rule { ref rule, ref pats } => Target::Rule {
			rule: self::expand_expr_string(rule, &mut global_expr_visitor).map_err(AppError::expand_expr(rule))?,
			pats: pats
				.iter()
				.map(|(pat, expr)| {
					Ok((
						pat.clone(),
						self::expand_expr_string(expr, &mut global_expr_visitor)
							.map_err(AppError::expand_expr(expr))?,
					))
				})
				.collect::<Result<_, AppError>>()?,
		},
	};

	Ok(target)
}
