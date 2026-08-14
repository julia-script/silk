/** Focused Silk sources for the maintained standard-library documentation policy. */

export const complete = `//! Models recoverable work.
//!
//! # When to use
//! Use this module when a caller must preserve a typed failure as data.
//!
//! # See also
//! - [\`Problem\`] describes the failure payload.

/// A recoverable problem with a stable code.
pub struct Problem {
  /// Identifies the problem within this module.
  pub code: i32
}

/// Recovers a problem into its stable code.
///
/// # Details
/// The callback observes the same problem value that the caller supplies.
///
/// # Examples
/// ## Read one problem code
/// \`\`\`silk
/// pub fn main() -> i32 { return 7 }
/// \`\`\`
///
/// # See also
/// - [\`Problem\`] carries the code.
pub fn recover(
  /// The owned failure payload consumed by this recovery attempt.
  problem: Problem,
) -> i32 { return problem.code }
`

export const omittedSections = `//! Small values that need no extended template.

/// Returns the value unchanged.
pub fn identity(value: i32) -> i32 { return value }
`

export const missingCoverage = `fn helper() -> i32 { return 0 }

pub struct Problem {
  pub code: i32
}

pub service Recovery {
  effect fn recover(problem: Problem) -> i32 ? &mut Recovery
}
`

export const malformedShape = `//! First summary paragraph.
//!
//! A second summary paragraph is not a section.
//!
//! # Gotchas
//! Concrete caveat.
//!
//! # Details
//! This section is out of order.
//!
//! # Extra
//! Ad hoc section.

/// Recovers a problem.
///
/// @param problem This belongs on the parameter.
///
/// # Examples
/// \`\`\`silk ignore
/// pub fn main() -> i32 { return 0 }
/// \`\`\`
/// ## Repeated title
/// \`\`\`silk,unknown
/// pub fn main() -> i32 { return 0 }
/// \`\`\`
/// ## Repeated title
/// \`\`\`silk
/// pub fn main() -> i32 { return 0 }
/// \`\`\`
pub fn recover(problem: i32) -> i32 { return problem }
`

export const unresolvedLink = `//! Uses [\`Missing\`] values.

/// Returns a value related to [\`Missing\`].
pub fn value() -> i32 { return 1 }
`
