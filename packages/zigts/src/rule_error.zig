//! Explicit error set for all rule/review code paths.
//!
//! New subcommands (edit-simulate, describe-rule, search, review-patch) use
//! this error set instead of anyerror to make failure modes visible at the
//! call site. The runtime layer (zruntime, module_binding) intentionally
//! keeps anyerror for generic function-pointer signatures.

pub const RuleError = error{
    /// Rule name or code not found in the registry.
    InvalidRuleName,
    /// Input JSON was structurally valid but semantically wrong
    /// (e.g. missing required "file" field).
    InvalidInput,
    /// A checker returned an unexpected error during rule evaluation.
    RuleEvaluationFailed,
    /// Could not compute the before/after violation diff.
    DiffComputationFailed,
    /// --stdin-json was specified but stdin was empty or unreadable.
    StdinReadFailed,
    /// JSON on stdin failed to parse.
    JsonParseError,
};
