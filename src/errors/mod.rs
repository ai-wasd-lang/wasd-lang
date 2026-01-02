//! Error reporting module for Lux.
//!
//! Uses ariadne for pretty error messages.

mod diagnostic;

pub use diagnostic::{report_error, Diagnostic, DiagnosticKind};
