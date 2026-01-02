//! Error reporting module for WASD.
//!
//! Uses ariadne for pretty error messages.

mod diagnostic;

pub use diagnostic::{report_error, Diagnostic};
