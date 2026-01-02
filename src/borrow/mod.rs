//! Borrow checker module for WASD.
//!
//! Validates ownership and borrowing rules:
//! - Many `&T` OR one `&mut T`, never both
//! - No dangling references
//! - No use after move

pub mod checker;
#[cfg(test)]
mod checker_tests;

pub use checker::BorrowChecker;
