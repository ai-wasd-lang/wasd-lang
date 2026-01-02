//! Borrow checker module for WASD.
//!
//! Validates ownership and borrowing rules:
//! - Many `&T` OR one `&mut T`, never both
//! - No dangling references
//! - No use after move

mod checker;

pub use checker::BorrowChecker;
