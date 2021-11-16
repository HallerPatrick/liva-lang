//! The parser for the liva source code
//!
//! ## Example
//! ```rust
//! use std::error::Error;
//!
//! use liva_parser::print_ast;
//!
//! fn main() -> Result<(), Box<dyn Error>> {
//!     print_ast("lib.lv");
//!     Ok(())
//! }
//!
//! ```
#[macro_use]
extern crate lazy_static;

mod parser;

pub use parser::*;

use std::error::Error;
use std::fs;

pub use parser::parse_source;
// pub use parser::parse_source_from_file;
pub use parser::statement::parse_statement;

/// Prints the AST of the parsed liva source code
pub fn print_ast(filename: &str) -> Result<(), Box<dyn Error>> {
    let source: String = fs::read_to_string(filename)?;
    println!("{:#?}", parse_source(Span::new(source.as_str())));
    Ok(())
}

#[test]
fn test_parser() -> Result<(), Box<dyn Error>> {
    let source: String = fs::read_to_string("examples/class.lv")?;
    let _ = parse_source(Span::new(source.as_str()));
    // println!("{:?}", res);
    Ok(())
}
