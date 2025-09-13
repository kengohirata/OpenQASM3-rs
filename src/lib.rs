//! # OpenQASM 3.0 Rust Interface
//!
//! This crate provides a comprehensive Rust interface for OpenQASM 3.0, including:
//! - Abstract Syntax Tree (AST) data structures
//! - Parser for converting OpenQASM 3.0 source code to AST
//! - Printer for converting AST back to OpenQASM 3.0 source code
//!
//! ## Example
//!
//! ```rust
//! use openqasm3::{parse, Program};
//!
//! // Parse OpenQASM 3.0 code
//! let code = r#"
//! OPENQASM 3.0;
//! qubit q[2];
//! h q[0];
//! cnot q[0], q[1];
//! "#;
//!
//! let program = parse(code).unwrap();
//! println!("{}", program); // Print back to OpenQASM
//! ```

pub mod ast;
pub mod parser;
pub mod printer;
pub mod error;

pub use ast::Program;
pub use parser::parse;
pub use error::Error;

/// Result type used throughout this crate
pub type Result<T> = std::result::Result<T, Error>;
