//! Error types for OpenQASM 3.0 parsing and processing

use thiserror::Error;

/// Error type for OpenQASM 3.0 operations
#[derive(Error, Debug, Clone, PartialEq)]
pub enum Error {
    /// Parse error with location information
    #[error("Parse error at line {line}, column {column}: {message}")]
    ParseError {
        line: usize,
        column: usize,
        message: String,
    },
    
    /// Semantic error (valid syntax but invalid semantics)
    #[error("Semantic error: {0}")]
    SemanticError(String),
    
    /// Type error
    #[error("Type error: {0}")]
    TypeError(String),
    
    /// Undefined identifier
    #[error("Undefined identifier: {0}")]
    UndefinedIdentifier(String),
    
    /// Invalid operation
    #[error("Invalid operation: {0}")]
    InvalidOperation(String),
}

impl Error {
    /// Create a new parse error
    pub fn parse_error(line: usize, column: usize, message: impl Into<String>) -> Self {
        Self::ParseError {
            line,
            column,
            message: message.into(),
        }
    }
    
    /// Create a new semantic error
    pub fn semantic_error(message: impl Into<String>) -> Self {
        Self::SemanticError(message.into())
    }
    
    /// Create a new type error
    pub fn type_error(message: impl Into<String>) -> Self {
        Self::TypeError(message.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let error = Error::parse_error(1, 5, "unexpected token");
        assert_eq!(error.to_string(), "Parse error at line 1, column 5: unexpected token");
        
        let error = Error::semantic_error("type mismatch");
        assert_eq!(error.to_string(), "Semantic error: type mismatch");
    }
}