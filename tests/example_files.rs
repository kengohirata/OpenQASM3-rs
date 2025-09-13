//! Tests that parse example OpenQASM files

use openqasm3::parse;
use std::fs;
use std::path::Path;

#[test]
fn test_bell_state_example() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/bell_state.qasm");
    let content = fs::read_to_string(path).expect("Failed to read bell_state.qasm");
    
    let program = parse(&content).expect("Failed to parse bell_state.qasm");
    
    // Should have include + 3 statements (qubit, bit, h, cnot, measure)
    assert_eq!(program.includes.len(), 1);
    assert!(program.statements.len() >= 4);
    
    // Test round-trip
    let printed = format!("{}", program);
    let _reparsed = parse(&printed).expect("Failed to reparse bell_state.qasm");
}

#[test]
fn test_grover_example() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/grover.qasm");
    let content = fs::read_to_string(path).expect("Failed to read grover.qasm");
    
    let program = parse(&content).expect("Failed to parse grover.qasm");
    
    // Should have include + gate definitions + quantum operations
    assert_eq!(program.includes.len(), 1);
    assert!(program.statements.len() >= 5);
    
    // Test round-trip
    let printed = format!("{}", program);
    let _reparsed = parse(&printed).expect("Failed to reparse grover.qasm");
}

#[test]
fn test_conditional_example() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/conditional.qasm");
    let content = fs::read_to_string(path).expect("Failed to read conditional.qasm");
    
    let program = parse(&content).expect("Failed to parse conditional.qasm");
    
    // Should have qubit/bit declarations + conditional logic
    assert!(program.statements.len() >= 3);
    
    // Check for conditional statement
    let has_conditional = program.statements.iter().any(|stmt| {
        matches!(stmt, openqasm3::ast::Statement::ControlFlow(_))
    });
    assert!(has_conditional, "Expected conditional statement in conditional.qasm");
    
    // Test round-trip
    let printed = format!("{}", program);
    let _reparsed = parse(&printed).expect("Failed to reparse conditional.qasm");
}