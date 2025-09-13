//! Basic usage examples for the OpenQASM 3.0 library

use openqasm3::{parse, ast::*};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Example 1: Parse a simple quantum program
    println!("=== Example 1: Parsing OpenQASM 3.0 ===");
    
    let qasm_code = r#"
OPENQASM 3.0;
include "stdgates.inc";

qubit q[2];
bit c[2];

h q[0];
cnot q[0], q[1];
c = measure q;
    "#;

    let program = parse(qasm_code)?;
    println!("Parsed program with {} statements", program.statements.len());
    println!("Version: {}", program.version);
    
    // Example 2: Programmatically build an AST
    println!("\n=== Example 2: Building AST programmatically ===");
    
    let mut new_program = Program::new(Version::openqasm_3_0());
    
    // Add qubit declaration
    new_program.add_statement(Statement::QuantumDeclaration(QuantumDeclaration {
        name: "q".to_string(),
        size: Some(Expression::Integer(3)),
    }));
    
    // Add bit declaration  
    new_program.add_statement(Statement::ClassicalDeclaration(ClassicalDeclaration {
        typ: ClassicalType::Bit,
        name: "c".to_string(),
        size: Some(Expression::Integer(3)),
        initial_value: None,
    }));
    
    // Add H gate to first qubit
    new_program.add_statement(Statement::GateApplication(GateApplication {
        name: "h".to_string(),
        parameters: vec![],
        qubits: vec![QubitRef::Indexed("q".to_string(), Box::new(Expression::Integer(0)))],
        modifiers: vec![],
    }));
    
    // Add CNOT gate (controlled-X)
    new_program.add_statement(Statement::GateApplication(GateApplication {
        name: "x".to_string(),
        parameters: vec![],
        qubits: vec![QubitRef::Indexed("q".to_string(), Box::new(Expression::Integer(2)))],
        modifiers: vec![GateModifier::Control(vec![
            QubitRef::Indexed("q".to_string(), Box::new(Expression::Integer(0)))
        ])],
    }));
    
    // Add measurement
    new_program.add_statement(Statement::Measurement(Measurement {
        qubits: vec![QubitRef::All("q".to_string())],
        target: Some("c".to_string()),
    }));
    
    println!("Built program programmatically:");
    println!("{}", new_program);
    
    // Example 3: Parse and modify existing program
    println!("\n=== Example 3: Modifying existing program ===");
    
    let mut modified_program = parse(qasm_code)?;
    
    // Add a barrier before measurement
    if let Some(last_pos) = modified_program.statements.len().checked_sub(1) {
        modified_program.statements.insert(last_pos, Statement::Barrier(Barrier {
            qubits: vec![QubitRef::All("q".to_string())],
        }));
    }
    
    println!("Modified program with barrier:");
    println!("{}", modified_program);
    
    // Example 4: Round-trip test
    println!("\n=== Example 4: Round-trip test ===");
    
    let printed = format!("{}", program);
    let reparsed = parse(&printed)?;
    
    println!("Original statements: {}", program.statements.len());
    println!("Reparsed statements: {}", reparsed.statements.len());
    println!("Round-trip successful: {}", program.statements.len() == reparsed.statements.len());
    
    // Example 5: Error handling
    println!("\n=== Example 5: Error handling ===");
    
    let invalid_qasm = "this is not valid openqasm code";
    match parse(invalid_qasm) {
        Ok(_) => println!("Unexpected success parsing invalid code"),
        Err(e) => println!("Expected error: {}", e),
    }
    
    Ok(())
}