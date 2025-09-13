//! Integration tests for OpenQASM 3.0 parser and printer
//!
//! These tests verify the end-to-end functionality of parsing OpenQASM code,
//! manipulating the AST, and printing it back to valid OpenQASM code.

use openqasm3::{parse, ast::*};

#[test]
fn test_basic_quantum_program() {
    let code = r#"
OPENQASM 3.0;
qubit q[2];
h q[0];
cnot q[0], q[1];
    "#;

    let program = parse(code).expect("Failed to parse basic quantum program");
    
    // Verify structure
    assert_eq!(program.version.major, 3);
    assert_eq!(program.version.minor, 0);
    assert_eq!(program.statements.len(), 3); // qubit declaration, h gate, cnot gate
    
    // Verify the qubit declaration
    match &program.statements[0] {
        Statement::QuantumDeclaration(decl) => {
            assert_eq!(decl.name, "q");
            match &decl.size {
                Some(Expression::Integer(2)) => {},
                _ => panic!("Expected qubit size of 2"),
            }
        },
        _ => panic!("Expected quantum declaration"),
    }
    
    // Verify H gate
    match &program.statements[1] {
        Statement::GateApplication(gate) => {
            assert_eq!(gate.name, "h");
            assert_eq!(gate.qubits.len(), 1);
        },
        _ => panic!("Expected gate application"),
    }
    
    // Test round-trip: parse -> print -> parse should be equivalent
    let printed = format!("{}", program);
    let reparsed = parse(&printed).expect("Failed to reparse printed code");
    assert_eq!(program.statements.len(), reparsed.statements.len());
}

#[test]
fn test_classical_operations() {
    let code = r#"
OPENQASM 3.0;
bit c[8];
int[32] counter = 0;
bool flag = true;
    "#;

    let program = parse(code).expect("Failed to parse classical operations");
    assert_eq!(program.statements.len(), 3);
    
    // Verify bit declaration
    match &program.statements[0] {
        Statement::ClassicalDeclaration(decl) => {
            assert_eq!(decl.name, "c");
            assert_eq!(decl.typ, ClassicalType::Bit);
            match &decl.size {
                Some(Expression::Integer(8)) => {},
                _ => panic!("Expected bit array size of 8"),
            }
        },
        _ => panic!("Expected classical declaration"),
    }
    
    // Verify int declaration with initial value
    match &program.statements[1] {
        Statement::ClassicalDeclaration(decl) => {
            assert_eq!(decl.name, "counter");
            assert_eq!(decl.typ, ClassicalType::Int(Some(32)));
            match &decl.initial_value {
                Some(Expression::Integer(0)) => {},
                _ => panic!("Expected initial value of 0"),
            }
        },
        _ => panic!("Expected classical declaration"),
    }
}

#[test]
fn test_controlled_gates() {
    let code = r#"
OPENQASM 3.0;
qubit q[3];
ctrl(q[0]) x q[1];
ctrl(q[0], q[1]) x q[2];
    "#;

    let program = parse(code).expect("Failed to parse controlled gates");
    
    // Verify single control
    match &program.statements[1] {
        Statement::GateApplication(gate) => {
            assert_eq!(gate.name, "x");
            assert_eq!(gate.modifiers.len(), 1);
            match &gate.modifiers[0] {
                GateModifier::Control(qubits) => {
                    assert_eq!(qubits.len(), 1);
                },
                _ => panic!("Expected control modifier"),
            }
        },
        _ => panic!("Expected gate application"),
    }
    
    // Test round-trip
    let printed = format!("{}", program);
    let _reparsed = parse(&printed).expect("Failed to reparse controlled gates");
}

#[test]
fn test_gate_definitions() {
    let code = r#"
OPENQASM 3.0;
gate my_gate(theta) q0, q1 {
    rx(theta) q0;
    cnot q0, q1;
}
    "#;

    let program = parse(code).expect("Failed to parse gate definition");
    
    match &program.statements[0] {
        Statement::GateDefinition(gate_def) => {
            assert_eq!(gate_def.name, "my_gate");
            assert_eq!(gate_def.parameters.len(), 1);
            assert_eq!(gate_def.parameters[0], "theta");
            assert_eq!(gate_def.qubits.len(), 2);
            assert_eq!(gate_def.qubits[0], "q0");
            assert_eq!(gate_def.qubits[1], "q1");
            assert_eq!(gate_def.body.len(), 2);
        },
        _ => panic!("Expected gate definition"),
    }
}

#[test]
fn test_measurements() {
    let code = r#"
OPENQASM 3.0;
qubit q[2];
bit c[2];
c = measure q;
    "#;

    let program = parse(code).expect("Failed to parse measurement");
    
    match &program.statements[2] {
        Statement::Measurement(meas) => {
            assert_eq!(meas.target, Some("c".to_string()));
            assert_eq!(meas.qubits.len(), 1);
        },
        _ => panic!("Expected measurement"),
    }
}

#[test]
fn test_includes() {
    let code = r#"
OPENQASM 3.0;
include "stdgates.inc";
qubit q;
h q;
    "#;

    let program = parse(code).expect("Failed to parse includes");
    assert_eq!(program.includes.len(), 1);
    assert_eq!(program.includes[0].filename, "stdgates.inc");
}

#[test]
fn test_control_flow() {
    let code = r#"
OPENQASM 3.0;
qubit q[2];
bit c[2];
if (c[0]) {
    x q[1];
}
    "#;

    let program = parse(code).expect("Failed to parse control flow");
    
    match &program.statements[2] {
        Statement::ControlFlow(ControlFlow::If { condition, then_body, else_body }) => {
            // Verify condition is an index expression
            match condition {
                Expression::Index(_, _) => {},
                _ => panic!("Expected index expression in condition"),
            }
            assert_eq!(then_body.len(), 1);
            assert!(else_body.is_none());
        },
        _ => panic!("Expected if statement"),
    }
}

#[test]
fn test_reset_and_barrier() {
    let code = r#"
OPENQASM 3.0;
qubit q[2];
reset q[0];
barrier q[0], q[1];
    "#;

    let program = parse(code).expect("Failed to parse reset and barrier");
    
    // Verify reset - should be the second statement (after qubit declaration)
    match &program.statements[1] {
        Statement::Reset(reset) => {
            match &reset.target {
                QubitRef::Indexed(name, _) => {
                    assert_eq!(name, "q");
                },
                _ => panic!("Expected indexed qubit reference"),
            }
        },
        _ => panic!("Expected reset statement"),
    }
    
    // Verify barrier
    match &program.statements[2] {
        Statement::Barrier(barrier) => {
            assert_eq!(barrier.qubits.len(), 2);
        },
        _ => panic!("Expected barrier statement"),
    }
}

#[test]
fn test_complex_program() {
    let code = r#"
OPENQASM 3.0;
include "stdgates.inc";

qubit q[3];
bit c[3];

h q[0];
cnot q[0], q[1];

h q[2];

c = measure q;
    "#;

    let program = parse(code).expect("Failed to parse complex program");
    
    // Should have include + 6 statements (qubit, bit, h, cnot, h, measure)
    assert_eq!(program.includes.len(), 1);
    assert_eq!(program.statements.len(), 6);
    
    // Test round-trip
    let printed = format!("{}", program);
    let reparsed = parse(&printed).expect("Failed to reparse complex program");
    assert_eq!(program.statements.len(), reparsed.statements.len());
    assert_eq!(program.includes.len(), reparsed.includes.len());
}

#[test]
fn test_error_handling() {
    // Test various error conditions
    
    // Invalid version
    let invalid_version = "OPENQASM 2.0;";
    let result = parse(invalid_version);
    // Should still parse but with version 2.0
    assert!(result.is_ok());
    
    // Completely invalid syntax
    let invalid_syntax = "this is not openqasm code";
    let result = parse(invalid_syntax);
    assert!(result.is_err());
    
    // Missing semicolon (should fail)
    let missing_semicolon = r#"
OPENQASM 3.0;
qubit q[2]
h q[0];
    "#;
    let result = parse(missing_semicolon);
    assert!(result.is_err());
}

#[test]
fn test_expression_parsing() {
    let code = r#"
OPENQASM 3.0;
int[32] a = 5 + 3 * 2;
bool b = (a > 10) && true;
    "#;

    let program = parse(code).expect("Failed to parse expressions");
    
    // Verify arithmetic expression
    match &program.statements[0] {
        Statement::ClassicalDeclaration(decl) => {
            match &decl.initial_value {
                Some(Expression::Binary { .. }) => {},
                _ => panic!("Expected binary expression"),
            }
        },
        _ => panic!("Expected classical declaration"),
    }
    
    // Verify boolean expression
    match &program.statements[1] {
        Statement::ClassicalDeclaration(decl) => {
            match &decl.initial_value {
                Some(Expression::Binary { .. }) => {},
                _ => panic!("Expected binary expression"),
            }
        },
        _ => panic!("Expected classical declaration"),
    }
}

#[cfg(test)]
mod property_tests {
    use super::*;
    use proptest::prelude::*;
    
    // Property-based test for round-trip consistency
    proptest! {
        #[test]
        fn test_round_trip_simple_programs(
            qubit_name in "[a-z][a-z0-9]*",
            qubit_size in 1u32..10u32,
        ) {
            let code = format!(
                "OPENQASM 3.0;\nqubit {}[{}];\nh {}[0];",
                qubit_name, qubit_size, qubit_name
            );
            
            if let Ok(program) = parse(&code) {
                let printed = format!("{}", program);
                let reparsed = parse(&printed);
                prop_assert!(reparsed.is_ok());
                
                let reparsed_program = reparsed.unwrap();
                prop_assert_eq!(program.statements.len(), reparsed_program.statements.len());
            }
        }
    }
}