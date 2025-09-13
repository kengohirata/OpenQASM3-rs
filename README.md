# OpenQASM3.0-rs

A comprehensive Rust library for parsing, manipulating, and generating OpenQASM 3.0 quantum programs.

## Features

- **Complete AST**: Comprehensive Abstract Syntax Tree representation of OpenQASM 3.0
- **Parser**: Robust parser that converts OpenQASM 3.0 source code to AST
- **Printer**: Pretty-printer that converts AST back to valid OpenQASM 3.0 code
- **Round-trip consistency**: Parse → Print → Parse produces identical results
- **Comprehensive testing**: Unit tests, integration tests, and property-based tests
- **Error handling**: Detailed error messages with location information

## Supported OpenQASM 3.0 Features

- Version declarations (`OPENQASM 3.0;`)
- Include statements (`include "stdgates.inc";`)
- Quantum register declarations (`qubit q[2];`)
- Classical register declarations (`bit c[2]; int[32] counter;`)
- Gate definitions with parameters and control flow
- Function definitions with return types
- Gate applications with modifiers (control, inverse, power)
- Measurements (`c = measure q;`)
- Classical assignments and expressions
- Control flow (if/else, for, while, switch)
- Barrier and reset operations
- Comprehensive expression support

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
openqasm3 = "0.1.0"
```

### Basic Example

```rust
use openqasm3::{parse, ast::*};

// Parse OpenQASM 3.0 code
let code = r#"
OPENQASM 3.0;
include "stdgates.inc";

qubit q[2];
bit c[2];

h q[0];
cnot q[0], q[1];
c = measure q;
"#;

let program = parse(code).unwrap();
println!("Parsed {} statements", program.statements.len());

// Print back to OpenQASM
println!("{}", program);
```

### Building AST Programmatically

```rust
use openqasm3::ast::*;

let mut program = Program::new(Version::openqasm_3_0());

// Add qubit declaration
program.add_statement(Statement::QuantumDeclaration(QuantumDeclaration {
    name: "q".to_string(),
    size: Some(Expression::Integer(2)),
}));

// Add H gate
program.add_statement(Statement::GateApplication(GateApplication {
    name: "h".to_string(),
    parameters: vec![],
    qubits: vec![QubitRef::Indexed("q".to_string(), Box::new(Expression::Integer(0)))],
    modifiers: vec![],
}));

println!("{}", program);
```

## Testing

Run the test suite:

```bash
# Run all tests
cargo test

# Run integration tests
cargo test --test integration_tests

# Run example file tests
cargo test --test example_files

# Run with example
cargo run --example basic_usage
```

### Test Coverage

- **Unit tests**: Each module has comprehensive unit tests
- **Integration tests**: End-to-end parsing and printing tests
- **Property-based tests**: Round-trip consistency testing
- **Example files**: Real OpenQASM programs tested for correctness

## Examples

The `examples/` directory contains several OpenQASM 3.0 programs:

- `bell_state.qasm`: Bell state preparation
- `grover.qasm`: Grover's algorithm implementation
- `conditional.qasm`: Conditional quantum operations

## Architecture

The library is structured in four main modules:

- `ast`: Abstract Syntax Tree definitions
- `parser`: OpenQASM 3.0 parser using nom
- `printer`: AST to OpenQASM code formatter
- `error`: Comprehensive error types

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
