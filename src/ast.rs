//! Abstract Syntax Tree (AST) definitions for OpenQASM 3.0
//!
//! This module defines the complete AST structure for OpenQASM 3.0,
//! covering all language constructs including quantum operations,
//! classical operations, control flow, and declarations.



/// The root of an OpenQASM 3.0 program
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    /// Version declaration (e.g., "3.0")
    pub version: Version,
    /// List of include statements
    pub includes: Vec<Include>,
    /// List of statements in the program
    pub statements: Vec<Statement>,
}

impl Program {
    /// Create a new empty program with the specified version
    pub fn new(version: Version) -> Self {
        Self {
            version,
            includes: Vec::new(),
            statements: Vec::new(),
        }
    }
    
    /// Add a statement to the program
    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }
    
    /// Add an include to the program
    pub fn add_include(&mut self, include: Include) {
        self.includes.push(include);
    }
}

/// Version declaration
#[derive(Debug, Clone, PartialEq)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
}

impl Version {
    /// Create a new version
    pub fn new(major: u32, minor: u32) -> Self {
        Self { major, minor }
    }
    
    /// Create OpenQASM 3.0
    pub fn openqasm_3_0() -> Self {
        Self::new(3, 0)
    }
}

/// Include statement
#[derive(Debug, Clone, PartialEq)]
pub struct Include {
    pub filename: String,
}

/// A statement in OpenQASM 3.0
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    /// Quantum register declaration
    QuantumDeclaration(QuantumDeclaration),
    /// Classical register declaration  
    ClassicalDeclaration(ClassicalDeclaration),
    /// Gate definition
    GateDefinition(GateDefinition),
    /// Function definition
    FunctionDefinition(FunctionDefinition),
    /// Quantum gate application
    GateApplication(GateApplication),
    /// Measurement operation
    Measurement(Measurement),
    /// Assignment statement
    Assignment(Assignment),
    /// Expression statement
    Expression(Expression),
    /// Control flow statement
    ControlFlow(ControlFlow),
    /// Barrier statement
    Barrier(Barrier),
    /// Reset statement
    Reset(Reset),
}

/// Quantum register declaration
#[derive(Debug, Clone, PartialEq)]
pub struct QuantumDeclaration {
    pub name: String,
    pub size: Option<Expression>, // None for single qubit
}

/// Classical register declaration
#[derive(Debug, Clone, PartialEq)]
pub struct ClassicalDeclaration {
    pub typ: ClassicalType,
    pub name: String,
    pub size: Option<Expression>, // None for single value
    pub initial_value: Option<Expression>,
}

/// Classical types in OpenQASM 3.0
#[derive(Debug, Clone, PartialEq)]
pub enum ClassicalType {
    Bit,
    Int(Option<u32>), // Optional bit width
    Uint(Option<u32>),
    Float(Option<u32>),
    Angle,
    Bool,
    Complex(Box<ClassicalType>),
    Array(Box<ClassicalType>, Expression), // Type and size
}

/// Gate definition
#[derive(Debug, Clone, PartialEq)]
pub struct GateDefinition {
    pub name: String,
    pub parameters: Vec<String>, // Parameter names
    pub qubits: Vec<String>,     // Qubit arguments
    pub body: Vec<GateOperation>,
}

/// Operations allowed in gate definitions
#[derive(Debug, Clone, PartialEq)]
pub enum GateOperation {
    /// Gate application within gate definition
    Gate(GateApplication),
    /// Control flow within gate
    ControlFlow(ControlFlow),
}

/// Function definition
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<ClassicalType>,
    pub body: Vec<Statement>,
}

/// Function parameter
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub typ: ClassicalType,
}

/// Gate application (quantum operation)
#[derive(Debug, Clone, PartialEq)]
pub struct GateApplication {
    pub name: String,
    pub parameters: Vec<Expression>, // Classical parameters
    pub qubits: Vec<QubitRef>,      // Quantum arguments
    pub modifiers: Vec<GateModifier>,
}

/// Gate modifiers
#[derive(Debug, Clone, PartialEq)]
pub enum GateModifier {
    Control(Vec<QubitRef>), // Controlled gate
    Negctrl(Vec<QubitRef>), // Negative control
    Inv,                     // Inverse
    Pow(Expression),         // Power
}

/// Reference to a qubit or qubit register
#[derive(Debug, Clone, PartialEq)]
pub enum QubitRef {
    /// Single qubit by name
    Single(String),
    /// Indexed qubit from register
    Indexed(String, Box<Expression>),
    /// Range of qubits
    Range(String, Box<Expression>, Box<Expression>), // name, start, end
    /// All qubits in register
    All(String),
}

/// Measurement operation
#[derive(Debug, Clone, PartialEq)]
pub struct Measurement {
    pub qubits: Vec<QubitRef>,
    pub target: Option<String>, // Classical register to store result
}

/// Assignment statement
#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub target: String,
    pub index: Option<Expression>, // For array assignment
    pub operator: AssignmentOp,
    pub value: Expression,
}

/// Assignment operators
#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentOp {
    Assign,    // =
    AddAssign, // +=
    SubAssign, // -=
    MulAssign, // *=
    DivAssign, // /=
    ModAssign, // %=
}

/// Control flow statements
#[derive(Debug, Clone, PartialEq)]
pub enum ControlFlow {
    /// If statement
    If {
        condition: Expression,
        then_body: Vec<Statement>,
        else_body: Option<Vec<Statement>>,
    },
    /// For loop
    For {
        variable: String,
        iterable: Expression,
        body: Vec<Statement>,
    },
    /// While loop
    While {
        condition: Expression,
        body: Vec<Statement>,
    },
    /// Switch statement
    Switch {
        expression: Expression,
        cases: Vec<(Expression, Vec<Statement>)>, // (value, statements)
        default: Option<Vec<Statement>>,
    },
}

/// Barrier statement
#[derive(Debug, Clone, PartialEq)]
pub struct Barrier {
    pub qubits: Vec<QubitRef>,
}

/// Reset statement
#[derive(Debug, Clone, PartialEq)]
pub struct Reset {
    pub target: QubitRef,
}

/// Expressions in OpenQASM 3.0
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    /// Integer literal
    Integer(i64),
    /// Floating-point literal
    Float(f64),
    /// Boolean literal
    Boolean(bool),
    /// String literal
    String(String),
    /// Identifier reference
    Identifier(String),
    /// Array/register indexing
    Index(Box<Expression>, Box<Expression>), // array[index]
    /// Binary operation
    Binary {
        left: Box<Expression>,
        operator: BinaryOp,
        right: Box<Expression>,
    },
    /// Unary operation
    Unary {
        operator: UnaryOp,
        operand: Box<Expression>,
    },
    /// Function call
    FunctionCall {
        name: String,
        args: Vec<Expression>,
    },
    /// Range expression
    Range {
        start: Box<Expression>,
        end: Box<Expression>,
    },
    /// Measurement expression
    MeasureExpression(Box<QubitRef>),
}

/// Binary operators
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    // Arithmetic
    Add, Sub, Mul, Div, Mod, Pow,
    // Comparison
    Eq, Ne, Lt, Le, Gt, Ge,
    // Logical
    And, Or, Xor,
    // Bitwise
    BitAnd, BitOr, BitXor, Shl, Shr,
}

/// Unary operators
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Plus, Minus, Not, BitNot,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_program_creation() {
        let mut program = Program::new(Version::openqasm_3_0());
        assert_eq!(program.version, Version::new(3, 0));
        assert!(program.statements.is_empty());
        
        // Add a qubit declaration
        let qubit_decl = Statement::QuantumDeclaration(QuantumDeclaration {
            name: "q".to_string(),
            size: Some(Expression::Integer(2)),
        });
        program.add_statement(qubit_decl);
        assert_eq!(program.statements.len(), 1);
    }
    
    #[test]
    fn test_gate_application() {
        let h_gate = GateApplication {
            name: "h".to_string(),
            parameters: Vec::new(),
            qubits: vec![QubitRef::Indexed("q".to_string(), Box::new(Expression::Integer(0)))],
            modifiers: Vec::new(),
        };
        
        assert_eq!(h_gate.name, "h");
        assert_eq!(h_gate.qubits.len(), 1);
    }
    
    #[test]
    fn test_classical_types() {
        let int_type = ClassicalType::Int(Some(32));
        let array_type = ClassicalType::Array(
            Box::new(ClassicalType::Bit),
            Expression::Integer(8)
        );
        
        match int_type {
            ClassicalType::Int(Some(32)) => {},
            _ => panic!("Expected Int(Some(32))"),
        }
        
        match array_type {
            ClassicalType::Array(inner, _) => {
                assert_eq!(*inner, ClassicalType::Bit);
            },
            _ => panic!("Expected Array type"),
        }
    }
}