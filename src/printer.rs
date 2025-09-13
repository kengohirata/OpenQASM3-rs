//! Printer module for converting AST back to OpenQASM 3.0 source code

use crate::ast::*;
use std::fmt::{self, Display, Formatter};

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "OPENQASM {};", self.version)?;
        
        // Print includes
        for include in &self.includes {
            writeln!(f, "{}", include)?;
        }
        
        if !self.includes.is_empty() {
            writeln!(f)?; // Empty line after includes
        }
        
        // Print statements
        for statement in &self.statements {
            writeln!(f, "{}", statement)?;
        }
        
        Ok(())
    }
}

impl Display for Version {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.major, self.minor)
    }
}

impl Display for Include {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "include \"{}\";", self.filename)
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::QuantumDeclaration(decl) => write!(f, "{};", decl),
            Statement::ClassicalDeclaration(decl) => write!(f, "{};", decl),
            Statement::GateDefinition(def) => write!(f, "{}", def),
            Statement::FunctionDefinition(def) => write!(f, "{}", def),
            Statement::GateApplication(app) => write!(f, "{};", app),
            Statement::Measurement(meas) => write!(f, "{};", meas),
            Statement::Assignment(assign) => write!(f, "{};", assign),
            Statement::Expression(expr) => write!(f, "{};", expr),
            Statement::ControlFlow(cf) => write!(f, "{}", cf),
            Statement::Barrier(barrier) => write!(f, "{};", barrier),
            Statement::Reset(reset) => write!(f, "{};", reset),
        }
    }
}

impl Display for QuantumDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.size {
            Some(size) => write!(f, "qubit {}[{}]", self.name, size),
            None => write!(f, "qubit {}", self.name),
        }
    }
}

impl Display for ClassicalDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.size {
            Some(size) => {
                write!(f, "{} {}[{}]", self.typ, self.name, size)?;
            }
            None => {
                write!(f, "{} {}", self.typ, self.name)?;
            }
        }
        
        if let Some(init) = &self.initial_value {
            write!(f, " = {}", init)?;
        }
        
        Ok(())
    }
}

impl Display for ClassicalType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ClassicalType::Bit => write!(f, "bit"),
            ClassicalType::Int(None) => write!(f, "int"),
            ClassicalType::Int(Some(width)) => write!(f, "int[{}]", width),
            ClassicalType::Uint(None) => write!(f, "uint"),
            ClassicalType::Uint(Some(width)) => write!(f, "uint[{}]", width),
            ClassicalType::Float(None) => write!(f, "float"),
            ClassicalType::Float(Some(width)) => write!(f, "float[{}]", width),
            ClassicalType::Angle => write!(f, "angle"),
            ClassicalType::Bool => write!(f, "bool"),
            ClassicalType::Complex(inner) => write!(f, "complex[{}]", inner),
            ClassicalType::Array(inner, size) => write!(f, "array[{}, {}]", inner, size),
        }
    }
}

impl Display for GateDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "gate {}", self.name)?;
        
        // Parameters
        if !self.parameters.is_empty() {
            write!(f, "(")?;
            for (i, param) in self.parameters.iter().enumerate() {
                if i > 0 { write!(f, ", ")?; }
                write!(f, "{}", param)?;
            }
            write!(f, ")")?;
        }
        
        // Qubits
        write!(f, " ")?;
        for (i, qubit) in self.qubits.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}", qubit)?;
        }
        
        writeln!(f, " {{")?;
        
        // Body
        for op in &self.body {
            writeln!(f, "    {}", op)?;
        }
        
        writeln!(f, "}}")
    }
}

impl Display for GateOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GateOperation::Gate(gate) => write!(f, "{};", gate),
            GateOperation::ControlFlow(cf) => write!(f, "{}", cf),
        }
    }
}

impl Display for FunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "def {}", self.name)?;
        
        // Parameters
        write!(f, "(")?;
        for (i, param) in self.parameters.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}", param)?;
        }
        write!(f, ")")?;
        
        // Return type
        if let Some(ret_type) = &self.return_type {
            write!(f, " -> {}", ret_type)?;
        }
        
        writeln!(f, " {{")?;
        
        // Body
        for stmt in &self.body {
            writeln!(f, "    {}", stmt)?;
        }
        
        writeln!(f, "}}")
    }
}

impl Display for Parameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.typ, self.name)
    }
}

impl Display for GateApplication {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // Modifiers
        for modifier in &self.modifiers {
            write!(f, "{} ", modifier)?;
        }
        
        write!(f, "{}", self.name)?;
        
        // Parameters
        if !self.parameters.is_empty() {
            write!(f, "(")?;
            for (i, param) in self.parameters.iter().enumerate() {
                if i > 0 { write!(f, ", ")?; }
                write!(f, "{}", param)?;
            }
            write!(f, ")")?;
        }
        
        // Qubits
        write!(f, " ")?;
        for (i, qubit) in self.qubits.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}", qubit)?;
        }
        
        Ok(())
    }
}

impl Display for GateModifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GateModifier::Control(qubits) => {
                write!(f, "ctrl(")?;
                for (i, qubit) in qubits.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", qubit)?;
                }
                write!(f, ")")
            }
            GateModifier::Negctrl(qubits) => {
                write!(f, "negctrl(")?;
                for (i, qubit) in qubits.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", qubit)?;
                }
                write!(f, ")")
            }
            GateModifier::Inv => write!(f, "inv"),
            GateModifier::Pow(expr) => write!(f, "pow({})", expr),
        }
    }
}

impl Display for QubitRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            QubitRef::Single(name) => write!(f, "{}", name),
            QubitRef::Indexed(name, index) => write!(f, "{}[{}]", name, index),
            QubitRef::Range(name, start, end) => write!(f, "{}[{}:{}]", name, start, end),
            QubitRef::All(name) => write!(f, "{}", name),
        }
    }
}

impl Display for Measurement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(target) = &self.target {
            write!(f, "{} = ", target)?;
        }
        
        write!(f, "measure ")?;
        for (i, qubit) in self.qubits.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}", qubit)?;
        }
        
        Ok(())
    }
}

impl Display for Assignment {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.target)?;
        
        if let Some(index) = &self.index {
            write!(f, "[{}]", index)?;
        }
        
        write!(f, " {} {}", self.operator, self.value)
    }
}

impl Display for AssignmentOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AssignmentOp::Assign => write!(f, "="),
            AssignmentOp::AddAssign => write!(f, "+="),
            AssignmentOp::SubAssign => write!(f, "-="),
            AssignmentOp::MulAssign => write!(f, "*="),
            AssignmentOp::DivAssign => write!(f, "/="),
            AssignmentOp::ModAssign => write!(f, "%="),
        }
    }
}

impl Display for ControlFlow {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ControlFlow::If { condition, then_body, else_body } => {
                writeln!(f, "if ({}) {{", condition)?;
                for stmt in then_body {
                    writeln!(f, "    {}", stmt)?;
                }
                if let Some(else_stmts) = else_body {
                    writeln!(f, "}} else {{")?;
                    for stmt in else_stmts {
                        writeln!(f, "    {}", stmt)?;
                    }
                }
                write!(f, "}}")
            }
            ControlFlow::For { variable, iterable, body } => {
                writeln!(f, "for {} in {} {{", variable, iterable)?;
                for stmt in body {
                    writeln!(f, "    {}", stmt)?;
                }
                write!(f, "}}")
            }
            ControlFlow::While { condition, body } => {
                writeln!(f, "while ({}) {{", condition)?;
                for stmt in body {
                    writeln!(f, "    {}", stmt)?;
                }
                write!(f, "}}")
            }
            ControlFlow::Switch { expression, cases, default } => {
                writeln!(f, "switch ({}) {{", expression)?;
                for (value, stmts) in cases {
                    writeln!(f, "case {}: {{", value)?;
                    for stmt in stmts {
                        writeln!(f, "        {}", stmt)?;
                    }
                    writeln!(f, "    }}")?;
                }
                if let Some(default_stmts) = default {
                    writeln!(f, "    default: {{")?;
                    for stmt in default_stmts {
                        writeln!(f, "        {}", stmt)?;
                    }
                    writeln!(f, "    }}")?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl Display for Barrier {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "barrier ")?;
        for (i, qubit) in self.qubits.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}", qubit)?;
        }
        Ok(())
    }
}

impl Display for Reset {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "reset {}", self.target)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Integer(n) => write!(f, "{}", n),
            Expression::Float(n) => write!(f, "{}", n),
            Expression::Boolean(b) => write!(f, "{}", b),
            Expression::String(s) => write!(f, "\"{}\"", s),
            Expression::Identifier(name) => write!(f, "{}", name),
            Expression::Index(array, index) => write!(f, "{}[{}]", array, index),
            Expression::Binary { left, operator, right } => {
                write!(f, "({} {} {})", left, operator, right)
            }
            Expression::Unary { operator, operand } => {
                write!(f, "({}{})", operator, operand)
            }
            Expression::FunctionCall { name, args } => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expression::Range { start, end } => {
                write!(f, "{}:{}", start, end)
            }
            Expression::MeasureExpression(qubit) => {
                write!(f, "measure {}", qubit)
            }
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Mod => write!(f, "%"),
            BinaryOp::Pow => write!(f, "**"),
            BinaryOp::Eq => write!(f, "=="),
            BinaryOp::Ne => write!(f, "!="),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::Le => write!(f, "<="),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::Ge => write!(f, ">="),
            BinaryOp::And => write!(f, "&&"),
            BinaryOp::Or => write!(f, "||"),
            BinaryOp::Xor => write!(f, "^"),
            BinaryOp::BitAnd => write!(f, "&"),
            BinaryOp::BitOr => write!(f, "|"),
            BinaryOp::BitXor => write!(f, "^"),
            BinaryOp::Shl => write!(f, "<<"),
            BinaryOp::Shr => write!(f, ">>"),
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Plus => write!(f, "+"),
            UnaryOp::Minus => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::BitNot => write!(f, "~"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_program_printing() {
        let mut program = Program::new(Version::openqasm_3_0());
        program.add_statement(Statement::QuantumDeclaration(QuantumDeclaration {
            name: "q".to_string(),
            size: Some(Expression::Integer(2)),
        }));
        
        let output = format!("{}", program);
        assert!(output.contains("OPENQASM 3.0;"));
        assert!(output.contains("qubit q[2];"));
    }
    
    #[test]
    fn test_gate_application_printing() {
        let h_gate = GateApplication {
            name: "h".to_string(),
            parameters: Vec::new(),
            qubits: vec![QubitRef::Indexed("q".to_string(), Box::new(Expression::Integer(0)))],
            modifiers: Vec::new(),
        };
        
        let output = format!("{}", h_gate);
        assert_eq!(output, "h q[0]");
    }
    
    #[test]
    fn test_controlled_gate_printing() {
        let cnot = GateApplication {
            name: "x".to_string(),
            parameters: Vec::new(),
            qubits: vec![QubitRef::Indexed("q".to_string(), Box::new(Expression::Integer(1)))],
            modifiers: vec![GateModifier::Control(vec![
                QubitRef::Indexed("q".to_string(), Box::new(Expression::Integer(0)))
            ])],
        };
        
        let output = format!("{}", cnot);
        assert!(output.contains("ctrl(q[0])"));
        assert!(output.contains("x q[1]"));
    }
}