//! Parser module for OpenQASM 3.0 source code
//! 
//! This module uses the `nom` parsing library to convert OpenQASM 3.0 source code
//! into the AST data structures defined in the `ast` module.

use crate::ast::*;
use crate::error::Error;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0, multispace1, space0},
    combinator::{map, map_res, opt, recognize, value},
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

/// Parse a complete OpenQASM 3.0 program
pub fn parse(input: &str) -> Result<Program, Error> {
    match program(input) {
        Ok(("", program)) => Ok(program),
        Ok((remaining, _)) => Err(Error::parse_error(
            1, 
            input.len() - remaining.len(), 
            format!("Unexpected input remaining: {}", remaining.chars().take(20).collect::<String>())
        )),
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            Err(Error::parse_error(1, 0, format!("Parse error: {:?}", e)))
        }
        Err(nom::Err::Incomplete(_)) => {
            Err(Error::parse_error(1, 0, "Incomplete input".to_string()))
        }
    }
}

/// Parse a complete program
fn program(input: &str) -> IResult<&str, Program> {
    let (input, _) = multispace0(input)?;
    let (input, version) = version_declaration(input)?;
    let (input, _) = multispace0(input)?;
    let (input, includes) = many0(terminated(include_statement, multispace0))(input)?;
    let (input, statements) = many0(terminated(statement, multispace0))(input)?;
    let (input, _) = multispace0(input)?;
    
    Ok((input, Program {
        version,
        includes,
        statements,
    }))
}

/// Parse version declaration: OPENQASM 3.0;
fn version_declaration(input: &str) -> IResult<&str, Version> {
    let (input, _) = tag("OPENQASM")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, major) = map_res(digit1, |s: &str| s.parse::<u32>())(input)?;
    let (input, _) = char('.')(input)?;
    let (input, minor) = map_res(digit1, |s: &str| s.parse::<u32>())(input)?;
    let (input, _) = char(';')(input)?;
    
    Ok((input, Version::new(major, minor)))
}

/// Parse include statement: include "filename";
fn include_statement(input: &str) -> IResult<&str, Include> {
    let (input, _) = tag("include")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, filename) = string_literal(input)?;
    let (input, _) = char(';')(input)?;
    
    Ok((input, Include { filename }))
}

/// Parse a statement
fn statement(input: &str) -> IResult<&str, Statement> {
    alt((
        map(quantum_declaration, Statement::QuantumDeclaration),
        map(classical_declaration, Statement::ClassicalDeclaration),
        map(gate_definition, Statement::GateDefinition),
        map(function_definition, Statement::FunctionDefinition),
        map(terminated(reset, char(';')), Statement::Reset),
        map(terminated(barrier, char(';')), Statement::Barrier),
        // Try assignment before measurement since measurement can also be on RHS
        map(terminated(assignment, char(';')), Statement::Assignment),
        map(terminated(measurement, char(';')), Statement::Measurement),
        map(control_flow, Statement::ControlFlow),
        map(terminated(gate_application, char(';')), Statement::GateApplication),
        map(terminated(expression, char(';')), Statement::Expression),
    ))(input)
}

/// Parse quantum register declaration: qubit q[2];
fn quantum_declaration(input: &str) -> IResult<&str, QuantumDeclaration> {
    let (input, _) = tag("qubit")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = identifier(input)?;
    let (input, size) = opt(delimited(
        char('['),
        preceded(space0, expression),
        preceded(space0, char(']'))
    ))(input)?;
    let (input, _) = char(';')(input)?;
    
    Ok((input, QuantumDeclaration { name, size }))
}

/// Parse classical register declaration: bit c[8];
fn classical_declaration(input: &str) -> IResult<&str, ClassicalDeclaration> {
    let (input, typ) = classical_type(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = identifier(input)?;
    let (input, size) = opt(delimited(
        char('['),
        preceded(space0, expression),
        preceded(space0, char(']'))
    ))(input)?;
    let (input, initial_value) = opt(preceded(
        tuple((space0, char('='), space0)),
        expression
    ))(input)?;
    let (input, _) = char(';')(input)?;
    
    Ok((input, ClassicalDeclaration {
        typ,
        name,
        size,
        initial_value,
    }))
}

/// Parse classical type
fn classical_type(input: &str) -> IResult<&str, ClassicalType> {
    alt((
        value(ClassicalType::Bit, tag("bit")),
        map(
            tuple((tag("int"), opt(delimited(char('['), digit1, char(']'))))),
            |(_, width)| ClassicalType::Int(width.map(|w: &str| w.parse().unwrap()))
        ),
        map(
            tuple((tag("uint"), opt(delimited(char('['), digit1, char(']'))))),
            |(_, width)| ClassicalType::Uint(width.map(|w: &str| w.parse().unwrap()))
        ),
        map(
            tuple((tag("float"), opt(delimited(char('['), digit1, char(']'))))),
            |(_, width)| ClassicalType::Float(width.map(|w: &str| w.parse().unwrap()))
        ),
        value(ClassicalType::Angle, tag("angle")),
        value(ClassicalType::Bool, tag("bool")),
    ))(input)
}

/// Parse gate definition
fn gate_definition(input: &str) -> IResult<&str, GateDefinition> {
    let (input, _) = tag("gate")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = identifier(input)?;
    
    // Optional parameters
    let (input, parameters) = opt(delimited(
        char('('),
        separated_list0(tuple((space0, char(','), space0)), identifier),
        char(')')
    ))(input)?;
    let parameters = parameters.unwrap_or_default();
    
    let (input, _) = multispace0(input)?;
    let (input, qubits) = separated_list1(
        tuple((space0, char(','), space0)),
        identifier
    )(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('{')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, body) = many0(terminated(gate_operation, multispace0))(input)?;
    let (input, _) = char('}')(input)?;
    
    Ok((input, GateDefinition {
        name,
        parameters,
        qubits,
        body,
    }))
}

/// Parse gate operation (inside gate definition)
fn gate_operation(input: &str) -> IResult<&str, GateOperation> {
    alt((
        map(terminated(gate_application, char(';')), GateOperation::Gate),
        map(control_flow, GateOperation::ControlFlow),
    ))(input)
}

/// Parse function definition
fn function_definition(input: &str) -> IResult<&str, FunctionDefinition> {
    let (input, _) = tag("def")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = char('(')(input)?;
    let (input, parameters) = separated_list0(
        tuple((space0, char(','), space0)),
        parameter
    )(input)?;
    let (input, _) = char(')')(input)?;
    
    // Optional return type
    let (input, return_type) = opt(preceded(
        tuple((space0, tag("->"), space0)),
        classical_type
    ))(input)?;
    
    let (input, _) = multispace0(input)?;
    let (input, _) = char('{')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, body) = many0(terminated(statement, multispace0))(input)?;
    let (input, _) = char('}')(input)?;
    
    Ok((input, FunctionDefinition {
        name,
        parameters,
        return_type,
        body,
    }))
}

/// Parse function parameter
fn parameter(input: &str) -> IResult<&str, Parameter> {
    let (input, typ) = classical_type(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = identifier(input)?;
    
    Ok((input, Parameter { name, typ }))
}

/// Parse gate application: h q[0];
fn gate_application(input: &str) -> IResult<&str, GateApplication> {
    let (input, modifiers) = many0(terminated(gate_modifier, multispace1))(input)?;
    let (input, name) = identifier(input)?;
    
    // Optional parameters
    let (input, parameters) = opt(delimited(
        char('('),
        separated_list0(tuple((space0, char(','), space0)), expression),
        char(')')
    ))(input)?;
    let parameters = parameters.unwrap_or_default();
    
    let (input, _) = multispace0(input)?;
    let (input, qubits) = separated_list1(
        tuple((space0, char(','), space0)),
        qubit_ref
    )(input)?;
    
    Ok((input, GateApplication {
        name,
        parameters,
        qubits,
        modifiers,
    }))
}

/// Parse gate modifier
fn gate_modifier(input: &str) -> IResult<&str, GateModifier> {
    alt((
        map(
            delimited(
                tag("ctrl("),
                separated_list1(tuple((space0, char(','), space0)), qubit_ref),
                char(')')
            ),
            GateModifier::Control
        ),
        map(
            delimited(
                tag("negctrl("),
                separated_list1(tuple((space0, char(','), space0)), qubit_ref),
                char(')')
            ),
            GateModifier::Negctrl
        ),
        value(GateModifier::Inv, tag("inv")),
        map(
            delimited(tag("pow("), expression, char(')')),
            GateModifier::Pow
        ),
    ))(input)
}

/// Parse qubit reference
fn qubit_ref(input: &str) -> IResult<&str, QubitRef> {
    let (input, name) = identifier(input)?;
    
    alt((
        // Range: q[0:2]
        {
            let name = name.clone();
            map(
                delimited(
                    char('['),
                    tuple((
                        preceded(space0, expression),
                        preceded(tuple((space0, char(':'), space0)), expression)
                    )),
                    preceded(space0, char(']'))
                ),
                move |(start, end)| QubitRef::Range(name.clone(), Box::new(start), Box::new(end))
            )
        },
        // Indexed: q[0]
        {
            let name = name.clone();
            map(
                delimited(char('['), preceded(space0, expression), preceded(space0, char(']'))),
                move |index| QubitRef::Indexed(name.clone(), Box::new(index))
            )
        },
        // Single: q
        {
            let name = name.clone();
            map(
                nom::combinator::success(()),
                move |_| QubitRef::Single(name.clone())
            )
        },
    ))(input)
}

/// Parse measurement: c = measure q;
fn measurement(input: &str) -> IResult<&str, Measurement> {
    let (input, target) = opt(terminated(identifier, tuple((space0, char('='), space0))))(input)?;
    let (input, _) = tag("measure")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, qubits) = separated_list1(
        tuple((space0, char(','), space0)),
        qubit_ref
    )(input)?;
    
    Ok((input, Measurement { qubits, target }))
}

/// Parse assignment: c[0] += 1;
fn assignment(input: &str) -> IResult<&str, Assignment> {
    let (input, target) = identifier(input)?;
    let (input, index) = opt(delimited(
        char('['),
        preceded(space0, expression),
        preceded(space0, char(']'))
    ))(input)?;
    let (input, _) = space0(input)?;
    let (input, operator) = assignment_op(input)?;
    let (input, _) = space0(input)?;
    let (input, value) = expression(input)?;
    
    Ok((input, Assignment {
        target,
        index,
        operator,
        value,
    }))
}

/// Parse assignment operator
fn assignment_op(input: &str) -> IResult<&str, AssignmentOp> {
    alt((
        value(AssignmentOp::AddAssign, tag("+=")),
        value(AssignmentOp::SubAssign, tag("-=")),
        value(AssignmentOp::MulAssign, tag("*=")),
        value(AssignmentOp::DivAssign, tag("/=")),
        value(AssignmentOp::ModAssign, tag("%=")),
        value(AssignmentOp::Assign, tag("=")),
    ))(input)
}

/// Parse control flow statements
fn control_flow(input: &str) -> IResult<&str, ControlFlow> {
    alt((
        parse_if,
        parse_for,
        parse_while,
    ))(input)
}

fn parse_if(input: &str) -> IResult<&str, ControlFlow> {
    let (input, _) = tag("if")(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char('(')(input)?;
    let (input, condition) = expression(input)?;
    let (input, _) = char(')')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('{')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, then_body) = many0(terminated(statement, multispace0))(input)?;
    let (input, _) = char('}')(input)?;
    
    let (input, else_body) = opt(preceded(
        tuple((multispace0, tag("else"), multispace0, char('{'), multispace0)),
        terminated(
            many0(terminated(statement, multispace0)),
            preceded(multispace0, char('}'))
        )
    ))(input)?;
    
    Ok((input, ControlFlow::If {
        condition,
        then_body,
        else_body,
    }))
}

fn parse_for(input: &str) -> IResult<&str, ControlFlow> {
    let (input, _) = tag("for")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, variable) = identifier(input)?;
    let (input, _) = multispace1(input)?;
    let (input, _) = tag("in")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, iterable) = expression(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('{')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, body) = many0(terminated(statement, multispace0))(input)?;
    let (input, _) = char('}')(input)?;
    
    Ok((input, ControlFlow::For {
        variable,
        iterable,
        body,
    }))
}

fn parse_while(input: &str) -> IResult<&str, ControlFlow> {
    let (input, _) = tag("while")(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char('(')(input)?;
    let (input, condition) = expression(input)?;
    let (input, _) = char(')')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('{')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, body) = many0(terminated(statement, multispace0))(input)?;
    let (input, _) = char('}')(input)?;
    
    Ok((input, ControlFlow::While { condition, body }))
}

/// Parse barrier statement
fn barrier(input: &str) -> IResult<&str, Barrier> {
    let (input, _) = tag("barrier")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, qubits) = separated_list1(
        tuple((space0, char(','), space0)),
        qubit_ref
    )(input)?;
    
    Ok((input, Barrier { qubits }))
}

/// Parse reset statement
fn reset(input: &str) -> IResult<&str, Reset> {
    let (input, _) = tag("reset")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, target) = qubit_ref(input)?;
    
    Ok((input, Reset { target }))
}

/// Parse expression
fn expression(input: &str) -> IResult<&str, Expression> {
    alt((
        parse_binary_expr,
        parse_primary_expr,
    ))(input)
}

fn parse_binary_expr(input: &str) -> IResult<&str, Expression> {
    let (input, left) = parse_primary_expr(input)?;
    let (input, _) = space0(input)?;
    let (input, operator) = binary_op(input)?;
    let (input, _) = space0(input)?;
    let (input, right) = expression(input)?;
    
    Ok((input, Expression::Binary {
        left: Box::new(left),
        operator,
        right: Box::new(right),
    }))
}

fn parse_primary_expr(input: &str) -> IResult<&str, Expression> {
    alt((
        // Parenthesized expression
        delimited(
            char('('),
            preceded(space0, terminated(expression, space0)),
            char(')')
        ),
        // Integer literal
        map(
            map_res(digit1, |s: &str| s.parse::<i64>()),
            Expression::Integer
        ),
        // Float literal
        map(
            map_res(
                recognize(tuple((digit1, char('.'), digit1))),
                |s: &str| s.parse::<f64>()
            ),
            Expression::Float
        ),
        // Boolean literal
        alt((
            value(Expression::Boolean(true), tag("true")),
            value(Expression::Boolean(false), tag("false")),
        )),
        // String literal
        map(string_literal, Expression::String),
        // Function call or identifier
        parse_identifier_or_call,
        // Measure expression
        map(
            preceded(pair(tag("measure"), multispace1), qubit_ref),
            |qref| Expression::MeasureExpression(Box::new(qref))
        ),
    ))(input)
}

fn parse_identifier_or_call(input: &str) -> IResult<&str, Expression> {
    let (input, name) = identifier(input)?;
    
    alt((
        // Function call
        {
            let name = name.clone();
            map(
                delimited(
                    char('('),
                    separated_list0(tuple((space0, char(','), space0)), expression),
                    char(')')
                ),
                move |args| Expression::FunctionCall {
                    name: name.clone(),
                    args,
                }
            )
        },
        // Array index
        {
            let name = name.clone();
            map(
                delimited(
                    char('['),
                    preceded(space0, expression),
                    preceded(space0, char(']'))
                ),
                move |index| Expression::Index(
                    Box::new(Expression::Identifier(name.clone())),
                    Box::new(index)
                )
            )
        },
        // Plain identifier
        {
            let name = name.clone();
            map(
                nom::combinator::success(()),
                move |_| Expression::Identifier(name.clone())
            )
        },
    ))(input)
}

/// Parse binary operator
fn binary_op(input: &str) -> IResult<&str, BinaryOp> {
    alt((
        value(BinaryOp::Pow, tag("**")),
        value(BinaryOp::Eq, tag("==")),
        value(BinaryOp::Ne, tag("!=")),
        value(BinaryOp::Le, tag("<=")),
        value(BinaryOp::Ge, tag(">=")),
        value(BinaryOp::And, tag("&&")),
        value(BinaryOp::Or, tag("||")),
        value(BinaryOp::Shl, tag("<<")),
        value(BinaryOp::Shr, tag(">>")),
        value(BinaryOp::Add, tag("+")),
        value(BinaryOp::Sub, tag("-")),
        value(BinaryOp::Mul, tag("*")),
        value(BinaryOp::Div, tag("/")),
        value(BinaryOp::Mod, tag("%")),
        value(BinaryOp::Lt, tag("<")),
        value(BinaryOp::Gt, tag(">")),
        value(BinaryOp::BitAnd, tag("&")),
        value(BinaryOp::BitOr, tag("|")),
        value(BinaryOp::BitXor, tag("^")),
        value(BinaryOp::Xor, tag("^")),
    ))(input)
}

/// Parse identifier
fn identifier(input: &str) -> IResult<&str, String> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_"))))
        )),
        |s: &str| s.to_string()
    )(input)
}

/// Parse string literal
fn string_literal(input: &str) -> IResult<&str, String> {
    delimited(
        char('"'),
        map(
            take_while(|c| c != '"'),
            |s: &str| s.to_string()
        ),
        char('"')
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_parsing() {
        let input = "OPENQASM 3.0;";
        let (_, version) = version_declaration(input).unwrap();
        assert_eq!(version, Version::new(3, 0));
    }
    
    #[test]
    fn test_qubit_declaration() {
        let input = "qubit q[2];";
        let (_, decl) = quantum_declaration(input).unwrap();
        assert_eq!(decl.name, "q");
        assert_eq!(decl.size, Some(Expression::Integer(2)));
    }
    
    #[test]
    fn test_gate_application() {
        let input = "h q[0];";
        let (_, gate) = terminated(gate_application, char(';'))(input).unwrap();
        assert_eq!(gate.name, "h");
        assert_eq!(gate.qubits.len(), 1);
    }
    
    #[test]
    fn test_simple_program() {
        let code = r#"
OPENQASM 3.0;
qubit q[2];
h q[0];
        "#;
        
        let program = parse(code).unwrap();
        assert_eq!(program.version, Version::openqasm_3_0());
        assert_eq!(program.statements.len(), 2);
    }
    
    #[test]
    fn test_controlled_gate() {
        let input = "ctrl(q[0]) x q[1];";
        let (_, gate) = terminated(gate_application, char(';'))(input).unwrap();
        assert_eq!(gate.name, "x");
        assert_eq!(gate.modifiers.len(), 1);
        match &gate.modifiers[0] {
            GateModifier::Control(qubits) => assert_eq!(qubits.len(), 1),
            _ => panic!("Expected control modifier"),
        }
    }
}