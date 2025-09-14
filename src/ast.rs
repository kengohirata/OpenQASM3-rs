/// Definition of the AST based on the OpenQASM 3.0 specification.
/// The definition is taken from "https://openqasm.com/language/types.html",
/// protected under Apache License 2.0
/// (https://github.com/openqasm/openqasm/blob/0178efff5f329b2e75c5062a05c891d537b6ec63/LICENSE).

// =========================================
// AST definitions corresponding to the grammar
// (Inserted without removing or reordering the original comments)
// =========================================

// Common aliases for tokens used as terminals in the grammar
/// lexing.md Identifier — keep raw lexeme for round-tripping and Unicode
pub type Identifier = String;
/// lexing.md StringLiteral — raw quoted content
pub type StringLiteral = String;
/// lexing.md BitstringLiteral — keep raw to preserve width/underscores
pub type BitstringLiteral = String;
/// lexing.md HardwareQubit ('$' DIGITS) — keep raw to preserve leading '$' and digits
pub type HardwareQubit = usize;
pub type AnnotationKeyword = (Vec<Identifier>, RemainingLineContent); // from lexing.md
pub type RemainingLineContent = String; // from lexing.md
/// lexing.md VersionSpecifier: [0-9]+ ('.' [0-9]+)? — (major, optional minor)
pub type VersionSpecifier = (usize, Option<usize>); // from lexing.md: major, optional minor

// ---------- Program ----------
/// grammar.md line 7: program: version? statementOrScope* EOF;
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub version: Option<Version>,
    pub items: Vec<StatementOrScope>,
}

/// grammar.md line 8: version: OPENQASM VersionSpecifier SEMICOLON;
#[derive(Debug, Clone, PartialEq)]
pub struct Version {
    pub spec: VersionSpecifier, // (major, optional minor), e.g., (3, Some(0))
}

// ---------- Statement / Scope ----------
/// grammar.md line 55: statementOrScope: statement | scope;
#[derive(Debug, Clone, PartialEq)]
pub enum StatementOrScope {
    Statement(Statement),
    Scope(Scope),
}

/// grammar.md line 52: scope: '{' statementOrScope* '}'
#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub items: Vec<StatementOrScope>,
}

/// grammar.md line 51: annotation: AnnotationKeyword RemainingLineContent?;
#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
    pub keyword: AnnotationKeyword,
    pub content: Option<RemainingLineContent>,
}

/// grammar.md line 53: pragma: PRAGMA RemainingLineContent;
#[derive(Debug, Clone, PartialEq)]
pub struct Pragma {
    pub content: RemainingLineContent, // RemainingLineContent
}

/// grammar.md lines 15–49: statement: pragma | annotation* (...variants...)
#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub annotations: Vec<Annotation>,
    pub kind: StatementKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    // Inclusion
    CalibrationGrammar { grammar: StringLiteral },
    Include { path: StringLiteral },

    // Control-flow
    Break,
    Continue,
    End,
    For(ForStatement),
    If(IfStatement),
    Return(Option<ReturnValue>),
    While(WhileStatement),
    Switch(SwitchStatement),

    // Quantum directives
    Barrier(Option<GateOperandList>),
    Box(BoxStatement),
    Delay(DelayStatement),
    Nop(Option<GateOperandList>),
    GateCall(GateCallStatement),
    MeasureArrowAssignment(MeasureArrowAssignmentStatement),
    Reset(GateOperand),

    // Declarations
    AliasDeclaration(AliasDeclarationStatement),
    ClassicalDeclaration(ClassicalDeclarationStatement),
    ConstDeclaration(ConstDeclarationStatement),
    IoDeclaration(IoDeclarationStatement),
    OldStyleDeclaration(OldStyleDeclarationStatement),
    QuantumDeclaration(QuantumDeclarationStatement),

    // Higher-order
    Def(DefStatement),
    Extern(ExternStatement),
    Gate(GateStatement),

    // Non-declaration calculations
    Assignment(AssignmentStatement),
    Expression(Expression),

    // Calibration language
    Cal(CalStatement),
    Defcal(DefcalStatement),

    // Direct pragma statement branch
    Pragma(Pragma),
}

// ---------- Inclusion ----------
#[derive(Debug, Clone, PartialEq)]
pub struct CalibrationGrammarStatement {
    pub grammar: StringLiteral,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IncludeStatement {
    pub path: StringLiteral,
}

// ---------- Control-flow ----------
/// grammar.md line 68: forStatement: FOR scalarType Identifier IN (...) body=statementOrScope;
#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub ty: ScalarType, // grammar: scalarType
    pub var: Identifier,
    pub iterable: ForIterable,
    pub body: Box<StatementOrScope>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForIterable {
    Set(SetExpression),
    RangeInBrackets(RangeExpression),
    Expr(Expression),
}

/// grammar.md line 69: ifStatement: IF '(' expression ')' if_body=... (ELSE else_body=...)?
#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Expression,
    pub if_body: Box<StatementOrScope>,
    pub else_body: Option<Box<StatementOrScope>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ReturnValue {
    Expr(Expression),
    Measure(MeasureExpression),
}

/// grammar.md line 71: whileStatement: WHILE '(' expression ')' body=statementOrScope;
#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Box<StatementOrScope>,
}

/// grammar.md line 72–76: switchStatement / switchCaseItem
#[derive(Debug, Clone, PartialEq)]
pub struct SwitchStatement {
    pub expr: Expression,
    pub items: Vec<SwitchCaseItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SwitchCaseItem {
    Case { expressions: Vec<Expression>, scope: Scope },
    Default { scope: Scope },
}

// ---------- Quantum directives ----------
/// grammar.md line 80: boxStatement: BOX designator? scope;
#[derive(Debug, Clone, PartialEq)]
pub struct BoxStatement {
    pub designator: Option<Designator>,
    pub body: Scope,
}

/// grammar.md line 81: delayStatement: DELAY designator gateOperandList?;
#[derive(Debug, Clone, PartialEq)]
pub struct DelayStatement {
    pub designator: Designator,
    pub operands: Option<GateOperandList>,
}

/// grammar.md lines 91–94: gateCallStatement (named | GPHASE)
#[derive(Debug, Clone, PartialEq)]
pub struct GateCallStatement {
    pub modifiers: Vec<GateModifier>,
    pub target: GateTarget,
    pub parameters: Option<Vec<Expression>>, // expressionList inside optional parentheses
    pub designator: Option<Designator>,
    pub operands: Option<GateOperandList>, // present for named gates; optional for GPHASE
}

#[derive(Debug, Clone, PartialEq)]
pub enum GateTarget {
    Named(Identifier),
    GPhase,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MeasureArrowAssignmentStatement {
    pub measure: MeasureExpression,
    pub target: Option<IndexedIdentifier>,
}

// ---------- Declarations ----------
/// grammar.md line 101: aliasDeclarationStatement
#[derive(Debug, Clone, PartialEq)]
pub struct AliasDeclarationStatement {
    pub name: Identifier,
    pub exprs: AliasExpression, // expression (DOUBLE_PLUS expression)*
}

/// grammar.md line 102: classicalDeclarationStatement
#[derive(Debug, Clone, PartialEq)]
pub struct ClassicalDeclarationStatement {
    pub ty: ClassicalType, // scalarType | arrayType
    pub name: Identifier,
    pub init: Option<DeclarationExpression>,
}

/// grammar.md line 103: constDeclarationStatement
#[derive(Debug, Clone, PartialEq)]
pub struct ConstDeclarationStatement {
    pub ty: ScalarType, // scalarType only
    pub name: Identifier,
    pub init: DeclarationExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IoDirection { Input, Output }

#[derive(Debug, Clone, PartialEq)]
pub struct IoDeclarationStatement {
    pub direction: IoDirection, // INPUT | OUTPUT
    pub ty: ClassicalType,      // scalarType | arrayType
    pub name: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OldStyleRegisterKind { CReg, QReg }

#[derive(Debug, Clone, PartialEq)]
pub struct OldStyleDeclarationStatement {
    pub kind: OldStyleRegisterKind, // CREG | QREG
    pub name: Identifier,
    pub designator: Option<Designator>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct QuantumDeclarationStatement {
    pub ty: QubitType,
    pub name: Identifier,
}

// ---------- Higher-order ----------
/// grammar.md line 109: defStatement
#[derive(Debug, Clone, PartialEq)]
pub struct DefStatement {
    pub name: Identifier,
    pub args: Vec<ArgumentDefinition>,
    pub return_signature: Option<ReturnSignature>,
    pub body: Scope,
}

/// grammar.md line 110: externStatement
#[derive(Debug, Clone, PartialEq)]
pub struct ExternStatement {
    pub name: Identifier,
    pub args: Vec<ExternArgument>,
    pub return_signature: Option<ReturnSignature>,
}

/// grammar.md line 111: gateStatement
#[derive(Debug, Clone, PartialEq)]
pub struct GateStatement {
    pub name: Identifier,
    pub params: Option<Vec<Identifier>>, // params in parentheses (identifierList?)
    pub qubits: Vec<Identifier>,         // identifierList
    pub body: Scope,
}

// ---------- Non-declaration calculations ----------
/// grammar.md line 114: assignmentStatement
#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentStatement {
    pub target: IndexedIdentifier,
    pub op: AssignmentOp,
    pub value: AssignmentRhs,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    ShlAssign,
    ShrAssign,
    PowAssign, // '**='
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentRhs {
    Expr(Expression),
    Measure(MeasureExpression),
}

/// grammar.md line 118: calStatement; line 119: defcalStatement
#[derive(Debug, Clone, PartialEq)]
pub struct CalStatement {
    pub block: Option<CalibrationBlock>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefcalStatement {
    pub target: DefcalTarget,
    pub args: Vec<DefcalArgumentDefinition>,
    pub operands: Vec<DefcalOperand>,
    pub return_signature: Option<ReturnSignature>,
    pub block: Option<CalibrationBlock>,
}

pub type CalibrationBlock = String; // raw content inside CAL/DEFCAL blocks

// ---------- Expressions and related nodes ----------
/// grammar.md lines 129–160: expression precedence and forms
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Parenthesis(Box<Expression>),
    Index { expr: Box<Expression>, indices: Vec<IndexOperator> },
    Unary { op: UnaryOp, expr: Box<Expression> },
    Binary { op: BinaryOp, lhs: Box<Expression>, rhs: Box<Expression> },
    Cast { ty: ClassicalType, expr: Box<Expression> },
    DurationOf(Box<Scope>),
    Call { callee: Identifier, args: Vec<Expression> },
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    BitNot,       // TILDE
    LogicalNot,   // EXCLAMATION_POINT
    Negate,       // MINUS
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Power,            // DOUBLE_ASTERISK
    Multiply,         // ASTERISK
    Divide,           // SLASH
    Modulo,           // PERCENT
    Add,              // PLUS
    Subtract,         // MINUS
    ShiftLeft,        // BitshiftOperator <<
    ShiftRight,       // BitshiftOperator >>
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    BitAnd,           // AMPERSAND
    BitXor,           // CARET
    BitOr,            // PIPE
    LogicalAnd,       // DOUBLE_AMPERSAND
    LogicalOr,        // DOUBLE_PIPE
}

/// grammar.md lines 147–159: literalExpression alternatives
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Identifier(Identifier),
    Integer(usize),
    Float(f64),
    Number(Number), // ImaginaryLiteral
    Boolean(bool),
    Bitstring(BitstringLiteral),
    Timing(Number, TimeUnit),
    HardwareQubit(HardwareQubit),
}

/// A numeric literal that can be either an integer or a float
#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Integer(usize),
    Float(f64),
}

/// Time units for Timing literals
/// 'dt' | 'ns' | 'us' | 'µs' | 'ms' | 's';
#[derive(Debug, Clone, PartialEq)]
pub enum TimeUnit {
    Dt,
    Ns,
    Us,
    Ms,
    S,
}


#[derive(Debug, Clone, PartialEq)]
pub enum IndexOperator {
    Set(SetExpression),
    Items(Vec<IndexItem>), // (expression | rangeExpression) list
}

#[derive(Debug, Clone, PartialEq)]
pub enum IndexItem {
    Expr(Expression),
    Range(RangeExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AliasExpression {
    pub parts: Vec<Expression>, // expression (DOUBLE_PLUS expression)*
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclarationExpression {
    ArrayLiteral(ArrayLiteral),
    Expr(Expression),
    Measure(MeasureExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MeasureExpression {
    pub operand: GateOperand,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RangeExpression {
    pub start: Option<Box<Expression>>,
    pub end: Option<Box<Expression>>,
    pub step: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SetExpression {
    pub elements: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayLiteral {
    pub elements: Vec<ArrayLiteralItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrayLiteralItem {
    Expr(Expression),
    Array(ArrayLiteral),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexedIdentifier {
    pub name: Identifier,
    pub indices: Vec<IndexOperator>,
}

// ---------- Types ----------
/// grammar.md line 190: returnSignature; lines 191–195: gateModifier; 197–207: scalarType
#[derive(Debug, Clone, PartialEq)]
pub struct ReturnSignature {
    pub ty: ScalarType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum GateModifier {
    Inv,
    Pow(Expression),
    Ctrl(Option<Expression>),
    NegCtrl(Option<Expression>),
}

/// grammar.md lines 197–207: scalarType
#[derive(Debug, Clone, PartialEq)]
pub enum ScalarType {
    Bit { designator: Option<Designator> },
    Int { designator: Option<Designator> },
    UInt { designator: Option<Designator> },
    Float { designator: Option<Designator> },
    Angle { designator: Option<Designator> },
    Bool,
    Duration,
    Stretch,
    Complex { inner: Option<Box<ScalarType>> }, // COMPLEX [ scalarType ] ?
}

/// grammar.md line 208: qubitType; line 209: arrayType; line 210: arrayReferenceType; line 212: designator
#[derive(Debug, Clone, PartialEq)]
pub struct QubitType {
    pub designator: Option<Designator>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub element_type: ScalarType,
    pub dimensions: Vec<Expression>, // expressionList
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrayReferenceMutability { Readonly, Mutable }

#[derive(Debug, Clone, PartialEq)]
pub enum ArrayReferenceSize {
    List(Vec<Expression>),
    DimEquals(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayReferenceType {
    pub mutability: ArrayReferenceMutability,
    pub element_type: ScalarType,
    pub size: ArrayReferenceSize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Designator {
    pub expr: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassicalType {
    Scalar(ScalarType),
    Array(ArrayType),
}

// ---------- Defcal / extern / gate operands, etc. ----------
#[derive(Debug, Clone, PartialEq)]
pub enum DefcalTarget {
    Measure,
    Reset,
    Delay,
    Identifier(Identifier),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DefcalArgumentDefinition {
    Expr(Expression),
    Arg(ArgumentDefinition),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DefcalOperand {
    HardwareQubit(HardwareQubit),
    Identifier(Identifier),
}

#[derive(Debug, Clone, PartialEq)]
pub enum GateOperand {
    Indexed(IndexedIdentifier),
    HardwareQubit(HardwareQubit),
}

pub type GateOperandList = Vec<GateOperand>;

#[derive(Debug, Clone, PartialEq)]
pub enum ExternArgument {
    Scalar(ScalarType),
    ArrayReference(ArrayReferenceType),
    CReg(Option<Designator>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArgumentDefinition {
    Scalar { ty: ScalarType, name: Identifier },
    Qubit { ty: QubitType, name: Identifier },
    Register { kind: OldStyleRegisterKind, name: Identifier, designator: Option<Designator> }, // CREG | QREG
    ArrayReference { ty: ArrayReferenceType, name: Identifier },
}
