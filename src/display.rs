use crate::ast::*;
use std::fmt::{self, Write};

// Public API retained: delegates to Display
pub fn to_qasm_string(program: &Program) -> String {
    format!("{}", program)
}

struct FmtPrinter<'a, 'b> {
    f: &'a mut fmt::Formatter<'b>,
    indent: usize,
}
impl<'a, 'b> FmtPrinter<'a, 'b> {
    fn new(f: &'a mut fmt::Formatter<'b>) -> Self {
        Self { f, indent: 0 }
    }
    fn write(&mut self, s: &str) -> fmt::Result {
        self.f.write_str(s)
    }
    fn write_char(&mut self, c: char) -> fmt::Result {
        self.f.write_char(c)
    }
    fn write_fmt(&mut self, args: fmt::Arguments) -> fmt::Result {
        self.f.write_fmt(args)
    }
    fn newline(&mut self) -> fmt::Result {
        self.write_char('\n')
    }
    fn indent(&mut self) {
        self.indent += 1;
    }
    fn dedent(&mut self) {
        if self.indent > 0 {
            self.indent -= 1;
        }
    }
    fn write_indent(&mut self) -> fmt::Result {
        for _ in 0..self.indent {
            self.write("    ")?;
        }
        Ok(())
    }

    // --- single-char helpers to improve readability ---
    #[inline]
    fn lbrace(&mut self) -> fmt::Result { self.write_char('{') }
    #[inline]
    fn rbrace(&mut self) -> fmt::Result { self.write_char('}') }
    #[inline]
    fn lbrack(&mut self) -> fmt::Result { self.write_char('[') }
    #[inline]
    fn rbrack(&mut self) -> fmt::Result { self.write_char(']') }
    #[inline]
    fn lparen(&mut self) -> fmt::Result { self.write_char('(') }
    #[inline]
    fn rparen(&mut self) -> fmt::Result { self.write_char(')') }
    #[inline]
    fn semi(&mut self) -> fmt::Result { self.write_char(';') }
    #[inline]
    fn space(&mut self) -> fmt::Result { self.write_char(' ') }

    // Top-level
    fn program(&mut self, pgr: &Program) -> fmt::Result {
        if let Some(v) = &pgr.version {
            self.version(v)?;
        }
        for item in &pgr.items {
            self.statement_or_scope(item)?;
        }
        Ok(())
    }
    fn version(&mut self, v: &Version) -> fmt::Result {
        self.write("OPENQASM ")?;
        self.version_specifier(&v.spec)?;
        self.semi()?;
        self.newline()
    }
    fn version_specifier(&mut self, v: &VersionSpecifier) -> fmt::Result {
        match v {
            (maj, Some(min)) => self.write_fmt(format_args!("{}.{}", maj, min)),
            (maj, None) => self.write_fmt(format_args!("{}", maj)),
        }
    }

    // Statements / scopes
    fn statement_or_scope(&mut self, ss: &StatementOrScope) -> fmt::Result {
        match ss {
            StatementOrScope::Statement(s) => self.statement(s),
            StatementOrScope::Scope(sc) => self.scope(sc),
        }
    }
    fn scope(&mut self, sc: &Scope) -> fmt::Result {
        self.write_indent()?;
        self.lbrace()?; self.newline()?;
        self.indent();
        for item in &sc.items {
            self.statement_or_scope(item)?;
        }
        self.dedent();
        self.write_indent()?;
        self.rbrace()?; self.newline()
    }
    fn statement(&mut self, s: &Statement) -> fmt::Result {
        for ann in &s.annotations {
            self.write_indent()?;
            let (path, kw_line) = &ann.keyword;
            self.write("@")?;
            self.write(&path.join("."))?;
            if !kw_line.is_empty() {
                self.write(" ")?;
                self.write(kw_line)?;
            }
            if let Some(extra) = &ann.content {
                if !extra.is_empty() {
                    self.write(" ")?;
                    self.write(extra)?;
                }
            }
            self.newline()?;
        }
        self.write_indent()?;
        match &s.kind {
            StatementKind::CalibrationGrammar { grammar } => {
                self.write("defcalgrammar ")?;
                self.write(grammar)?;
                self.write(";\n")
            }
            StatementKind::Include { path } => {
                self.write("include ")?;
                self.write(path)?;
                self.write(";\n")
            }
            StatementKind::Break => self.write("break;\n"),
            StatementKind::Continue => self.write("continue;\n"),
            StatementKind::End => self.write("end;\n"),
            StatementKind::For(f) => self.for_stmt(f),
            StatementKind::If(i) => self.if_stmt(i),
            StatementKind::Return(rv) => self.return_stmt(rv.as_ref()),
            StatementKind::While(w) => self.while_stmt(w),
            StatementKind::Switch(sw) => self.switch_stmt(sw),
            StatementKind::Barrier(list) => {
                self.write("barrier")?;
                if let Some(list) = list {
                    self.write(" ")?;
                    self.gate_operand_list(list)?;
                }
                self.write(";\n")
            }
            StatementKind::Box(bx) => self.box_stmt(bx),
            StatementKind::Delay(d) => {
                self.write("delay ")?;
                self.designator(&d.designator)?;
                if let Some(ops) = &d.operands {
                    self.write(" ")?;
                    self.gate_operand_list(ops)?;
                }
                self.write(";\n")
            }
            StatementKind::Nop(list) => {
                self.write("nop")?;
                if let Some(list) = list {
                    self.write(" ")?;
                    self.gate_operand_list(list)?;
                }
                self.write(";\n")
            }
            StatementKind::GateCall(gc) => self.gate_call(gc),
            StatementKind::MeasureArrowAssignment(ma) => {
                self.measure_expression(&ma.measure)?;
                if let Some(target) = &ma.target {
                    self.write(" -> ")?;
                    self.indexed_identifier(target)?;
                }
                self.write(";\n")
            }
            StatementKind::Reset(op) => {
                self.write("reset ")?;
                self.gate_operand(op)?;
                self.write(";\n")
            }
            StatementKind::AliasDeclaration(a) => {
                self.write("let ")?;
                self.write(&a.name)?;
                self.write(" = ")?;
                self.alias_expression(&a.exprs)?;
                self.write(";\n")
            }
            StatementKind::ClassicalDeclaration(c) => {
                self.classical_type(&c.ty)?;
                self.write(" ")?;
                self.write(&c.name)?;
                if let Some(init) = &c.init {
                    self.write(" = ")?;
                    self.declaration_expression(init)?;
                }
                self.write(";\n")
            }
            StatementKind::ConstDeclaration(c) => {
                self.write("const ")?;
                self.scalar_type(&c.ty)?;
                self.write(" ")?;
                self.write(&c.name)?;
                self.write(" = ")?;
                self.declaration_expression(&c.init)?;
                self.write(";\n")
            }
            StatementKind::IoDeclaration(io) => {
                match io.direction {
                    IoDirection::Input => self.write("input ")?,
                    IoDirection::Output => self.write("output ")?,
                }
                self.classical_type(&io.ty)?;
                self.write(" ")?;
                self.write(&io.name)?;
                self.write(";\n")
            }
            StatementKind::OldStyleDeclaration(os) => {
                match os.kind {
                    OldStyleRegisterKind::CReg => self.write("creg ")?,
                    OldStyleRegisterKind::QReg => self.write("qreg ")?,
                }
                self.write(&os.name)?;
                if let Some(d) = &os.designator {
                    self.write(" ")?;
                    self.designator(d)?;
                }
                self.write(";\n")
            }
            StatementKind::QuantumDeclaration(q) => {
                self.qubit_type(&q.ty)?;
                self.write(" ")?;
                self.write(&q.name)?;
                self.write(";\n")
            }
            StatementKind::Def(d) => self.def_stmt(d),
            StatementKind::Extern(e) => self.extern_stmt(e),
            StatementKind::Gate(g) => self.gate_stmt(g),
            StatementKind::Assignment(a) => self.assignment_stmt(a),
            StatementKind::Expression(e) => {
                self.expression(e)?;
                self.write(";\n")
            }
            StatementKind::Cal(c) => {
                self.write("cal ")?; self.lbrace()?;
                if let Some(b) = &c.block {
                    if !b.is_empty() {
                        self.space()?;
                        self.write(b)?;
                        self.space()?;
                    }
                }
                self.rbrace()?; self.newline()
            }
            StatementKind::Defcal(d) => self.defcal_stmt(d),
            StatementKind::Pragma(p) => {
                self.write("pragma ")?;
                self.write(&p.content)?;
                self.newline()
            }
        }
    }

    // Control-flow helpers
    fn for_stmt(&mut self, f: &ForStatement) -> fmt::Result {
        self.write("for ")?;
        self.scalar_type(&f.ty)?;
        self.write(" ")?;
        self.write(&f.var)?;
        self.write(" in ")?;
        match &f.iterable {
            ForIterable::Set(s) => self.set_expression(s)?,
            ForIterable::RangeInBrackets(r) => {
                self.lbrack()?;
                self.range_expression(r)?;
                self.rbrack()?;
            }
            ForIterable::Expr(e) => self.expression(e)?,
        };
        self.write(" ")?;
        self.statement_or_scope(&f.body)
    }
    fn if_stmt(&mut self, i: &IfStatement) -> fmt::Result {
        self.write("if ")?; self.lparen()?;
        self.expression(&i.condition)?;
        self.rparen()?; self.write(" ")?;
        self.statement_or_scope(&i.if_body)?;
        if let Some(e) = &i.else_body {
            self.write_indent()?;
            self.write("else ")?;
            self.statement_or_scope(e)?;
        }
        Ok(())
    }
    fn while_stmt(&mut self, w: &WhileStatement) -> fmt::Result {
        self.write("while ")?; self.lparen()?;
        self.expression(&w.condition)?;
        self.rparen()?; self.write(" ")?;
        self.statement_or_scope(&w.body)
    }
    fn switch_stmt(&mut self, s: &SwitchStatement) -> fmt::Result {
        self.write("switch ")?;
        self.lparen()?; self.expression(&s.expr)?; self.rparen()?; self.space()?; self.lbrace()?; self.newline()?;
        self.indent();
        for item in &s.items {
            self.write_indent()?;
            match item {
                SwitchCaseItem::Case { expressions, scope } => {
                    self.write("case ")?;
                    self.expression_list(expressions)?;
                    self.write(" ")?;
                    self.scope(scope)?;
                }
                SwitchCaseItem::Default { scope } => {
                    self.write("default ")?;
                    self.scope(scope)?;
                }
            }
        }
        self.dedent();
        self.write_indent()?;
        self.rbrace()?; self.newline()
    }
    fn return_stmt(&mut self, rv: Option<&ReturnValue>) -> fmt::Result {
        self.write("return")?;
        if let Some(rv) = rv {
            self.write(" ")?;
            match rv {
                ReturnValue::Expr(e) => self.expression(e)?,
                ReturnValue::Measure(m) => self.measure_expression(m)?,
            }
        }
        self.write(";\n")
    }
    fn box_stmt(&mut self, b: &BoxStatement) -> fmt::Result {
        self.write("box")?;
        if let Some(d) = &b.designator {
            self.write(" ")?;
            self.designator(d)?;
        }
        self.write(" ")?;
        self.scope(&b.body)
    }
    fn gate_call(&mut self, g: &GateCallStatement) -> fmt::Result {
        for m in &g.modifiers {
            self.gate_modifier(m)?;
            self.write(" ")?;
        }
        match &g.target {
            GateTarget::Named(id) => self.write(id)?,
            GateTarget::GPhase => self.write("gphase")?,
        };
        if let Some(params) = &g.parameters {
            self.lparen()?;
            self.expression_list(params)?;
            self.rparen()?;
        }
        if let Some(d) = &g.designator {
            self.write(" ")?;
            self.designator(d)?;
        }
        if let Some(ops) = &g.operands {
            self.write(" ")?;
            self.gate_operand_list(ops)?;
        }
        self.write(";\n")
    }

    // Declarations and helpers
    fn alias_expression(&mut self, a: &AliasExpression) -> fmt::Result {
        for (i, e) in a.parts.iter().enumerate() {
            if i > 0 {
                self.write(" ++ ")?;
            }
            self.expression(e)?;
        }
        Ok(())
    }
    fn declaration_expression(&mut self, d: &DeclarationExpression) -> fmt::Result {
        match d {
            DeclarationExpression::ArrayLiteral(a) => self.array_literal(a),
            DeclarationExpression::Expr(e) => self.expression(e),
            DeclarationExpression::Measure(m) => self.measure_expression(m),
        }
    }
    fn def_stmt(&mut self, d: &DefStatement) -> fmt::Result {
        self.write("def ")?;
        self.write(&d.name)?;
        self.lparen()?;
        self.argument_definition_list(&d.args)?;
        self.rparen()?;
        if let Some(ret) = &d.return_signature {
            self.write(" ")?;
            self.return_signature(ret)?;
        }
        self.write(" ")?;
        self.scope(&d.body)
    }
    fn extern_stmt(&mut self, e: &ExternStatement) -> fmt::Result {
        self.write("extern ")?;
        self.write(&e.name)?;
        self.lparen()?;
        self.extern_argument_list(&e.args)?;
        self.rparen()?;
        if let Some(ret) = &e.return_signature {
            self.write(" ")?;
            self.return_signature(ret)?;
        }
        self.write(";\n")
    }
    fn gate_stmt(&mut self, g: &GateStatement) -> fmt::Result {
        self.write("gate ")?;
        self.write(&g.name)?;
        if let Some(params) = &g.params {
            self.lparen()?;
            self.identifier_list(params)?;
            self.rparen()?;
        }
        self.write(" ")?;
        self.identifier_list(&g.qubits)?;
        self.write(" ")?;
        self.scope(&g.body)
    }
    fn assignment_stmt(&mut self, a: &AssignmentStatement) -> fmt::Result {
        self.indexed_identifier(&a.target)?;
        self.write(" ")?;
        self.assignment_op(&a.op)?;
        self.write(" ")?;
        match &a.value {
            AssignmentRhs::Expr(e) => self.expression(e)?,
            AssignmentRhs::Measure(m) => self.measure_expression(m)?,
        }
        self.write(";\n")
    }

    // Calibration
    fn defcal_stmt(&mut self, d: &DefcalStatement) -> fmt::Result {
        self.write("defcal ")?;
        self.defcal_target(&d.target)?;
        if !d.args.is_empty() {
            self.lparen()?;
            self.defcal_argument_definition_list(&d.args)?;
            self.rparen()?;
        }
        self.write(" ")?;
        self.defcal_operand_list(&d.operands)?;
        if let Some(ret) = &d.return_signature {
            self.write(" ")?;
            self.return_signature(ret)?;
        }
        // Use the same spacing convention as cal: add spaces inside braces only when block is non-empty
        self.space()?; self.lbrace()?;
        if let Some(b) = &d.block {
            if !b.is_empty() {
                self.space()?;
                self.write(b)?;
                self.space()?;
            }
        }
        self.rbrace()?; self.newline()
    }

    // Expressions
    fn expression(&mut self, e: &Expression) -> fmt::Result {
        match e {
            Expression::Parenthesis(inner) => {
                self.lparen()?;
                self.expression(inner)?;
                self.rparen()
            }
            Expression::Index { expr, indices } => {
                self.expression(expr)?;
                for idx in indices {
                    self.index_operator(idx)?;
                }
                Ok(())
            }
            Expression::Unary { op, expr } => {
                self.unary_op(op)?;
                self.expression(expr)
            }
            Expression::Binary { op, lhs, rhs } => {
                self.lparen()?;
                self.expression(lhs)?;
                self.write(" ")?;
                self.binary_op(op)?;
                self.write(" ")?;
                self.expression(rhs)?;
                self.rparen()
            }
            Expression::Cast { ty, expr } => {
                self.classical_type(ty)?;
                self.lparen()?;
                self.expression(expr)?;
                self.rparen()
            }
            Expression::DurationOf(sc) => {
                self.write("durationof")?; self.lparen()?;
                self.scope(sc)?;
                self.rparen()
            }
            Expression::Call { callee, args } => {
                self.write(callee)?;
                self.lparen()?;
                self.expression_list(args)?;
                self.rparen()
            }
            Expression::Literal(l) => self.literal(l),
        }
    }
    fn unary_op(&mut self, op: &UnaryOp) -> fmt::Result {
        match op {
            UnaryOp::BitNot => self.write("~"),
            UnaryOp::LogicalNot => self.write("!"),
            UnaryOp::Negate => self.write("-"),
        }
    }
    fn binary_op(&mut self, op: &BinaryOp) -> fmt::Result {
        match op {
            BinaryOp::Power => self.write("**"),
            BinaryOp::Multiply => self.write("*"),
            BinaryOp::Divide => self.write("/"),
            BinaryOp::Modulo => self.write("%"),
            BinaryOp::Add => self.write("+"),
            BinaryOp::Subtract => self.write("-"),
            BinaryOp::ShiftLeft => self.write("<<"),
            BinaryOp::ShiftRight => self.write(">>"),
            BinaryOp::Less => self.write("<"),
            BinaryOp::LessEqual => self.write("<="),
            BinaryOp::Greater => self.write(">"),
            BinaryOp::GreaterEqual => self.write(">="),
            BinaryOp::Equal => self.write("=="),
            BinaryOp::NotEqual => self.write("!="),
            BinaryOp::BitAnd => self.write("&"),
            BinaryOp::BitXor => self.write("^"),
            BinaryOp::BitOr => self.write("|"),
            BinaryOp::LogicalAnd => self.write("&&"),
            BinaryOp::LogicalOr => self.write("||"),
        }
    }
    fn literal(&mut self, l: &Literal) -> fmt::Result {
        match l {
            Literal::Identifier(id) => self.write(id),
            Literal::Integer(i) => self.write_fmt(format_args!("{}", i)),
            Literal::Float(f) => self.write_fmt(format_args!("{}", f)),
            Literal::Number(n) => match n {
                Number::Integer(i) => self.write_fmt(format_args!("{}", i)),
                Number::Float(f) => self.write_fmt(format_args!("{}", f)),
            },
            Literal::Boolean(b) => self.write(if *b { "true" } else { "false" }),
            Literal::Bitstring(bits) => self.write(bits),
            Literal::Timing(n, u) => {
                self.number(n)?;
                self.write(" ")?;
                self.time_unit(u)
            }
            Literal::HardwareQubit(q) => {
                self.write("$")?;
                self.write_fmt(format_args!("{}", q))
            }
        }
    }
    fn number(&mut self, n: &Number) -> fmt::Result {
        match n {
            Number::Integer(i) => self.write_fmt(format_args!("{}", i)),
            Number::Float(f) => self.write_fmt(format_args!("{}", f)),
        }
    }
    fn time_unit(&mut self, u: &TimeUnit) -> fmt::Result {
        let s = match u {
            TimeUnit::Dt => "dt",
            TimeUnit::Ns => "ns",
            TimeUnit::Us => "us",
            TimeUnit::Ms => "ms",
            TimeUnit::S => "s",
        };
        self.write(s)
    }
    fn index_operator(&mut self, op: &IndexOperator) -> fmt::Result {
        self.lbrack()?;
        match op {
            IndexOperator::Set(s) => self.set_expression(s)?,
            IndexOperator::Items(items) => {
                for (i, it) in items.iter().enumerate() {
                    if i > 0 {
                        self.write(", ")?;
                    }
                    match it {
                        IndexItem::Expr(e) => self.expression(e)?,
                        IndexItem::Range(r) => self.range_expression(r)?,
                    }
                }
            }
        }
        self.rbrack()
    }
    fn range_expression(&mut self, r: &RangeExpression) -> fmt::Result {
        if let Some(s) = &r.start {
            self.expression(s)?;
        }
        self.write(":")?;
        if let Some(e) = &r.end {
            self.expression(e)?;
        }
        if let Some(st) = &r.step {
            self.write(":")?;
            self.expression(st)?;
        }
        Ok(())
    }
    fn set_expression(&mut self, s: &SetExpression) -> fmt::Result {
        self.lbrace()?;
        for (i, e) in s.elements.iter().enumerate() {
            if i > 0 {
                self.write(", ")?;
            }
            self.expression(e)?;
        }
        self.rbrace()
    }
    fn array_literal(&mut self, a: &ArrayLiteral) -> fmt::Result {
        self.lbrace()?;
        for (i, it) in a.elements.iter().enumerate() {
            if i > 0 {
                self.write(", ")?;
            }
            match it {
                ArrayLiteralItem::Expr(e) => self.expression(e)?,
                ArrayLiteralItem::Array(ar) => self.array_literal(ar)?,
            }
        }
        self.rbrace()
    }
    fn indexed_identifier(&mut self, ii: &IndexedIdentifier) -> fmt::Result {
        self.write(&ii.name)?;
        for idx in &ii.indices {
            self.index_operator(idx)?;
        }
        Ok(())
    }
    fn expression_list(&mut self, list: &[Expression]) -> fmt::Result {
        for (i, e) in list.iter().enumerate() {
            if i > 0 {
                self.write(", ")?;
            }
            self.expression(e)?;
        }
        Ok(())
    }

    // Types
    fn return_signature(&mut self, r: &ReturnSignature) -> fmt::Result {
        self.write("-> ")?;
        self.scalar_type(&r.ty)
    }
    fn gate_modifier(&mut self, m: &GateModifier) -> fmt::Result {
        match m {
            GateModifier::Inv => self.write("inv @"),
            GateModifier::Pow(e) => {
                self.write("pow")?; self.lparen()?;
                self.expression(e)?;
                self.rparen()?; self.write(" @")
            }
            GateModifier::Ctrl(e) => {
                self.write("ctrl")?;
                if let Some(e) = e {
                    self.lparen()?;
                    self.expression(e)?;
                    self.rparen()?;
                }
                self.write(" @")
            }
            GateModifier::NegCtrl(e) => {
                self.write("negctrl")?;
                if let Some(e) = e {
                    self.lparen()?;
                    self.expression(e)?;
                    self.rparen()?;
                }
                self.write(" @")
            }
        }
    }
    fn scalar_type(&mut self, t: &ScalarType) -> fmt::Result {
        match t {
            ScalarType::Bit { designator } => {
                self.write("bit")?;
                if let Some(d) = designator {
                    self.write(" ")?;
                    self.designator(d)?;
                }
                Ok(())
            }
            ScalarType::Int { designator } => {
                self.write("int")?;
                if let Some(d) = designator {
                    self.write(" ")?;
                    self.designator(d)?;
                }
                Ok(())
            }
            ScalarType::UInt { designator } => {
                self.write("uint")?;
                if let Some(d) = designator {
                    self.write(" ")?;
                    self.designator(d)?;
                }
                Ok(())
            }
            ScalarType::Float { designator } => {
                self.write("float")?;
                if let Some(d) = designator {
                    self.write(" ")?;
                    self.designator(d)?;
                }
                Ok(())
            }
            ScalarType::Angle { designator } => {
                self.write("angle")?;
                if let Some(d) = designator {
                    self.write(" ")?;
                    self.designator(d)?;
                }
                Ok(())
            }
            ScalarType::Bool => self.write("bool"),
            ScalarType::Duration => self.write("duration"),
            ScalarType::Stretch => self.write("stretch"),
            ScalarType::Complex { inner } => {
                self.write("complex")?;
                if let Some(inner) = inner {
                    self.lbrack()?;
                    self.scalar_type(inner)?;
                    self.rbrack()?;
                }
                Ok(())
            }
        }
    }
    fn qubit_type(&mut self, q: &QubitType) -> fmt::Result {
        self.write("qubit")?;
        if let Some(d) = &q.designator {
            self.write(" ")?;
            self.designator(d)?;
        }
        Ok(())
    }
    fn array_type(&mut self, a: &ArrayType) -> fmt::Result {
        self.write("array")?; self.lbrack()?;
        self.scalar_type(&a.element_type)?;
        self.write(", ")?;
        self.expression_list(&a.dimensions)?;
        self.rbrack()
    }
    fn array_ref_type(&mut self, a: &ArrayReferenceType) -> fmt::Result {
        match a.mutability {
            ArrayReferenceMutability::Readonly => self.write("readonly ")?,
            ArrayReferenceMutability::Mutable => self.write("mutable ")?,
        }
        self.write("array")?; self.lbrack()?;
        self.scalar_type(&a.element_type)?;
        self.write(", ")?;
        match &a.size {
            ArrayReferenceSize::List(list) => self.expression_list(list)?,
            ArrayReferenceSize::DimEquals(e) => {
                self.write("#dim = ")?;
                self.expression(e)?;
            }
        }
        self.rbrack()
    }
    fn designator(&mut self, d: &Designator) -> fmt::Result {
        self.lbrack()?;
        self.expression(&d.expr)?;
        self.rbrack()
    }
    fn classical_type(&mut self, t: &ClassicalType) -> fmt::Result {
        match t {
            ClassicalType::Scalar(s) => self.scalar_type(s),
            ClassicalType::Array(a) => self.array_type(a),
        }
    }

    // Gate / extern / defcal operands and args
    fn gate_operand(&mut self, o: &GateOperand) -> fmt::Result {
        match o {
            GateOperand::Indexed(i) => self.indexed_identifier(i),
            GateOperand::HardwareQubit(q) => {
                self.write("$")?;
                self.write_fmt(format_args!("{}", q))
            }
        }
    }
    fn gate_operand_list(&mut self, list: &GateOperandList) -> fmt::Result {
        for (i, o) in list.iter().enumerate() {
            if i > 0 {
                self.write(", ")?;
            }
            self.gate_operand(o)?;
        }
        Ok(())
    }
    fn extern_argument(&mut self, a: &ExternArgument) -> fmt::Result {
        match a {
            ExternArgument::Scalar(s) => self.scalar_type(s),
            ExternArgument::ArrayReference(ar) => self.array_ref_type(ar),
            ExternArgument::CReg(d) => {
                self.write("creg")?;
                if let Some(des) = d {
                    self.write(" ")?;
                    self.designator(des)?;
                }
                Ok(())
            }
        }
    }
    fn extern_argument_list(&mut self, list: &[ExternArgument]) -> fmt::Result {
        for (i, a) in list.iter().enumerate() {
            if i > 0 {
                self.write(", ")?;
            }
            self.extern_argument(a)?;
        }
        Ok(())
    }
    fn argument_definition(&mut self, a: &ArgumentDefinition) -> fmt::Result {
        match a {
            ArgumentDefinition::Scalar { ty, name } => {
                self.scalar_type(ty)?;
                self.write(" ")?;
                self.write(name)
            }
            ArgumentDefinition::Qubit { ty, name } => {
                self.qubit_type(ty)?;
                self.write(" ")?;
                self.write(name)
            }
            ArgumentDefinition::Register {
                kind,
                name,
                designator,
            } => {
                match kind {
                    OldStyleRegisterKind::CReg => self.write("creg")?,
                    OldStyleRegisterKind::QReg => self.write("qreg")?,
                }
                if let Some(d) = designator {
                    self.write(" ")?;
                    self.designator(d)?;
                }
                self.write(" ")?;
                self.write(name)
            }
            ArgumentDefinition::ArrayReference { ty, name } => {
                self.array_ref_type(ty)?;
                self.write(" ")?;
                self.write(name)
            }
        }
    }
    fn argument_definition_list(&mut self, list: &[ArgumentDefinition]) -> fmt::Result {
        for (i, a) in list.iter().enumerate() {
            if i > 0 {
                self.write(", ")?;
            }
            self.argument_definition(a)?;
        }
        Ok(())
    }
    fn defcal_target(&mut self, t: &DefcalTarget) -> fmt::Result {
        match t {
            DefcalTarget::Measure => self.write("measure"),
            DefcalTarget::Reset => self.write("reset"),
            DefcalTarget::Delay => self.write("delay"),
            DefcalTarget::Identifier(id) => self.write(id),
        }
    }
    fn defcal_argument_definition(&mut self, a: &DefcalArgumentDefinition) -> fmt::Result {
        match a {
            DefcalArgumentDefinition::Expr(e) => self.expression(e),
            DefcalArgumentDefinition::Arg(ad) => self.argument_definition(ad),
        }
    }
    fn defcal_argument_definition_list(
        &mut self,
        list: &[DefcalArgumentDefinition],
    ) -> fmt::Result {
        for (i, a) in list.iter().enumerate() {
            if i > 0 {
                self.write(", ")?;
            }
            self.defcal_argument_definition(a)?;
        }
        Ok(())
    }
    fn defcal_operand(&mut self, o: &DefcalOperand) -> fmt::Result {
        match o {
            DefcalOperand::HardwareQubit(q) => {
                self.write("$")?;
                self.write_fmt(format_args!("{}", q))
            }
            DefcalOperand::Identifier(id) => self.write(id),
        }
    }
    fn defcal_operand_list(&mut self, list: &[DefcalOperand]) -> fmt::Result {
        for (i, o) in list.iter().enumerate() {
            if i > 0 {
                self.write(", ")?;
            }
            self.defcal_operand(o)?;
        }
        Ok(())
    }
    fn identifier_list(&mut self, list: &[Identifier]) -> fmt::Result {
        for (i, id) in list.iter().enumerate() {
            if i > 0 {
                self.write(", ")?;
            }
            self.write(id)?;
        }
        Ok(())
    }
    fn assignment_op(&mut self, op: &AssignmentOp) -> fmt::Result {
        let s = match op {
            AssignmentOp::Assign => "=",
            AssignmentOp::AddAssign => "+=",
            AssignmentOp::SubAssign => "-=",
            AssignmentOp::MulAssign => "*=",
            AssignmentOp::DivAssign => "/=",
            AssignmentOp::RemAssign => "%=",
            AssignmentOp::BitAndAssign => "&=",
            AssignmentOp::BitOrAssign => "|=",
            AssignmentOp::BitXorAssign => "^=",
            AssignmentOp::ShlAssign => "<<=",
            AssignmentOp::ShrAssign => ">>=",
            AssignmentOp::PowAssign => "**=",
        };
        self.write(s)
    }
    fn measure_expression(&mut self, m: &MeasureExpression) -> fmt::Result {
        self.write("measure ")?;
        self.gate_operand(&m.operand)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.program(self)
    }
}

// ---- Display impls for all AST nodes ----

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.version(self)
    }
}

impl fmt::Display for StatementOrScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.statement_or_scope(self)
    }
}

impl fmt::Display for Scope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.scope(self)
    }
}

impl fmt::Display for Annotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Single-line annotation (with trailing newline)
        let mut p = FmtPrinter::new(f);
        p.write("@")?;
        p.write(&self.keyword.0.join("."))?;
        if !self.keyword.1.is_empty() {
            p.write(" ")?;
            p.write(&self.keyword.1)?;
        }
        if let Some(extra) = &self.content {
            if !extra.is_empty() {
                p.write(" ")?;
                p.write(extra)?;
            }
        }
        p.newline()
    }
}

impl fmt::Display for Pragma {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.write("pragma ")?;
        p.write(&self.content)?;
        p.newline()
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.statement(self)
    }
}

impl fmt::Display for ForStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.for_stmt(self)
    }
}

impl fmt::Display for IfStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.if_stmt(self)
    }
}

impl fmt::Display for WhileStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.while_stmt(self)
    }
}

impl fmt::Display for SwitchStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.switch_stmt(self)
    }
}

impl fmt::Display for SwitchCaseItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Render as in switch body (includes nested scope)
        let mut p = FmtPrinter::new(f);
        match self {
            SwitchCaseItem::Case { expressions, scope } => {
                p.write("case ")?;
                p.expression_list(expressions)?;
                p.write(" ")?;
                p.scope(scope)
            }
            SwitchCaseItem::Default { scope } => {
                p.write("default ")?;
                p.scope(scope)
            }
        }
    }
}

impl fmt::Display for BoxStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.box_stmt(self)
    }
}

impl fmt::Display for DelayStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.write("delay ")?;
        p.designator(&self.designator)?;
        if let Some(ops) = &self.operands {
            p.write(" ")?;
            p.gate_operand_list(ops)?;
        }
        p.write(";\n")
    }
}

impl fmt::Display for GateCallStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.gate_call(self)
    }
}

impl fmt::Display for GateTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GateTarget::Named(id) => f.write_str(id),
            GateTarget::GPhase => f.write_str("gphase"),
        }
    }
}

impl fmt::Display for MeasureArrowAssignmentStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.measure_expression(&self.measure)?;
        if let Some(target) = &self.target {
            p.write(" -> ")?;
            p.indexed_identifier(target)?;
        }
        p.write(";\n")
    }
}

impl fmt::Display for AliasDeclarationStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.write("let ")?;
        p.write(&self.name)?;
        p.write(" = ")?;
        p.alias_expression(&self.exprs)?;
        p.write(";\n")
    }
}

impl fmt::Display for ClassicalDeclarationStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.classical_type(&self.ty)?;
        p.write(" ")?;
        p.write(&self.name)?;
        if let Some(init) = &self.init {
            p.write(" = ")?;
            p.declaration_expression(init)?;
        }
        p.write(";\n")
    }
}

impl fmt::Display for ConstDeclarationStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.write("const ")?;
        p.scalar_type(&self.ty)?;
        p.write(" ")?;
        p.write(&self.name)?;
        p.write(" = ")?;
        p.declaration_expression(&self.init)?;
        p.write(";\n")
    }
}

impl fmt::Display for IoDeclarationStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.direction {
            IoDirection::Input => write!(f, "input "),
            IoDirection::Output => write!(f, "output "),
        }?;
        let mut p = FmtPrinter::new(f);
        p.classical_type(&self.ty)?;
        p.write(" ")?;
        p.write(&self.name)?;
        p.write(";\n")
    }
}

impl fmt::Display for OldStyleDeclarationStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            OldStyleRegisterKind::CReg => write!(f, "creg "),
            OldStyleRegisterKind::QReg => write!(f, "qreg "),
        }?;
        let mut p = FmtPrinter::new(f);
        p.write(&self.name)?;
        if let Some(d) = &self.designator {
            p.write(" ")?;
            p.designator(d)?;
        }
        p.write(";\n")
    }
}

impl fmt::Display for QuantumDeclarationStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.qubit_type(&self.ty)?;
        p.write(" ")?;
        p.write(&self.name)?;
        p.write(";\n")
    }
}

impl fmt::Display for DefStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.def_stmt(self)
    }
}

impl fmt::Display for ExternStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.extern_stmt(self)
    }
}

impl fmt::Display for GateStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.gate_stmt(self)
    }
}

impl fmt::Display for AssignmentStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.assignment_stmt(self)
    }
}

impl fmt::Display for CalStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(b) = &self.block {
            write!(f, "cal {{ {} }}\n", b)
        } else {
            write!(f, "cal {{ }}\n")
        }
    }
}

impl fmt::Display for DefcalStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.defcal_stmt(self)
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        p.expression(self)
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).unary_op(self)
    }
}
impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).binary_op(self)
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).literal(self)
    }
}
impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).number(self)
    }
}
impl fmt::Display for TimeUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).time_unit(self)
    }
}

impl fmt::Display for IndexOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).index_operator(self)
    }
}
impl fmt::Display for IndexItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IndexItem::Expr(e) => write!(f, "{}", e),
            IndexItem::Range(r) => write!(f, "{}", r),
        }
    }
}

impl fmt::Display for AliasExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).alias_expression(self)
    }
}
impl fmt::Display for DeclarationExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).declaration_expression(self)
    }
}
impl fmt::Display for MeasureExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).measure_expression(self)
    }
}
impl fmt::Display for RangeExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).range_expression(self)
    }
}
impl fmt::Display for SetExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).set_expression(self)
    }
}
impl fmt::Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).array_literal(self)
    }
}
impl fmt::Display for ArrayLiteralItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArrayLiteralItem::Expr(e) => write!(f, "{}", e),
            ArrayLiteralItem::Array(a) => write!(f, "{}", a),
        }
    }
}

impl fmt::Display for IndexedIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).indexed_identifier(self)
    }
}

impl fmt::Display for ReturnSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).return_signature(self)
    }
}
impl fmt::Display for GateModifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).gate_modifier(self)
    }
}

impl fmt::Display for ScalarType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).scalar_type(self)
    }
}
impl fmt::Display for QubitType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).qubit_type(self)
    }
}
impl fmt::Display for ArrayType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).array_type(self)
    }
}
impl fmt::Display for ArrayReferenceType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).array_ref_type(self)
    }
}

impl fmt::Display for Designator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).designator(self)
    }
}
impl fmt::Display for ClassicalType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).classical_type(self)
    }
}

impl fmt::Display for DefcalTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).defcal_target(self)
    }
}
impl fmt::Display for DefcalArgumentDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).defcal_argument_definition(self)
    }
}
impl fmt::Display for DefcalOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).defcal_operand(self)
    }
}

impl fmt::Display for GateOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).gate_operand(self)
    }
}
impl fmt::Display for ExternArgument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).extern_argument(self)
    }
}
impl fmt::Display for ArgumentDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).argument_definition(self)
    }
}

impl fmt::Display for AssignmentOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FmtPrinter::new(f).assignment_op(self)
    }
}
impl fmt::Display for AssignmentRhs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AssignmentRhs::Expr(e) => write!(f, "{}", e),
            AssignmentRhs::Measure(m) => write!(f, "{}", m),
        }
    }
}
impl fmt::Display for ReturnValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReturnValue::Expr(e) => write!(f, "{}", e),
            ReturnValue::Measure(m) => write!(f, "{}", m),
        }
    }
}

impl fmt::Display for IoDirection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IoDirection::Input => f.write_str("input"),
            IoDirection::Output => f.write_str("output"),
        }
    }
}
impl fmt::Display for OldStyleRegisterKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OldStyleRegisterKind::CReg => f.write_str("creg"),
            OldStyleRegisterKind::QReg => f.write_str("qreg"),
        }
    }
}
impl fmt::Display for ArrayReferenceMutability {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArrayReferenceMutability::Readonly => f.write_str("readonly"),
            ArrayReferenceMutability::Mutable => f.write_str("mutable"),
        }
    }
}
impl fmt::Display for ArrayReferenceSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArrayReferenceSize::List(list) => {
                let mut p = FmtPrinter::new(f);
                p.expression_list(list)
            }
            ArrayReferenceSize::DimEquals(e) => write!(f, "#dim = {}", e),
        }
    }
}

impl fmt::Display for ForIterable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut p = FmtPrinter::new(f);
        match self {
            ForIterable::Set(s) => p.set_expression(s),
            ForIterable::RangeInBrackets(r) => {
                p.lbrack()?;
                p.range_expression(r)?;
                p.rbrack()
            }
            ForIterable::Expr(e) => p.expression(e),
        }
    }
}

impl fmt::Display for CalibrationGrammarStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "defcalgrammar {}\n", self.grammar)
    }
}

impl fmt::Display for IncludeStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "include {}\n", self.path)
    }
}

impl fmt::Display for StatementKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Reuse Statement's Display to keep formatting consistent
        let stmt = Statement {
            annotations: vec![],
            kind: self.clone(),
        };
        write!(f, "{}", stmt)
    }
}
