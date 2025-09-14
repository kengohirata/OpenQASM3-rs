use crate::ast::*;
use std::fmt::{self, Write};

// Public API: pretty-print the whole program to a String
pub fn to_qasm_string(program: &Program) -> String {
    let mut p = Printer::new();
    p.program(program);
    p.finish()
}

struct Printer {
    out: String,
    indent: usize,
}

impl Printer {
    fn new() -> Self {
        Self { out: String::new(), indent: 0 }
    }

    fn finish(self) -> String { self.out }

    fn write<S: AsRef<str>>(&mut self, s: S) { let _ = self.out.write_str(s.as_ref()); }
    fn write_char(&mut self, c: char) { let _ = self.out.write_char(c); }
    fn write_fmt(&mut self, args: fmt::Arguments) { let _ = self.out.write_fmt(args); }

    fn newline(&mut self) {
        self.write_char('\n');
    }

    fn indent(&mut self) { self.indent += 1; }
    fn dedent(&mut self) { if self.indent > 0 { self.indent -= 1; } }
    fn write_indent(&mut self) {
        for _ in 0..self.indent { self.write("    "); }
    }

    // ----- Top-level -----
    fn program(&mut self, pgr: &Program) {
        if let Some(v) = &pgr.version {
            self.version(v);
        }
        for item in &pgr.items {
            self.statement_or_scope(item);
        }
    }

    fn version(&mut self, v: &Version) {
        self.write("OPENQASM ");
        self.version_specifier(&v.spec);
        self.write(";");
        self.newline();
    }

    fn version_specifier(&mut self, v: &VersionSpecifier) {
        match v {
            (maj, Some(min)) => self.write_fmt(format_args!("{}.{}", maj, min)),
            (maj, None) => self.write_fmt(format_args!("{}", maj)),
        }
    }

    // ----- Statements / Scopes -----
    fn statement_or_scope(&mut self, ss: &StatementOrScope) {
        match ss {
            StatementOrScope::Statement(s) => self.statement(s),
            StatementOrScope::Scope(sc) => self.scope(sc),
        }
    }

    fn scope(&mut self, sc: &Scope) {
        self.write_indent();
        self.write("{\n");
        self.indent();
        for item in &sc.items {
            self.statement_or_scope(item);
        }
        self.dedent();
        self.write_indent();
        self.write("}\n");
    }

    fn statement(&mut self, s: &Statement) {
        // Print annotations (best-effort given current AST shape)
        for ann in &s.annotations {
            self.write_indent();
            // AnnotationKeyword is (Vec<Identifier>, RemainingLineContent)
            let (path, kw_line) = &ann.keyword;
            self.write("@");
            self.write(&path.join("."));
            if !kw_line.is_empty() { self.write(" "); self.write(kw_line); }
            if let Some(extra) = &ann.content {
                if !extra.is_empty() { self.write(" "); self.write(extra); }
            }
            self.newline();
        }

        self.write_indent();
        match &s.kind {
            // Inclusion
            StatementKind::CalibrationGrammar { grammar } => {
                self.write("defcalgrammar ");
                self.write(grammar);
                self.write(";\n");
            }
            StatementKind::Include { path } => {
                self.write("include ");
                self.write(path);
                self.write(";\n");
            }

            // Control-flow
            StatementKind::Break => { self.write("break;\n"); }
            StatementKind::Continue => { self.write("continue;\n"); }
            StatementKind::End => { self.write("end;\n"); }
            StatementKind::For(f) => { self.for_stmt(f); }
            StatementKind::If(i) => { self.if_stmt(i); }
            StatementKind::Return(rv) => { self.return_stmt(rv.as_ref()); }
            StatementKind::While(w) => { self.while_stmt(w); }
            StatementKind::Switch(sw) => { self.switch_stmt(sw); }

            // Quantum directives
            StatementKind::Barrier(list) => {
                self.write("barrier");
                if let Some(list) = list { self.write(" "); self.gate_operand_list(list); }
                self.write(";\n");
            }
            StatementKind::Box(bx) => { self.box_stmt(bx); }
            StatementKind::Delay(d) => {
                self.write("delay ");
                self.designator(&d.designator);
                if let Some(ops) = &d.operands { self.write(" "); self.gate_operand_list(ops); }
                self.write(";\n");
            }
            StatementKind::Nop(list) => {
                self.write("nop");
                if let Some(list) = list { self.write(" "); self.gate_operand_list(list); }
                self.write(";\n");
            }
            StatementKind::GateCall(gc) => { self.gate_call(gc); }
            StatementKind::MeasureArrowAssignment(ma) => {
                self.measure_expression(&ma.measure);
                if let Some(target) = &ma.target {
                    self.write(" -> ");
                    self.indexed_identifier(target);
                }
                self.write(";\n");
            }
            StatementKind::Reset(op) => {
                self.write("reset ");
                self.gate_operand(op);
                self.write(";\n");
            }

            // Declarations
            StatementKind::AliasDeclaration(a) => {
                self.write("let ");
                self.write(&a.name);
                self.write(" = ");
                self.alias_expression(&a.exprs);
                self.write(";\n");
            }
            StatementKind::ClassicalDeclaration(c) => {
                self.classical_type(&c.ty);
                self.write(" ");
                self.write(&c.name);
                if let Some(init) = &c.init { self.write(" = "); self.declaration_expression(init); }
                self.write(";\n");
            }
            StatementKind::ConstDeclaration(c) => {
                self.write("const ");
                self.scalar_type(&c.ty);
                self.write(" ");
                self.write(&c.name);
                self.write(" = ");
                self.declaration_expression(&c.init);
                self.write(";\n");
            }
            StatementKind::IoDeclaration(io) => {
                match io.direction { IoDirection::Input => self.write("input "), IoDirection::Output => self.write("output ") }
                self.classical_type(&io.ty);
                self.write(" ");
                self.write(&io.name);
                self.write(";\n");
            }
            StatementKind::OldStyleDeclaration(os) => {
                match os.kind { OldStyleRegisterKind::CReg => self.write("creg "), OldStyleRegisterKind::QReg => self.write("qreg ") }
                self.write(&os.name);
                if let Some(d) = &os.designator { self.write(" "); self.designator(d); }
                self.write(";\n");
            }
            StatementKind::QuantumDeclaration(q) => {
                self.qubit_type(&q.ty);
                self.write(" ");
                self.write(&q.name);
                self.write(";\n");
            }

            // Higher-order
            StatementKind::Def(d) => { self.def_stmt(d); }
            StatementKind::Extern(e) => { self.extern_stmt(e); }
            StatementKind::Gate(g) => { self.gate_stmt(g); }

            // Non-declaration calculations
            StatementKind::Assignment(a) => { self.assignment_stmt(a); }
            StatementKind::Expression(e) => { self.expression(e); self.write(";\n"); }

            // Calibration
            StatementKind::Cal(c) => {
                self.write("cal {");
                if let Some(b) = &c.block { if !b.is_empty() { self.write(" "); self.write(b); self.write(" "); } }
                self.write("}\n");
            }
            StatementKind::Defcal(d) => { self.defcal_stmt(d); }

            // Direct pragma
            StatementKind::Pragma(p) => {
                self.write("pragma ");
                self.write(&p.content);
                self.newline();
            }
        }
    }

    // ----- Control flow helpers -----
    fn for_stmt(&mut self, f: &ForStatement) {
        self.write("for ");
        self.scalar_type(&f.ty);
        self.write(" ");
        self.write(&f.var);
        self.write(" in ");
        match &f.iterable {
            ForIterable::Set(s) => self.set_expression(s),
            ForIterable::RangeInBrackets(r) => { self.write("["); self.range_expression(r); self.write("]"); }
            ForIterable::Expr(e) => self.expression(e),
        }
        self.write(" ");
        self.statement_or_scope(&f.body);
    }

    fn if_stmt(&mut self, i: &IfStatement) {
        self.write("if (");
        self.expression(&i.condition);
        self.write(") ");
        self.statement_or_scope(&i.if_body);
        if let Some(e) = &i.else_body { self.write_indent(); self.write("else "); self.statement_or_scope(e); }
    }

    fn while_stmt(&mut self, w: &WhileStatement) {
        self.write("while (");
        self.expression(&w.condition);
        self.write(") ");
        self.statement_or_scope(&w.body);
    }

    fn switch_stmt(&mut self, s: &SwitchStatement) {
        self.write("switch (");
        self.expression(&s.expr);
        self.write(") {\n");
        self.indent();
        for item in &s.items {
            self.write_indent();
            match item {
                SwitchCaseItem::Case { expressions, scope } => {
                    self.write("case ");
                    self.expression_list(expressions);
                    self.write(" ");
                    self.scope(scope);
                }
                SwitchCaseItem::Default { scope } => {
                    self.write("default ");
                    self.scope(scope);
                }
            }
        }
        self.dedent();
        self.write_indent();
        self.write("}\n");
    }

    fn return_stmt(&mut self, rv: Option<&ReturnValue>) {
        self.write("return");
        if let Some(rv) = rv {
            self.write(" ");
            match rv {
                ReturnValue::Expr(e) => self.expression(e),
                ReturnValue::Measure(m) => self.measure_expression(m),
            }
        }
        self.write(";\n");
    }

    fn box_stmt(&mut self, b: &BoxStatement) {
        self.write("box");
        if let Some(d) = &b.designator { self.write(" "); self.designator(d); }
        self.write(" ");
        self.scope(&b.body);
    }

    fn gate_call(&mut self, g: &GateCallStatement) {
        for m in &g.modifiers { self.gate_modifier(m); self.write(" "); }
        match &g.target { GateTarget::Named(id) => self.write(id), GateTarget::GPhase => self.write("gphase") }
        if let Some(params) = &g.parameters { self.write("("); self.expression_list(params); self.write(")"); }
        if let Some(d) = &g.designator { self.write(" "); self.designator(d); }
        if let Some(ops) = &g.operands { self.write(" "); self.gate_operand_list(ops); }
        self.write(";\n");
    }

    // ----- Declarations -----
    fn alias_expression(&mut self, a: &AliasExpression) {
        for (i, e) in a.parts.iter().enumerate() {
            if i > 0 { self.write(" ++ "); }
            self.expression(e);
        }
    }

    fn declaration_expression(&mut self, d: &DeclarationExpression) {
        match d {
            DeclarationExpression::ArrayLiteral(a) => self.array_literal(a),
            DeclarationExpression::Expr(e) => self.expression(e),
            DeclarationExpression::Measure(m) => self.measure_expression(m),
        }
    }

    fn def_stmt(&mut self, d: &DefStatement) {
        self.write("def ");
        self.write(&d.name);
        self.write("(");
        self.argument_definition_list(&d.args);
        self.write(")");
        if let Some(ret) = &d.return_signature { self.write(" "); self.return_signature(ret); }
        self.write(" ");
        self.scope(&d.body);
    }

    fn extern_stmt(&mut self, e: &ExternStatement) {
        self.write("extern ");
        self.write(&e.name);
        self.write("(");
        self.extern_argument_list(&e.args);
        self.write(")");
        if let Some(ret) = &e.return_signature { self.write(" "); self.return_signature(ret); }
        self.write(";\n");
    }

    fn gate_stmt(&mut self, g: &GateStatement) {
        self.write("gate ");
        self.write(&g.name);
        if let Some(params) = &g.params { self.write("("); self.identifier_list(params); self.write(")"); }
        self.write(" ");
        self.identifier_list(&g.qubits);
        self.write(" ");
        self.scope(&g.body);
    }

    fn assignment_stmt(&mut self, a: &AssignmentStatement) {
        self.indexed_identifier(&a.target);
        self.write(" ");
        self.assignment_op(&a.op);
        self.write(" ");
        match &a.value {
            AssignmentRhs::Expr(e) => self.expression(e),
            AssignmentRhs::Measure(m) => self.measure_expression(m),
        }
        self.write(";\n");
    }

    // ----- Calibration -----
    fn defcal_stmt(&mut self, d: &DefcalStatement) {
        self.write("defcal ");
        self.defcal_target(&d.target);
        if !d.args.is_empty() {
            self.write("(");
            self.defcal_argument_definition_list(&d.args);
            self.write(")");
        }
        self.write(" ");
        self.defcal_operand_list(&d.operands);
        if let Some(ret) = &d.return_signature { self.write(" "); self.return_signature(ret); }
        self.write(" { ");
        if let Some(b) = &d.block { self.write(b); }
        self.write(" }\n");
    }

    // ----- Expressions -----
    fn expression(&mut self, e: &Expression) {
        match e {
            Expression::Parenthesis(inner) => { self.write("("); self.expression(inner); self.write(")"); }
            Expression::Index { expr, indices } => {
                self.expression(expr);
                for idx in indices { self.index_operator(idx); }
            }
            Expression::Unary { op, expr } => {
                self.unary_op(op);
                self.expression(expr);
            }
            Expression::Binary { op, lhs, rhs } => {
                // Add parentheses to preserve correctness regardless of precedence
                self.write("(");
                self.expression(lhs);
                self.write(" ");
                self.binary_op(op);
                self.write(" ");
                self.expression(rhs);
                self.write(")");
            }
            Expression::Cast { ty, expr } => {
                self.classical_type(ty);
                self.write("(");
                self.expression(expr);
                self.write(")");
            }
            Expression::DurationOf(sc) => {
                self.write("durationof(");
                self.scope(sc);
                self.write(")");
            }
            Expression::Call { callee, args } => {
                self.write(callee);
                self.write("(");
                self.expression_list(args);
                self.write(")");
            }
            Expression::Literal(l) => self.literal(l),
        }
    }

    fn unary_op(&mut self, op: &UnaryOp) {
        match op {
            UnaryOp::BitNot => self.write("~"),
            UnaryOp::LogicalNot => self.write("!"),
            UnaryOp::Negate => self.write("-"),
        }
    }

    fn binary_op(&mut self, op: &BinaryOp) {
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

    fn literal(&mut self, l: &Literal) {
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
            Literal::Timing(n, u) => { self.number(n); self.write(" "); self.time_unit(u); }
            Literal::HardwareQubit(q) => { self.write("$"); self.write_fmt(format_args!("{}", q)); }
        }
    }

    fn number(&mut self, n: &Number) {
        match n { Number::Integer(i) => self.write_fmt(format_args!("{}", i)), Number::Float(f) => self.write_fmt(format_args!("{}", f)) }
    }

    fn time_unit(&mut self, u: &TimeUnit) {
        let s = match u { TimeUnit::Dt => "dt", TimeUnit::Ns => "ns", TimeUnit::Us => "us", TimeUnit::Ms => "ms", TimeUnit::S => "s" };
        self.write(s);
    }

    fn index_operator(&mut self, op: &IndexOperator) {
        self.write("[");
        match op {
            IndexOperator::Set(s) => self.set_expression(s),
            IndexOperator::Items(items) => {
                for (i, it) in items.iter().enumerate() {
                    if i > 0 { self.write(", "); }
                    match it { IndexItem::Expr(e) => self.expression(e), IndexItem::Range(r) => self.range_expression(r) }
                }
            }
        }
        self.write("]");
    }

    fn range_expression(&mut self, r: &RangeExpression) {
        if let Some(s) = &r.start { self.expression(s); }
        self.write(":");
        if let Some(e) = &r.end { self.expression(e); }
        if let Some(st) = &r.step { self.write(":"); self.expression(st); }
    }

    fn set_expression(&mut self, s: &SetExpression) {
        self.write("{");
        for (i, e) in s.elements.iter().enumerate() { if i > 0 { self.write(", "); } self.expression(e); }
        self.write("}");
    }

    fn array_literal(&mut self, a: &ArrayLiteral) {
        self.write("{");
        for (i, it) in a.elements.iter().enumerate() {
            if i > 0 { self.write(", "); }
            match it { ArrayLiteralItem::Expr(e) => self.expression(e), ArrayLiteralItem::Array(ar) => self.array_literal(ar) }
        }
        self.write("}");
    }

    fn indexed_identifier(&mut self, ii: &IndexedIdentifier) {
        self.write(&ii.name);
        for idx in &ii.indices { self.index_operator(idx); }
    }

    fn expression_list(&mut self, list: &[Expression]) {
        for (i, e) in list.iter().enumerate() { if i > 0 { self.write(", "); } self.expression(e); }
    }

    // ----- Types -----
    fn return_signature(&mut self, r: &ReturnSignature) {
        self.write("-> ");
        self.scalar_type(&r.ty);
    }

    fn gate_modifier(&mut self, m: &GateModifier) {
        match m {
            GateModifier::Inv => self.write("inv @"),
            GateModifier::Pow(e) => { self.write("pow("); self.expression(e); self.write(") @"); }
            GateModifier::Ctrl(e) => { self.write("ctrl"); if let Some(e) = e { self.write("("); self.expression(e); self.write(")"); } self.write(" @"); }
            GateModifier::NegCtrl(e) => { self.write("negctrl"); if let Some(e) = e { self.write("("); self.expression(e); self.write(")"); } self.write(" @"); }
        }
    }

    fn scalar_type(&mut self, t: &ScalarType) {
        match t {
            ScalarType::Bit { designator } => { self.write("bit"); if let Some(d) = designator { self.write(" "); self.designator(d); } }
            ScalarType::Int { designator } => { self.write("int"); if let Some(d) = designator { self.write(" "); self.designator(d); } }
            ScalarType::UInt { designator } => { self.write("uint"); if let Some(d) = designator { self.write(" "); self.designator(d); } }
            ScalarType::Float { designator } => { self.write("float"); if let Some(d) = designator { self.write(" "); self.designator(d); } }
            ScalarType::Angle { designator } => { self.write("angle"); if let Some(d) = designator { self.write(" "); self.designator(d); } }
            ScalarType::Bool => self.write("bool"),
            ScalarType::Duration => self.write("duration"),
            ScalarType::Stretch => self.write("stretch"),
            ScalarType::Complex { inner } => {
                self.write("complex");
                if let Some(inner) = inner { self.write("["); self.scalar_type(inner); self.write("]"); }
            }
        }
    }

    fn qubit_type(&mut self, q: &QubitType) {
        self.write("qubit");
        if let Some(d) = &q.designator { self.write(" "); self.designator(d); }
    }

    fn array_type(&mut self, a: &ArrayType) {
        self.write("array[");
        self.scalar_type(&a.element_type);
        self.write(", ");
        self.expression_list(&a.dimensions);
        self.write("]");
    }

    fn array_ref_type(&mut self, a: &ArrayReferenceType) {
        match a.mutability { ArrayReferenceMutability::Readonly => self.write("readonly "), ArrayReferenceMutability::Mutable => self.write("mutable ") }
        self.write("array[");
        self.scalar_type(&a.element_type);
        self.write(", ");
        match &a.size {
            ArrayReferenceSize::List(list) => self.expression_list(list),
            ArrayReferenceSize::DimEquals(e) => { self.write("#dim = "); self.expression(e); },
        }
        self.write("]");
    }

    fn designator(&mut self, d: &Designator) {
        self.write("[");
        self.expression(&d.expr);
        self.write("]");
    }

    fn classical_type(&mut self, t: &ClassicalType) {
        match t { ClassicalType::Scalar(s) => self.scalar_type(s), ClassicalType::Array(a) => self.array_type(a) }
    }

    // ----- Gate / extern / defcal operands and args -----
    fn gate_operand(&mut self, o: &GateOperand) {
        match o {
            GateOperand::Indexed(i) => self.indexed_identifier(i),
            GateOperand::HardwareQubit(q) => { self.write("$"); self.write_fmt(format_args!("{}", q)); }
        }
    }

    fn gate_operand_list(&mut self, list: &GateOperandList) {
        for (i, o) in list.iter().enumerate() { if i > 0 { self.write(", "); } self.gate_operand(o); }
    }

    fn extern_argument(&mut self, a: &ExternArgument) {
        match a {
            ExternArgument::Scalar(s) => self.scalar_type(s),
            ExternArgument::ArrayReference(ar) => self.array_ref_type(ar),
            ExternArgument::CReg(d) => { self.write("creg"); if let Some(des) = d { self.write(" "); self.designator(des); } }
        }
    }

    fn extern_argument_list(&mut self, list: &[ExternArgument]) {
        for (i, a) in list.iter().enumerate() { if i > 0 { self.write(", "); } self.extern_argument(a); }
    }

    fn argument_definition(&mut self, a: &ArgumentDefinition) {
        match a {
            ArgumentDefinition::Scalar { ty, name } => { self.scalar_type(ty); self.write(" "); self.write(name); }
            ArgumentDefinition::Qubit { ty, name } => { self.qubit_type(ty); self.write(" "); self.write(name); }
            ArgumentDefinition::Register { kind, name, designator } => {
                match kind { OldStyleRegisterKind::CReg => self.write("creg"), OldStyleRegisterKind::QReg => self.write("qreg") }
                if let Some(d) = designator { self.write(" "); self.designator(d); }
                self.write(" ");
                self.write(name);
            }
            ArgumentDefinition::ArrayReference { ty, name } => { self.array_ref_type(ty); self.write(" "); self.write(name); }
        }
    }

    fn argument_definition_list(&mut self, list: &[ArgumentDefinition]) {
        for (i, a) in list.iter().enumerate() { if i > 0 { self.write(", "); } self.argument_definition(a); }
    }

    fn defcal_target(&mut self, t: &DefcalTarget) {
        match t { DefcalTarget::Measure => self.write("measure"), DefcalTarget::Reset => self.write("reset"), DefcalTarget::Delay => self.write("delay"), DefcalTarget::Identifier(id) => self.write(id) }
    }

    fn defcal_argument_definition(&mut self, a: &DefcalArgumentDefinition) {
        match a { DefcalArgumentDefinition::Expr(e) => self.expression(e), DefcalArgumentDefinition::Arg(ad) => self.argument_definition(ad) }
    }

    fn defcal_argument_definition_list(&mut self, list: &[DefcalArgumentDefinition]) {
        for (i, a) in list.iter().enumerate() { if i > 0 { self.write(", "); } self.defcal_argument_definition(a); }
    }

    fn defcal_operand(&mut self, o: &DefcalOperand) {
        match o { DefcalOperand::HardwareQubit(q) => { self.write("$"); self.write_fmt(format_args!("{}", q)); }, DefcalOperand::Identifier(id) => self.write(id) }
    }

    fn defcal_operand_list(&mut self, list: &[DefcalOperand]) {
        for (i, o) in list.iter().enumerate() { if i > 0 { self.write(", "); } self.defcal_operand(o); }
    }

    // ----- Identifiers -----
    fn identifier_list(&mut self, list: &[Identifier]) {
        for (i, id) in list.iter().enumerate() { if i > 0 { self.write(", "); } self.write(id); }
    }

    fn assignment_op(&mut self, op: &AssignmentOp) {
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
        self.write(s);
    }

    // ----- Measure -----
    fn measure_expression(&mut self, m: &MeasureExpression) {
        self.write("measure ");
        self.gate_operand(&m.operand);
    }
}

