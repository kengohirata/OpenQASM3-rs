// Builds a Program with a Version.
pub fn build() -> openqasm3::ast::Program {
    let version = openqasm3::ast::Version { spec: (3, Some(0)) };
    openqasm3::ast::Program { version: Some(version), items: vec![] }
}
