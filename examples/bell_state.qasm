OPENQASM 3.0;
include "stdgates.inc";

qubit q[2];
bit c[2];

h q[0];

cnot q[0], q[1];

c = measure q;