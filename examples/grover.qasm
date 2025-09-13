OPENQASM 3.0;
include "stdgates.inc";

gate oracle q0, q1 {
    ctrl(q0, q1) z q1;
}

gate diffuser q0, q1 {
    h q0;
    h q1;
    x q0;
    x q1;
    ctrl(q0) z q1;
    x q0;
    x q1;
    h q0;
    h q1;
}

qubit q[2];
bit c[2];

h q[0];
h q[1];

oracle q[0], q[1];
diffuser q[0], q[1];

c = measure q;