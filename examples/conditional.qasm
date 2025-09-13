OPENQASM 3.0;

qubit q[3];
bit c[3];

h q[0];

c = measure q;

if (c[0]) {
    x q[1];
    h q[2];
} else {
    h q[1];
    x q[2];
}