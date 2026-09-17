module full_adder (
    input a, b, cin,
    output sum, cout
);
    assign sum = a ^ b ^ cin;
    assign cout = (a & b) | (b & cin) | (a & cin);
endmodule

module ripple_carry_2bit (
    input [1:0] a, b,
    input cin,
    output [1:0] sum,
    output cout
);
    wire c1; // Intermediate carries

    // Port mapping each bit
    full_adder fa0 (a[0], b[0], cin, sum[0], c1);
    full_adder fa3 (a[1], b[1], c1,  sum[1], cout);
endmodule