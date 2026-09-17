// Verilog code for Multiplexer implementation using always block.
// by Harsha Perla for http://electrosofts.com
// harsha@electrosofts.com 
// Available at http://electrosofts.com/verilog

module mux2( select, d, q );

input[1:0] select;
input[3:0] d;
output     q;

reg        q;
wire[1:0]  select;
wire[3:0]  d;

always @(d or select)
    q = d[select];

endmodule
