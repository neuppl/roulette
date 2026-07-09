// Source: https://www.edaplayground.com/x/2LS3
module jhalfadder(sum,carry,a,b);
	output sum,carry;
	input a,b;
	
	assign sum = a^b;
	assign carry = a&b;
	
endmodule

module jfulladder(y,carryout,a,b,carryin);
  output y,carryout;
  input a,b,carryin;
  
  assign y = a ^ b ^ carryin;
  assign carryout = ( a & b ) | ( a & carryin ) | ( b & carryin );
  
endmodule

// This takes two 4 bit numbers and outputs a 8 bit number
// Reuses the half adder and full adder modules already existing
// Look at array multiplier block diagram to understand the code
module junsignedArrayMultiplier(PRODUCT, A, B);
  output [7:0] PRODUCT;
  input [3:0] A, B;
  
  wire [14:0] W;
  wire [10:0] C;
  wire [5:0] S;

  // Instantiate all the AND functions
  
  and and00 ( W[0], A[0], B[1] );
  and and01 ( W[1], A[0], B[2] );
  and and02 ( W[2], A[0], B[3] );

  and and03 ( W[3], A[1], B[0] );
  and and04 ( W[4], A[1], B[1] );
  and and05 ( W[5], A[1], B[2] );
  and and06 ( W[6], A[1], B[3] );

  and and07 ( W[7], A[2], B[0] );
  and and08 ( W[8], A[2], B[1] );
  and and09 ( W[9], A[2], B[2] );
  and and10 ( W[10], A[2], B[3] );

  and and11 ( W[11], A[3], B[0] );
  and and12 ( W[12], A[3], B[1] );
  and and13 ( W[13], A[3], B[2] );
  and and14 ( W[14], A[3], B[3] );


  // LSB is calculated here
  and and15 ( PRODUCT[0], A[0], B[0] );

  // First row
  // Bit 1 of the product as well
	jhalfadder jha01 ( PRODUCT[1], C[0], W[0], W[3] );
  jfulladder jfa01 ( S[0], C[1], W[1], W[4], C[0] );
  jfulladder jfa02 ( S[1], C[2], W[2], W[5], C[1] );
	jhalfadder jha02 ( S[2], C[3], W[6], C[2] );

  // Next row
	jhalfadder jha03 ( PRODUCT[2], C[4], W[7], S[0] );
  jfulladder jfa03 ( S[3], C[5], W[8], S[1], C[4] );
  jfulladder jfa04 ( S[4], C[6], W[9], S[2], C[5] );
  jfulladder jfa05 ( S[5], C[7], W[10], C[3], C[6] );
  
  // Next row
	jhalfadder jha04 ( PRODUCT[3], C[8], W[11], S[3] );
  jfulladder jfa06 ( PRODUCT[4], C[9], W[12], S[4], C[8] );
  jfulladder jfa07 ( PRODUCT[5], C[10], W[13], S[5], C[9] );
  jfulladder jfa08 ( PRODUCT[6], PRODUCT[7], W[14], C[7], C[10] );
  
endmodule
