module syn_spram_4x1_tmr (
  clk,
  addr_0,
  addr_1,
  d_in,
  wr_en,
  d_out
);

  input wire clk;
  input wire addr_0;
  input wire addr_1;
  input wire d_in;
  input wire wr_en;
  output reg d_out;

  wire [1:0] addr;

  reg [3:0] mem_a;
  reg [3:0] mem_b;
  reg [3:0] mem_c;

  wire tmp_a;
  wire tmp_b;
  wire tmp_c;

  wire voted_tmp;

  assign addr = {addr_1, addr_0};

  assign tmp_a = (addr == 2'd0) ? mem_a[0] :
                 (addr == 2'd1) ? mem_a[1] :
                 (addr == 2'd2) ? mem_a[2] : mem_a[3];

  assign tmp_b = (addr == 2'd0) ? mem_b[0] :
                 (addr == 2'd1) ? mem_b[1] :
                 (addr == 2'd2) ? mem_b[2] : mem_b[3];

  assign tmp_c = (addr == 2'd0) ? mem_c[0] :
                 (addr == 2'd1) ? mem_c[1] :
                 (addr == 2'd2) ? mem_c[2] : mem_c[3];

  assign voted_tmp = (tmp_a & tmp_b) |
                     (tmp_a & tmp_c) |
                     (tmp_b & tmp_c);

  always @(posedge clk) begin
    if (wr_en) begin
      mem_a[addr] <= d_in;
      mem_b[addr] <= d_in;
      mem_c[addr] <= d_in;
    end
    d_out <= voted_tmp;
  end

endmodule