module decoder_2_4(
    input  wire [ 1:0] in,
    output wire [ 3:0] out
);

genvar i;
generate for (i=0; i<4; i=i+1) begin : gen_for_dec_2_4
    assign out[i] = (in == i);
end endgenerate

endmodule


module decoder_4_16(
    input  wire [ 3:0] in,
    output wire [15:0] out
);

genvar i;
generate for (i=0; i<16; i=i+1) begin : gen_for_dec_4_16
    assign out[i] = (in == i);
end endgenerate

endmodule


module decoder_5_32(
    input  wire [ 4:0] in,
    output wire [31:0] out
);

genvar i;
generate for (i=0; i<32; i=i+1) begin : gen_for_dec_5_32
    assign out[i] = (in == i);
end endgenerate

endmodule


module decoder_6_64(
    input  wire [ 5:0] in,
    output wire [63:0] out
);

genvar i;
generate for (i=0; i<63; i=i+1) begin : gen_for_dec_6_64
    assign out[i] = (in == i);
end endgenerate

endmodule


module encoder_16_4(
    input  wire [15:0] in,
    output wire [ 3:0] out
);

assign out = {4{in[0]}} & 0
           | {4{in[1]}} & 1
           | {4{in[2]}} & 2
           | {4{in[3]}} & 3
           | {4{in[4]}} & 4
           | {4{in[5]}} & 5
           | {4{in[6]}} & 6
           | {4{in[7]}} & 7
           | {4{in[8]}} & 8
           | {4{in[9]}} & 9
           | {4{in[10]}} & 10
           | {4{in[11]}} & 11
           | {4{in[12]}} & 12
           | {4{in[13]}} & 13
           | {4{in[14]}} & 14
           | {4{in[15]}} & 15;

endmodule