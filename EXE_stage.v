`include "marco.h"

module EXE_stage(
    input  wire        clk,
    input  wire        resetn,
    //BUS
    input  wire [`ID_to_EXE_LEN  - 1: 0] ID_to_EXE_BUS,
    output wire [`EXE_to_MEM_LEN - 1: 0] EXE_to_MEM_BUS,
    output wire [`EXE_RF_LEN     - 1: 0] EXE_RF_BUS,
    //
    output wire        data_sram_en,     
    output wire [ 3:0] data_sram_we,
    output wire [31:0] data_sram_addr,
    output wire [31:0] data_sram_wdata,
    //
    input  wire ID_to_EXE_valid,
    input  wire MEM_allowin,
    output wire EXE_allowin,
    output wire EXE_to_MEM_valid
);
reg EXE_valid;
wire EXE_ready_go;
assign EXE_to_MEM_valid = EXE_valid && EXE_ready_go;
assign EXE_allowin = !EXE_valid || EXE_ready_go && MEM_allowin;

always @(posedge clk)begin
    if(~resetn)begin
        EXE_valid <= 1'b0;
    end
    else if(EXE_allowin)begin
        EXE_valid <= ID_to_EXE_valid;
    end
end

reg [`ID_to_EXE_LEN  - 1: 0] ID_to_EXE_BUS_temp;

always @(posedge clk)begin
    if(~resetn)begin
        ID_to_EXE_BUS_temp <= {`ID_to_EXE_LEN{1'b0}};
    end
    else if(ID_to_EXE_valid && EXE_allowin)begin
        ID_to_EXE_BUS_temp <= ID_to_EXE_BUS;
    end
    // else if(~ID_to_EXE_valid)begin
    //     ID_to_EXE_BUS_temp <= {`ID_to_EXE_LEN{1'b0}};    
    // end
end
//ID_to_EXE_BUS = {id_pc,gr_we,dest,mem_res,mem_en,data_addr,aluop,alusrc1,alusrc2}
wire [31: 0] exe_pc;
wire         gr_we;
wire [ 4: 0] dest;
wire [31: 0] mem_sum;
wire         mem_en;
wire [ 3: 0] mem_we;
wire [11: 0] alu_op;
wire [31: 0] alu_src1;
wire [31: 0] alu_src2;
wire [ 3: 0] load_op;
wire         rfrom_mem;
wire [ 6: 0] mul_div_op;
assign {exe_pc,gr_we,dest,mem_sum,mem_en,mem_we,alu_op,alu_src1,alu_src2,load_op,rfrom_mem,mul_div_op} = ID_to_EXE_BUS_temp;


wire [31: 0] alu_result;
alu u_alu(
    .alu_op     (alu_op    ),
    .alu_src1   (alu_src1  ),
    .alu_src2   (alu_src2  ),
    .alu_result (alu_result)
);

wire [63: 0] div_result_signed;
reg          div_valid_signed;
wire         div_divisor_ready_signed;
wire         div_dividend_ready_signed;
wire         div_dout_valid_signed;
div_gen_signed u_div_gen_signed(
    .aclk                   (clk),
    .s_axis_divisor_tdata   (alu_src2),
    .s_axis_divisor_tready  (div_divisor_ready_signed),
    .s_axis_divisor_tvalid  (div_valid_signed && EXE_valid),
    .s_axis_dividend_tdata  (alu_src1),
    .s_axis_dividend_tready (div_dividend_ready_signed),
    .s_axis_dividend_tvalid (div_valid_signed && EXE_valid),
    .m_axis_dout_tdata      (div_result_signed),
    .m_axis_dout_tvalid     (div_dout_valid_signed)
);

always @(posedge clk)begin
    if(~resetn)begin
        div_valid_signed <= 1'b0;
    end else if (ID_to_EXE_valid && EXE_allowin)
        div_valid_signed <= mul_div_op[3] || mul_div_op[4];
    else if(div_divisor_ready_signed && div_dividend_ready_signed)begin
        div_valid_signed <= 1'b0;
    end
end

wire [63: 0] div_result_unsigned;
reg          div_valid_unsigned;
wire         div_divisor_ready_unsigned;
wire         div_dividend_ready_unsigned;
wire         div_dout_valid_unsigned;
div_gen_unsigned u_div_gen_unsigned(
    .aclk                   (clk),
    .s_axis_divisor_tdata   (alu_src2),
    .s_axis_divisor_tready  (div_divisor_ready_unsigned),
    .s_axis_divisor_tvalid  (div_valid_unsigned && EXE_valid),
    .s_axis_dividend_tdata  (alu_src1),
    .s_axis_dividend_tready (div_dividend_ready_unsigned),
    .s_axis_dividend_tvalid (div_valid_unsigned && EXE_valid),
    .m_axis_dout_tdata      (div_result_unsigned),
    .m_axis_dout_tvalid     (div_dout_valid_unsigned)
);

always @(posedge clk)begin
    if(~resetn)begin
        div_valid_unsigned <= 1'b0;
    end else if (ID_to_EXE_valid && EXE_allowin)
        div_valid_unsigned <= mul_div_op[5] || mul_div_op[6];
    else if(div_divisor_ready_unsigned && div_dividend_ready_unsigned)begin
        div_valid_unsigned <= 1'b0;
    end
end

// wire [63: 0] unsigned_prod, signed_prod;
// assign unsigned_prod = alu_src1 * alu_src2;
// assign signed_prod = $signed(alu_src1) * $signed(alu_src2);
wire [65: 0] mul_result;
wire         is_signed;
assign is_signed = mul_div_op[1];
assign mul_result = $signed({is_signed & alu_src1[31] ,alu_src1}) * $signed({is_signed & alu_src2[31], alu_src2});

wire [31: 0] exe_result;
assign exe_result = {32{mul_div_op[0]}} & mul_result[31: 0]
                   | {32{mul_div_op[1] || mul_div_op[2]}} & mul_result[63:32]
                   | {32{mul_div_op[3]}} & div_result_signed[63:32]
                   | {32{mul_div_op[4]}} & div_result_signed[31: 0]
                   | {32{mul_div_op[5]}} & div_result_unsigned[63:32]
                   | {32{mul_div_op[6]}} & div_result_unsigned[31: 0];

assign EXE_ready_go = (mul_div_op[3] || mul_div_op[4]) && div_dout_valid_signed 
                   || (mul_div_op[5] || mul_div_op[6]) && div_dout_valid_unsigned
                   || !(mul_div_op[3] || mul_div_op[4] || mul_div_op[5] || mul_div_op[6]);

//{dest,op,aluresult}
assign EXE_RF_BUS = {{`DEST_LEN{gr_we & EXE_valid}} & dest,rfrom_mem,exe_result};

assign data_sram_en    = (rfrom_mem || mem_en) && EXE_valid;
assign data_sram_we    = mem_we;
assign data_sram_addr  = alu_result;
assign data_sram_wdata = mem_sum;
//EXE_to_MEM_BUS = {exe_pc,gr_we,dest,alu_res,data_sum,mem_en,mem_we,loadop}
assign EXE_to_MEM_BUS = {exe_pc,gr_we,dest,exe_result,mem_sum,mem_en,mem_we,load_op,rfrom_mem};
endmodule