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
wire EXE_ready_go;
assign EXE_ready_go = 1'b1;
reg EXE_valid;
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

//{dest,op,aluresult}
assign EXE_RF_BUS = {{`DEST_LEN{gr_we & EXE_valid}} & dest,rfrom_mem,alu_result};

assign data_sram_en    = (rfrom_mem || mem_en) && EXE_valid;
assign data_sram_we    = mem_we;
assign data_sram_addr  = alu_result;
assign data_sram_wdata = mem_sum;
//EXE_to_MEM_BUS = {exe_pc,gr_we,dest,alu_res,data_sum,mem_en,mem_we,loadop}
assign EXE_to_MEM_BUS = {exe_pc,gr_we,dest,alu_result,mem_sum,mem_en,mem_we,load_op,rfrom_mem};
endmodule