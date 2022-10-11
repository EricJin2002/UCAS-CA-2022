`include "macro.vh"

module MEM_stage(
    input  wire        clk,
    input  wire        resetn,
    // data sram interface
    input  wire [31:0] data_sram_rdata,
    // BUS
    input  wire [`EXE_to_MEM_LEN - 1: 0] EXE_to_MEM_BUS,
    output wire [`MEM_to_WB_LEN  - 1: 0] MEM_to_WB_BUS,
    output wire [`MEM_RF_LEN     - 1: 0] MEM_RF_BUS,
    //
    input  wire EXE_to_MEM_valid,
    input  wire WB_allowin,
    output wire MEM_allowin,
    output wire MEM_to_WB_valid 
);
wire MEM_ready_go;
assign MEM_ready_go = 1'b1;
reg MEM_valid;
assign MEM_to_WB_valid = MEM_valid && MEM_ready_go;
assign MEM_allowin = !MEM_valid || MEM_ready_go && WB_allowin;

always @(posedge clk)begin
    if(~resetn)begin
        MEM_valid <= 1'b0;
    end
    else if(MEM_allowin)begin
        MEM_valid <= EXE_to_MEM_valid;
    end
end

reg [`EXE_to_MEM_LEN - 1: 0] EXE_to_MEM_BUS_temp;

always @(posedge clk)begin
    if(~resetn)begin
        EXE_to_MEM_BUS_temp <= {`EXE_to_MEM_LEN{1'b0}};
    end
    else if(EXE_to_MEM_valid && MEM_allowin)begin
        EXE_to_MEM_BUS_temp <= EXE_to_MEM_BUS;
    end
    // else if(~EXE_to_MEM_valid)begin
    //     EXE_to_MEM_BUS_temp <= {`EXE_to_MEM_LEN{1'b0}};        
    // end
end
//EXE_to_MEM_BUS = {exe_pc,gr_we,dest,alu_res,data_addr,mem_en,mem_we,loadop,rfrom_mem}
wire [31: 0] mem_pc     ;
wire         gr_we      ;
wire [ 4: 0] dest       ;
wire [31: 0] alu_result ;
wire [31: 0] data_sum   ;
wire         mem_en     ;
wire [ 4: 0] load_op    ;
wire         rfrom_mem  ;
assign {mem_pc,gr_we,dest,alu_result,data_sum,mem_en,load_op,rfrom_mem} = EXE_to_MEM_BUS_temp;

wire [31: 0] mem_result_shift;
wire [31: 0] mem_result;
assign mem_result_shift = data_sram_rdata>>({3'b0, alu_result[1:0]}<<3);
assign mem_result       = {32{load_op[`LD_B] || load_op[`LD_BU]}} & {{24{load_op[`LD_B] && mem_result_shift[7]}}, mem_result_shift[7:0]}
                        | {32{load_op[`LD_H] || load_op[`LD_HU]}} & {{16{load_op[`LD_H] && mem_result_shift[15]}}, mem_result_shift[15:0]}
                        | {32{load_op[`LD_W]}} & mem_result_shift;

wire [31: 0] ms_final_result;
assign ms_final_result = rfrom_mem ? mem_result : alu_result;
//{dest,op,mem_result,ms_final_result}
assign MEM_RF_BUS = {{`DEST_LEN{gr_we & MEM_valid}} & dest,rfrom_mem,ms_final_result};

//MEM_to_WB_BUS = {mem_pc,gr_we,dest,memresult,aluresult,loadop,rfrom_mem}
assign MEM_to_WB_BUS = {mem_pc,gr_we,dest,mem_result,alu_result,rfrom_mem};

endmodule