`include "macro.vh"
module WB_stage(
    input  wire        clk,
    input  wire        resetn,

    output wire         csr_num,
    output wire         csr_we,
    output wire [31: 0] csr_wvalue,
    output wire [31: 0] csr_wmask,
    // trace debug interface
    output wire [31:0] debug_wb_pc,
    output wire [ 3:0] debug_wb_rf_we,
    output wire [ 4:0] debug_wb_rf_wnum,
    output wire [31:0] debug_wb_rf_wdata,    
    //BUS 
    input  wire [`MEM_to_WB_LEN  - 1: 0] MEM_to_WB_BUS,
    output wire [`RF_BUS_LEN     - 1: 0] RF_BUS,
    output wire [`WB_RF_LEN      - 1: 0] WB_RF_BUS,
    //
    input  wire MEM_to_WB_valid,
    output wire WB_allowin
);
wire WB_ready_go;
assign WB_ready_go = 1'b1;
reg  WB_valid;
assign WB_allowin = !WB_valid || WB_ready_go; 
always @(posedge clk)begin
    if(~resetn)begin
        WB_valid <= 1'b0;
    end
    else if(WB_allowin)begin
        WB_valid <= MEM_to_WB_valid;
    end
end

reg [`MEM_to_WB_LEN  - 1: 0] MEM_to_WB_BUS_temp;

always @(posedge clk)begin
    if(~resetn)begin
        MEM_to_WB_BUS_temp <= {`MEM_to_WB_LEN{1'b0}};
    end
    else if(MEM_to_WB_valid && WB_allowin)begin
        MEM_to_WB_BUS_temp <= MEM_to_WB_BUS;
    end
    // else if(~MEM_to_WB_valid)begin
    //     MEM_to_WB_BUS_temp <= {`MEM_to_WB_LEN{1'b0}};
    // end
end

//MEM_to_WB_BUS = {mem_pc,gr_we,dest,memresult,aluresult,loadop,rfrom_mem}
wire [31: 0] wb_pc;
wire         gr_we;
wire [ 4: 0] dest;
wire [31: 0] mem_result;
wire [31: 0] alu_result;
wire         rfrom_mem;
assign {wb_pc,gr_we,dest,mem_result,alu_result,rfrom_mem,csr_num,csr_we,csr_wvalue,csr_wmask} = MEM_to_WB_BUS_temp;

 
wire         rf_we   ;
wire [ 4: 0] rf_waddr;
wire [31: 0] rf_wdata;
wire [31: 0] ms_final_result;
assign ms_final_result = rfrom_mem ? mem_result : alu_result;
assign rf_we    = gr_we && WB_valid;
assign rf_waddr = dest;
assign rf_wdata = ms_final_result;
assign RF_BUS = {rf_we,rf_waddr,rf_wdata};

//{dest,op,final_result}
assign WB_RF_BUS = {{`DEST_LEN{gr_we & WB_valid}} & dest,rfrom_mem,ms_final_result};

// debug info generate
assign debug_wb_pc       = wb_pc;
assign debug_wb_rf_we    = {4{rf_we}};
assign debug_wb_rf_wnum  = dest;
assign debug_wb_rf_wdata = ms_final_result;

endmodule