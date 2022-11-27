`include "macro.vh"
module WB_stage(
    input  wire        clk,
    input  wire        resetn,

    output wire [13: 0] csr_num,
    output wire         csr_we,
    output wire [31: 0] csr_wvalue,
    output wire [31: 0] csr_wmask,

    output wire [31: 0] wb_pc,
    output wire [31: 0] wb_vaddr,
    output wire         ertn_flush,
    output wire         wb_ex,
    output wire [ 5: 0] wb_ecode,
    output wire [ 8: 0] wb_esubcode,
    output wire         wb_refetch,
    // trace debug interface
    output wire [31:0] debug_wb_pc,
    output wire [ 3:0] debug_wb_rf_we,
    output wire [ 4:0] debug_wb_rf_wnum,
    output wire [31:0] debug_wb_rf_wdata,    
    // bus
    input  wire [`MEM_to_WB_LEN  - 1: 0] MEM_to_WB_BUS,
    output wire [`RF_BUS_LEN     - 1: 0] RF_BUS,
    output wire [`WB_RF_LEN      - 1: 0] WB_RF_BUS,
    
    input  wire MEM_to_WB_valid,
    output wire WB_allowin,

    input  wire [31: 0] stable_cnt_tid,

    output wire [ 4: 0] tlb_inst_op_to_csr,
    output wire         WB_to_EXE_block_tlbsrch
);


// WB
reg  WB_valid;
wire WB_ready_go;
assign WB_allowin = !WB_valid || WB_ready_go; 

assign WB_ready_go = 1'b1;

always @(posedge clk) begin
    if (~resetn) begin
        WB_valid <= 1'b0;
    end else if (ertn_flush || wb_ex || wb_refetch) begin
        WB_valid <= 1'b0;
    end else if (WB_allowin) begin
        WB_valid <= MEM_to_WB_valid;
    end
end


// MEM to WB
reg [`MEM_to_WB_LEN  - 1: 0] MEM_to_WB_BUS_temp;

always @(posedge clk)begin
    if(~resetn)begin
        MEM_to_WB_BUS_temp <= {`MEM_to_WB_LEN{1'b0}};
    end
    else if(MEM_to_WB_valid && WB_allowin)begin
        MEM_to_WB_BUS_temp <= MEM_to_WB_BUS;
    end
end

// wire [31: 0] wb_pc;
wire         gr_we;
wire [ 4: 0] dest;
wire [31: 0] ms_final_result;
wire         csr_we_in;
// wire [31: 0] csr_wvalue;
// wire [31: 0] csr_wmask;
wire         wb_ex_in;
wire         wb_ex_out;
wire [14: 0] wb_ex_code_in;
wire [14: 0] wb_ex_code_out;
wire [31: 0] wb_ex_vaddr_in;
wire [31: 0] wb_ex_vaddr_out;
wire         wb_refetch_in;
wire         wb_refetch_out;
wire         inst_ertn;
wire [ 4: 0] tlb_inst_op;
assign {wb_pc,gr_we,dest,ms_final_result,csr_num,csr_we_in,csr_wvalue,csr_wmask,
    wb_ex_in,wb_ex_code_in,wb_ex_vaddr_in,inst_ertn,rfrom_cntid,wb_refetch_in,tlb_inst_op} = MEM_to_WB_BUS_temp;


// wb result
wire [31: 0] ws_final_result;
assign ws_final_result = rfrom_cntid ? stable_cnt_tid : ms_final_result;


// WB to RF
wire         rf_we   ;
wire [ 4: 0] rf_waddr;
wire [31: 0] rf_wdata;
assign rf_we    = gr_we && WB_valid && !wb_ex && !ertn_flush && !wb_refetch;
assign rf_waddr = dest;
assign rf_wdata = ws_final_result;
assign RF_BUS = {rf_we,rf_waddr,rf_wdata};


// operand forwarding
// if WB is invalid, WB_dest is 0
assign WB_RF_BUS = {{`DEST_LEN{gr_we & WB_valid}} & dest,ws_final_result};


// csr write
assign csr_we = csr_we_in && WB_valid && !wb_ex && !ertn_flush && !wb_refetch;


// exception
assign wb_ex_out        = wb_ex_in;
assign wb_ex_code_out   = wb_ex_code_in;
assign wb_ex_vaddr_out  = wb_ex_vaddr_in;

assign wb_ex                    = wb_ex_out && WB_valid;
assign {wb_esubcode,wb_ecode}   = wb_ex_code_out;
assign wb_vaddr                 = wb_ex_vaddr_out;

assign ertn_flush = inst_ertn && WB_valid;

assign wb_refetch_out = wb_refetch_in;

assign wb_refetch = wb_refetch_out && WB_valid;

assign WB_to_EXE_block_tlbsrch = (csr_we && (csr_num==`CSR_ASID || csr_num==`CSR_TLBEHI) || tlb_inst_op[`TLB_INST_TLBRD]) && WB_valid;


// tlb
assign tlb_inst_op_to_csr = {
    5{WB_valid && !wb_ex && !ertn_flush && !wb_refetch}
} & tlb_inst_op & (`TLB_INST_TLBWR_MASK | `TLB_INST_TLBFILL_MASK | `TLB_INST_TLBRD_MASK);


// debug info generate
assign debug_wb_pc       = wb_pc;
assign debug_wb_rf_we    = {4{rf_we}};
assign debug_wb_rf_wnum  = dest;
assign debug_wb_rf_wdata = ws_final_result;

endmodule