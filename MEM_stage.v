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
    output wire MEM_to_WB_valid,

    input  wire        ertn_flush,
    input  wire        wb_ex,
    output wire        mem_ertn,
    output wire        mem_ex
);


// MEM
reg  MEM_valid;
wire MEM_ready_go;
assign MEM_to_WB_valid = MEM_valid && MEM_ready_go;
assign MEM_allowin = !MEM_valid || MEM_ready_go && WB_allowin;

assign MEM_ready_go = 1'b1;

always @(posedge clk) begin
    if (~resetn) begin
        MEM_valid <= 1'b0;
    end else if (ertn_flush || wb_ex) begin
        MEM_valid <= 1'b0;
    end else if (MEM_allowin) begin
        MEM_valid <= EXE_to_MEM_valid;
    end
end


// EXE to MEM
reg [`EXE_to_MEM_LEN - 1: 0] EXE_to_MEM_BUS_temp;

always @(posedge clk) begin
    if(~resetn) begin
        EXE_to_MEM_BUS_temp <= {`EXE_to_MEM_LEN{1'b0}};
    end else if(EXE_to_MEM_valid && MEM_allowin) begin
        EXE_to_MEM_BUS_temp <= EXE_to_MEM_BUS;
    end
end

wire [31: 0] mem_pc;
wire         gr_we;
wire [ 4: 0] dest;
wire [31: 0] exe_result;
wire [31: 0] data_sum;
wire         mem_en;
wire [ 4: 0] load_op;
wire         rfrom_mem;
wire [13: 0] csr_num;
wire         csr_we;
wire [31: 0] csr_wvalue;
wire [31: 0] csr_wmask;
wire         mem_ex_in;
wire         mem_ex_out;
wire [14: 0] mem_ex_code_in;
wire [14: 0] mem_ex_code_out;
wire         inst_ertn;
assign {mem_pc,gr_we,dest,exe_result,data_sum,mem_en,load_op,rfrom_mem,csr_num,csr_we,csr_wvalue,csr_wmask,mem_ex_in,mem_ex_code_in,inst_ertn} = EXE_to_MEM_BUS_temp;


// mem result
wire [31: 0] mem_result_shift;
wire [31: 0] mem_result;
assign mem_result_shift = data_sram_rdata>>({3'b0, exe_result[1:0]}<<3);
assign mem_result       = {32{load_op[`LD_B] || load_op[`LD_BU]}} & {{24{load_op[`LD_B] && mem_result_shift[7]}}, mem_result_shift[7:0]}
                        | {32{load_op[`LD_H] || load_op[`LD_HU]}} & {{16{load_op[`LD_H] && mem_result_shift[15]}}, mem_result_shift[15:0]}
                        | {32{load_op[`LD_W]}} & mem_result_shift;

wire [31: 0] ms_final_result;
assign ms_final_result = rfrom_mem ? mem_result : exe_result;


// operand forwarding
// if MEM is invalid, MEM_dest is 0
assign MEM_RF_BUS = {{`DEST_LEN{gr_we & MEM_valid}} & dest,ms_final_result,MEM_valid,csr_we,csr_num};


// exception
assign mem_ex_out = mem_ex_in;
assign mem_ex_code_out = mem_ex_code_in;

assign mem_ex = mem_ex_out && MEM_valid;
assign mem_ertn = inst_ertn && MEM_valid;


// MEM to WB
assign MEM_to_WB_BUS = {mem_pc,gr_we,dest,ms_final_result,csr_num,csr_we,csr_wvalue,csr_wmask,mem_ex_out,mem_ex_code_out,inst_ertn};

endmodule