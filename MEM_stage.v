`include "macro.vh"

module MEM_stage(
    input  wire        clk,
    input  wire        resetn,
    // data sram interface
    input wire         data_sram_data_ok,
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

    input  wire [ 3:0] IO_cnt,

    input  wire        ertn_flush,
    input  wire        wb_ex,
    output wire        mem_ertn,
    output wire        mem_ex
);
reg [3:0] current_state;
reg [3:0] next_state;
reg data_ok_r;

// MEM
reg  MEM_valid;
wire MEM_ready_go;
assign MEM_to_WB_valid = MEM_valid && MEM_ready_go;
assign MEM_allowin = (!MEM_valid || MEM_ready_go && WB_allowin) && current_state == `MEM_INIT;


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
wire [31: 0] mem_ex_vaddr_in;
wire [31: 0] mem_ex_vaddr_out;
wire         inst_ertn;
wire         rfrom_cntid;
wire [63: 0] mul_result;
wire [ 6: 0] mul_div_op;
wire         wait_store_ok;
assign {mem_pc,gr_we,dest,exe_result,data_sum,mem_en,load_op,rfrom_mem,csr_num,csr_we,csr_wvalue,csr_wmask,mem_ex_in,mem_ex_code_in,mem_ex_vaddr_in,inst_ertn,rfrom_cntid,mul_result,mul_div_op,wait_store_ok} = EXE_to_MEM_BUS_temp;


assign MEM_ready_go = (data_ok_r || data_sram_data_ok) && (current_state != `MEM_CANCEL) || !(rfrom_mem || wait_store_ok) || mem_ex_out;

always @(posedge clk) begin
    if (~resetn) begin
        MEM_valid <= 1'b0;
    end else if (ertn_flush || wb_ex) begin
        MEM_valid <= 1'b0;
    end else if (MEM_allowin) begin
        MEM_valid <= EXE_to_MEM_valid;
    end
end

always @(posedge clk) begin
    if (~resetn) begin
        current_state <= `MEM_INIT;
    end else begin
        current_state <= next_state;
    end
end

always @(*) begin
    if (~resetn) begin
        current_state <= `MEM_INIT;
    end else begin
        case (current_state)
            `MEM_INIT : begin
                if((wb_ex || ertn_flush) && !MEM_allowin && !MEM_ready_go && (rfrom_mem || wait_store_ok))
                    next_state <= `MEM_CANCEL;
                else 
                    next_state <= `MEM_INIT; 
            end
            `MEM_CANCEL : begin
                if (IO_cnt == 4'b1 && data_sram_data_ok)
                    next_state <= `MEM_INIT;
                else
                    next_state <= `MEM_CANCEL;
            end
        endcase
    end
end


wire [31:0] mem_data_out;
wire [31:0] mem_data;
reg  [31:0] mem_data_r;
assign mem_data_out = {32{ data_sram_data_ok             }} & mem_data
                    | {32{!data_sram_data_ok && data_ok_r}} & mem_data_r;

always @(posedge clk) begin
    if (~resetn) begin
        data_ok_r   <= 1'b0;
        mem_data_r  <= 32'b0;
    end else if (wb_ex || ertn_flush) begin
        data_ok_r   <= 1'b0;
        mem_data_r  <= 32'b0;
    end else if (data_sram_data_ok && !(MEM_to_WB_valid && WB_allowin)) begin
        data_ok_r   <= 1'b1;
        mem_data_r  <= data_sram_rdata;
    end else if (MEM_to_WB_valid && WB_allowin) begin
        data_ok_r   <= 1'b0;
        mem_data_r  <= 32'b0;
    end
end

// mem result
wire [31: 0] mem_result_shift;
wire [31: 0] mem_result;
assign mem_result_shift = mem_data_out>>{exe_result[1:0], 3'b0};
// assign mem_result_shift = {32{ exe_result[1] &&  exe_result[0]}} & {24'b0, data_sram_rdata[31:24]}
//                         | {32{ exe_result[1] && ~exe_result[0]}} & {16'b0, data_sram_rdata[31:16]}
//                         | {32{~exe_result[1] &&  exe_result[0]}} & { 8'b0, data_sram_rdata[31: 8]}
//                         | {32{~exe_result[1] && ~exe_result[0]}} & {       data_sram_rdata[31: 0]};
assign mem_result       = {32{load_op[`LD_B] || load_op[`LD_BU]}} & {{24{load_op[`LD_B] && mem_result_shift[7]}}, mem_result_shift[7:0]}
                        | {32{load_op[`LD_H] || load_op[`LD_HU]}} & {{16{load_op[`LD_H] && mem_result_shift[15]}}, mem_result_shift[15:0]}
                        | {32{load_op[`LD_W]}} & mem_result_shift;

wire [31: 0] ms_final_result;
assign ms_final_result = {32{rfrom_mem}}                                                        & mem_result
                       | {32{mul_div_op[6]}}                                                    & mul_result[31: 0]
                       | {32{mul_div_op[5] || mul_div_op[4]}}                                   & mul_result[63:32]
                       | {32{!rfrom_mem && !mul_div_op[6] && !mul_div_op[5] && !mul_div_op[4]}} & exe_result;                   


// operand forwarding
// if MEM is invalid, MEM_dest is 0
assign MEM_RF_BUS = {{`DEST_LEN{gr_we & MEM_valid}} & dest,MEM_to_WB_valid,rfrom_cntid,ms_final_result,MEM_valid,csr_we,csr_num};


// exception
assign mem_ex_out       = mem_ex_in;
assign mem_ex_code_out  = mem_ex_code_in;
assign mem_ex_vaddr_out = mem_ex_vaddr_in;

assign mem_ex = mem_ex_out && MEM_valid;
assign mem_ertn = inst_ertn && MEM_valid;


// data sram
assign mem_data = data_sram_rdata;


// MEM to WB
assign MEM_to_WB_BUS = {mem_pc,gr_we,dest,ms_final_result,csr_num,csr_we,csr_wvalue,csr_wmask,mem_ex_out,mem_ex_code_out,mem_ex_vaddr_out,inst_ertn,rfrom_cntid};

endmodule