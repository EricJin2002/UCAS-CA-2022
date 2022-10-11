`include "macro.vh"

module IF_stage(
    input  wire        clk,
    input  wire        resetn,
    // inst sram interface
    output wire        inst_sram_en,
    output wire [ 3:0] inst_sram_we,
    output wire [31:0] inst_sram_addr,
    output wire [31:0] inst_sram_wdata,
    input  wire [31:0] inst_sram_rdata,
    //BUS
    input  wire [`BR_BUS_LEN  : 0] BR_BUS,
    output wire [`IF_to_ID_LEN: 0] IF_to_ID_BUS,
    //
    input  wire        ID_allowin,
    output wire        IF_to_ID_valid,
    output wire        IF_allowin    
);

//BR_BUS = {BR_target,BR_taken}
//IF_to_ID_BUS = {if_pc,if_inst}

wire         br_taken ;
wire         br_taken_cancel;
wire [31: 0] br_target;

assign {br_target,br_taken,br_taken_cancel} = BR_BUS;  

reg  [31: 0] if_pc    ;//fs_pc
wire [31: 0] if_inst;
wire [31: 0] seq_pc   ;
wire [31: 0] nextpc   ;

assign IF_to_ID_BUS = {if_pc,if_inst};

assign seq_pc = if_pc + 3'h4;
assign nextpc = br_taken ? br_target : seq_pc;

reg IF_valid;
wire IF_ready_go;
wire validin;

assign IF_ready_go = 1'b1; 
assign validin = resetn;
assign IF_allowin = !IF_valid || IF_ready_go && ID_allowin;
assign IF_to_ID_valid = IF_valid && IF_ready_go;

always @(posedge clk)begin
    if(~resetn)begin
        IF_valid <= 1'b0;
    end
    else if(IF_allowin) begin
        IF_valid <= validin;
    end
    else if(br_taken_cancel)begin
        IF_valid <= 1'b0;
    end
end

always @(posedge clk)begin
    if (~resetn) begin
        if_pc <= 32'h1BFFFFFC; 
    end
    else if(validin && IF_allowin)begin
        if_pc <= nextpc;
    end
end
//INST RAM
assign inst_sram_en    = validin && IF_allowin;
assign inst_sram_we    = 4'b0;
assign inst_sram_addr  = nextpc;
assign inst_sram_wdata = 32'b0; 
assign if_inst         = inst_sram_rdata;
endmodule