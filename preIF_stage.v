`include "macro.vh"

module preIF_stage(
    input  wire        clk,
    input  wire        resetn,
    // inst sram interface
    output wire        inst_sram_req,
    output wire        inst_sram_wr,
    output wire [ 1:0] inst_sram_size,
    output wire [ 3:0] inst_sram_wstrb,
    output wire [31:0] inst_sram_addr,
    output wire [31:0] inst_sram_wdata,
    input  wire        inst_sram_addr_ok,
    // bus
    input  wire [`BR_BUS_LEN      - 1: 0] BR_BUS,
    output wire [`preIF_to_IF_LEN - 1: 0] preIF_to_IF_BUS,
    
    input  wire        IF_allowin,
    input  wire        preIF_validin, // todo: assign preIF_validin = resetn;
    output wire        preIF_to_IF_valid,
    output wire        preIF_allowin,

    input  wire [31:0] if_pc,

    input  wire        ertn_flush,
    input  wire        wb_ex,
    input  wire [31:0] ex_ra,
    input  wire [31:0] ex_entry
);


// input branch bus
wire        br_taken ;
wire        br_taken_cancel;
wire [31:0] br_target;

assign {br_target,br_taken,br_taken_cancel} = BR_BUS;  


// preIF
reg  preIF_valid;
wire preIF_ready_go;
assign preIF_to_IF_valid = preIF_valid && preIF_ready_go;
assign preIF_allowin = !preIF_valid || preIF_ready_go && IF_allowin;

always @(posedge clk) begin
    if (~resetn) begin
        preIF_valid <= 1'b0;
    end else if (preIF_allowin) begin
        preIF_valid <= preIF_validin;
    end
end


// // fsm
// reg  [1:0] current_state;
// reg  [1:0] next_state;
// 
// always @(posedge clk) begin
//     if (~resetn) begin
//         current_state <= `preIF_INIT;
//     end else begin
//         current_state <= next_state;
//     end
// end
// 
// always @(*) begin
//     if (~resetn) begin
//         next_state <= `preIF_INIT;
//     end else begin
//         case (current_state)
//             `preIF_INIT: begin
//                 if (inst_sram_addr_ok && !IF_allowin) begin // fixme: addr ok will randomly send 
//                     next_state <= `preIF_BLOCKED;
//                 end else begin
//                     next_state <= `preIF_INIT;
//                 end
//             end
//             `preIF_BLOCKED: begin
//                 if (IF_allowin) begin
//                     next_state <= `preIF_INIT;
//                 end else begin
//                     next_state <= `preIF_BLOCKED;
//                 end
//                 
//             end
//         endcase
//     end
// end


// preIF
wire [31:0] seq_pc;
wire [31:0] next_pc;
reg  [31:0] preif_pc;
reg  [31:0] preif_pc_r;
reg  [ 1:0] preif_pc_plv;

reg         wb_ex_r;
reg         ertn_flush_r;
reg         br_taken_cancel_r;

reg  [31:0] ex_ra_r;
reg  [31:0] ex_entry_r;
reg  [31:0] br_target_r;

always @(posedge clk) begin
    if (~resetn) begin
        wb_ex_r <= 0;
        ertn_flush_r <= 0;
        br_taken_cancel_r <= 0;
        ex_ra_r <= 32'b0;
        ex_entry_r <= 32'b0;
        br_target_r <= 32'b0;
    end else if(wb_ex && (preif_pc_plv < 2) && (~preIF_ready_go))begin
        wb_ex_r <= wb_ex;
        ex_entry_r <= ex_entry;
    end else if(ertn_flush && (preif_pc_plv < 2) && (~preIF_ready_go))begin
        ertn_flush_r <= ertn_flush;
        ex_ra_r <= ex_ra;
    end else if(br_taken_cancel && (preif_pc_plv < 1) && (~preIF_ready_go))begin
        br_taken_cancel_r <= br_taken_cancel;
        br_target_r <= br_target;
    end else if(preIF_ready_go)begin
        wb_ex_r <= 0;
        ertn_flush_r <= 0;
        br_taken_cancel_r <= 0;
        ex_ra_r <= 32'b0;
        ex_entry_r <= 32'b0;
        br_target_r <= 32'b0;
    end
end
// preIF
assign seq_pc = if_pc + 3'h4;
assign next_pc = {32{ wb_ex_r                                                                                     }} & ex_entry_r
               | {32{!wb_ex_r &&  wb_ex                                                                           }} & ex_entry
               | {32{!wb_ex_r && !wb_ex &&  ertn_flush_r                                                          }} & ex_ra_r
               | {32{!wb_ex_r && !wb_ex && !ertn_flush_r  &&  ertn_flush                                          }} & ex_ra
               | {32{!wb_ex_r && !wb_ex && !ertn_flush_r  && !ertn_flush &&  br_taken_cancel_r                    }} & br_target_r
               | {32{!wb_ex_r && !wb_ex && !ertn_flush_r  && !ertn_flush && !br_taken_cancel_r &&  br_taken_cancel}} & br_target
               | {32{!wb_ex_r && !wb_ex && !ertn_flush_r  && !ertn_flush && !br_taken_cancel_r && !br_taken_cancel}} & seq_pc;
// preif_pc_plv 
// 2: exception
// 1: branch
// 0: normal
always @(posedge clk) begin
    if (~resetn) begin
        preif_pc <= 32'h1BFFFFFC; // todo
        preif_pc_plv <= 2'h0;
    end else if ((wb_ex || ertn_flush) && preif_pc_plv<2 && (~preIF_ready_go)) begin
        preif_pc <= next_pc;
        preif_pc_plv <= 2'h2;
    end else if (br_taken_cancel && preif_pc_plv<1 && (~preIF_ready_go)) begin
        preif_pc <= br_target;
        preif_pc_plv <= 2'h1;
    end else if (preIF_ready_go) begin
        preif_pc <= seq_pc;
        preif_pc_plv <= 2'h0;
    end
end

always @(posedge clk) begin
    if(~resetn)begin
        preif_pc_r <= 32'h1BFFFFFC;
    end else begin
        preif_pc_r <= preif_pc;
    end
end


// ready go
assign preIF_ready_go   = inst_sram_req && inst_sram_addr_ok; // || current_state == `preIF_BLOCKED;


// exception
wire ex_adef;
assign ex_adef = next_pc[1:0] != 2'b00;

wire        preif_ex;
wire [14:0] preif_ex_code;
wire [31:0] preif_ex_vaddr;

assign preif_ex         = ex_adef;
assign preif_ex_code    = {15{ex_adef}} & {`ESUBCODE_ADEF, `ECODE_ADE};
assign preif_ex_vaddr   = /*{32{ex_adef}} &*/ next_pc;


// preIF to IF
assign preIF_to_IF_BUS = {preif_pc_r,next_pc,preif_ex,preif_ex_code,preif_ex_vaddr};


// inst sram
assign inst_sram_req   = IF_allowin && preIF_valid && resetn && !(br_taken && !br_taken_cancel);
assign inst_sram_wr    = 1'b0;
assign inst_sram_size  = 2'b10;
assign inst_sram_wstrb = 4'b0;
assign inst_sram_addr  =  {next_pc[31:2],2'b00};
assign inst_sram_wdata = 32'b0; 

endmodule