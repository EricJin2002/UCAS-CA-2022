`include "macro.vh"

module IF_stage(
    input  wire        clk,
    input  wire        resetn,
    // inst sram interface
    input  wire        inst_sram_data_ok,
    input  wire [31:0] inst_sram_rdata,
    // bus
    input  wire [`BR_BUS_LEN      - 1: 0] BR_BUS,
    input  wire [`preIF_to_IF_LEN - 1: 0] preIF_to_IF_BUS,
    output wire [`IF_to_ID_LEN    - 1: 0] IF_to_ID_BUS,
    
    input  wire        ID_allowin,
    input  wire        preIF_to_IF_valid,
    input  wire [3: 0] IO_cnt,
    output wire        IF_to_ID_valid,
    output wire        IF_allowin,

    output reg  [31:0] if_pc,

    input  wire        ertn_flush,
    input  wire        wb_ex
);
reg  [3:0] current_state;
reg  [3:0] next_state;

reg  IF_valid;
wire IF_ready_go;
reg  data_ok_r;

assign IF_ready_go = (data_ok_r || inst_sram_data_ok) && (current_state != `IF_CANCEL);

// input branch bus
wire        br_taken ;
wire        br_taken_cancel;
wire [31:0] br_target;

assign {br_target,br_taken,br_taken_cancel} = BR_BUS;  


always @(posedge clk) begin
    if (~resetn) begin
        current_state <= `IF_INIT;
    end else begin
        current_state <= next_state;
    end
end

always @(*) begin
    if (~resetn) begin
        next_state <= `IF_INIT;
    end else begin
        case (current_state)
            `IF_INIT : begin
                if((wb_ex || ertn_flush || br_taken_cancel) && !IF_allowin && !IF_ready_go)
                    next_state <= `IF_CANCEL;
                else 
                    next_state <= `IF_INIT; 
                    // if(inst_sram_data_ok && !ID_allowin)
                    //     next_state <= `IF_BLOCKED1;
                    // else 
                        
            end
            `IF_CANCEL : begin
                if(IO_cnt == 4'b1 && inst_sram_data_ok)
                    next_state <= `IF_INIT;
                else 
                    next_state <= `IF_CANCEL;
            end
            // `IF_BLOCKED1 : begin
            //     if(wb_ex || ertn_flush || br_taken_cancel)
            //         next_state <= `IF_CANCEL;
            //     else 
            //         if(!inst_sram_data_ok && ID_allowin)
            //             next_state <= `IF_INIT;
            //         else if(inst_sram_data_ok && !ID_allowin)
            //                 next_state <= `IF_BLOCKED2;
            //             else 
            //                 next_state <= `IF_BLOCKED1;
            // end
            // `IF_BLOCKED2 : begin
            //     if(wb_ex || ertn_flush || br_taken_cancel)
            //         next_state <= `IF_CANCEL;
            //     else 
            //         if(ID_allowin)
            //             next_state <= `IF_BLOCKED1;
            //         else
            //             next_state <= `IF_BLOCKED2;
            // end

        endcase
    end
end

wire [31:0] preif_pc_r;
wire [31:0] nextpc;
wire        if_ex_in;
wire [14:0] if_ex_code_in;
wire [31:0] if_ex_vaddr_in;
assign {preif_pc_r,nextpc,if_ex_in,if_ex_code_in,if_ex_vaddr_in} = preIF_to_IF_BUS;


// IF to ID
wire [31:0] if_inst_out;
wire [31:0] if_inst;
reg  [31:0] if_inst_r;
reg         if_ex_out;
reg  [14:0] if_ex_code_out;
reg  [31:0] if_ex_vaddr_out;

assign if_inst_out  = {32{ inst_sram_data_ok             }} & if_inst 
                    | {32{~inst_sram_data_ok && data_ok_r}} & if_inst_r;

assign IF_to_ID_BUS = {if_pc,if_inst_out,if_ex_out,if_ex_code_out,if_ex_vaddr_out};




// IF
assign IF_allowin = (!IF_valid || IF_ready_go && ID_allowin) && current_state == `IF_INIT;
assign IF_to_ID_valid = IF_valid && IF_ready_go;

always @(posedge clk) begin
    if (~resetn) begin
        IF_valid <= 1'b0;
    end else if (IF_allowin) begin
        IF_valid <= preIF_to_IF_valid;
    end else if (ertn_flush || wb_ex) begin
        IF_valid <= 1'b0; 
    end else if (br_taken_cancel) begin
        IF_valid <= 1'b0;
    end
end

always @(posedge clk) begin
    if (~resetn) begin
        if_pc <= 32'h1BFFFFFC; 
    end else if (preIF_to_IF_valid && IF_allowin) begin
        if_pc <= nextpc;
    end
end


// exception
always @(posedge clk) begin
    if (~resetn) begin
        if_ex_out <= 1'b0;
    end else if (preIF_to_IF_valid && IF_allowin) begin
        if_ex_out <= if_ex_in;
        if_ex_code_out  <= if_ex_code_in;
        if_ex_vaddr_out <= if_ex_vaddr_in;
    end
end


always @(posedge clk) begin
    if(~resetn) begin
        data_ok_r <= 1'b0;
        if_inst_r <= 32'h00000000;
    end else if (wb_ex || ertn_flush) begin
        data_ok_r <= 1'b0;
        if_inst_r <= 32'h00000000;          
    end else if ((inst_sram_data_ok && current_state == `IF_INIT) && !(IF_to_ID_valid && ID_allowin)) begin
        data_ok_r <= 1'b1;
        if_inst_r <= inst_sram_rdata;
    end else if (IF_to_ID_valid && ID_allowin) begin 
        data_ok_r <= 1'b0;
        if_inst_r <= 32'h00000000;
    end
end


// inst sram
assign if_inst         = inst_sram_rdata;

endmodule