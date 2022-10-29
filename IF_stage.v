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
    input  wire [ 3: 0] IO_cnt,
    output wire        IF_to_ID_valid,
    output wire        IF_allowin,
    output wire        IF_blocked,

    input  wire        ertn_flush,
    input  wire        wb_ex
);

// input branch bus
wire        br_taken ;
wire        br_taken_cancel;
wire [31:0] br_target;

assign {br_target,br_taken,br_taken_cancel} = BR_BUS;  

reg  [3:0] current_state;
reg  [3:0] next_state;

always @(posedge clk) begin
    if (~resetn) begin
        current_state <= `IF_INIT;
    end else begin
        current_state <= next_state;
    end
end

always @(*) begin
    if (~resetn) begin
        current_state <= `IF_INIT;
    end else begin
        case (current_state)
            `IF_INIT : begin
                if(wb_ex || ertn_flush || br_taken_cancel)
                    next_state <= `IF_CANCEL;
                else 
                    if(inst_sram_data_ok && !ID_allowin)
                        next_state <= `IF_BLOCKED1;
                    else 
                        next_state <= `IF_INIT; 
            end
            `IF_BLOCKED1 : begin
                if(wb_ex || ertn_flush || br_taken_cancel)
                    next_state <= `IF_CANCEL;
                else 
                    if(!inst_sram_data_ok && ID_allowin)
                        next_state <= `IF_INIT;
                    else if(inst_sram_data_ok && !ID_allowin)
                            next_state <= `IF_BLOCKED2;
                        else 
                            next_state <= `IF_BLOCKED1;
            end
            `IF_BLOCKED2 : begin
                if(wb_ex || ertn_flush || br_taken_cancel)
                    next_state <= `IF_CANCEL;
                else 
                    if(ID_allowin)
                        next_state <= `IF_BLOCKED1;
                    else
                        next_state <= `IF_BLOCKED2;
            end
            `IF_CANCEL : begin
                if(IO_cnt == 4'b0)
                    next_state <= `IF_INIT;
                else
            end
        endcase
    end
end




// IF to ID
reg  [31:0] if_pc;//fs_pc
wire [31:0] if_inst;
reg         if_ex;
reg  [14:0] if_ex_code;
reg  [31:0] if_ex_vaddr;

assign IF_to_ID_BUS = {if_pc,if_inst,if_ex,if_ex_code,if_ex_vaddr};

reg  IF_valid;
wire IF_ready_go;


// IF
assign IF_allowin = !IF_valid || IF_ready_go && ID_allowin;
assign IF_to_ID_valid = IF_valid && IF_ready_go;

always @(posedge clk) begin
    if (~resetn) begin
        IF_valid <= 1'b0;
    end else if (IF_allowin) begin
        IF_valid <= preIF_to_IF_valid;
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
        if_ex <= 1'b0;
    end else if (preIF_to_IF_valid && IF_allowin) begin
        if_ex <= preif_ex;
    end
end

always @(posedge clk) begin
    if (preIF_to_IF_valid && IF_allowin) begin
        if_ex_code  <= preif_ex_code;
        if_ex_vaddr <= preif_ex_vaddr;
    end
end


// ready go
assign IF_ready_go      = inst_sram_data_ok;


// inst sram
assign if_inst         = inst_sram_rdata;

endmodule