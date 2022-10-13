`include "macro.vh"

module control_status_register(
    input  wire         clk,
    input  wire         resetn,

    input  wire [13: 0] csr_wnum,
    input  wire         csr_we,
    input  wire [31: 0] csr_wvalue,
    input  wire [31: 0] csr_wmask,
    input  wire [13: 0] csr_rnum,
    output wire [31: 0] csr_rvalue,

    input  wire [31: 0] wb_pc,
    input  wire [ 7: 0] hw_int_in,
    input  wire         ipi_int_in,
    output wire [31: 0] ex_ra,
    output wire [31: 0] ex_entry,
    output wire         has_int,
    input  wire         ertn_flush,
    input  wire         wb_ex,
    input  wire [ 5: 0] wb_ecode,
    input  wire [ 8: 0] wb_esubcode
);

wire reset;
assign reset = ~resetn;

// CRMD, PRMD, ESTAT, ERA, EENTRY, SAVE0~3
// reg  [31: 0] csr_crmd;
// reg  [31: 0] csr_prmd;
// reg  [31: 0] csr_estat;
// reg  [31: 0] csr_era;
// reg  [31: 0] csr_eentry;
// reg  [31: 0] csr_save0;
// reg  [31: 0] csr_save1;
// reg  [31: 0] csr_save2;
// reg  [31: 0] csr_save3;

reg  [ 1:0] csr_crmd_plv;
reg         csr_crmd_ie;
reg         csr_crmd_da;
reg         csr_crmd_pg;
reg  [ 1:0] csr_crmd_datf;
reg  [ 1:0] csr_crmd_datm;

reg  [ 1:0] csr_prmd_pplv;
reg         csr_prmd_pie;

reg  [12:0] csr_estat_is;
reg  [ 5:0] csr_estat_ecode;
reg  [ 8:0] csr_estat_esubcode;

reg  [31:0] csr_era_pc;

reg  [25:0] csr_eentry_va;

reg  [31:0] csr_save0_data;

reg  [31:0] csr_save1_data;

reg  [31:0] csr_save2_data;

reg  [31:0] csr_save3_data;


// todo: finish this
reg  [31:0] timer_cnt;

always @(posedge clk) begin
    if (reset) begin
        csr_crmd_plv <= 2'b0;
        csr_crmd_ie  <= 1'b0;
    end else if (wb_ex) begin
        csr_crmd_plv <= 2'b0;
        csr_crmd_ie  <= 1'b0;
    end else if (ertn_flush) begin
        csr_crmd_plv <= csr_prmd_pplv;
        csr_crmd_ie  <= csr_prmd_pie;
    end else if (csr_we && csr_wnum==`CSR_CRMD) begin
        csr_crmd_plv <= csr_wmask[`CSR_CRMD_PLV] & csr_wvalue[`CSR_CRMD_PLV]
                     | ~csr_wmask[`CSR_CRMD_PLV] & csr_crmd_plv;
        csr_crmd_ie  <= csr_wmask[`CSR_CRMD_IE] & csr_wvalue[`CSR_CRMD_IE]
                     | ~csr_wmask[`CSR_CRMD_IE] & csr_crmd_ie;
    end

    if (reset) begin
        csr_crmd_datm <= 2'b0;
        csr_crmd_datf <= 2'b0;
        csr_crmd_pg   <= 1'b0;
        csr_crmd_da   <= 1'b1;
    end else if (csr_we && csr_wnum==`CSR_CRMD) begin
        csr_crmd_datm <= csr_wmask[`CSR_CRMD_DATM] & csr_wvalue[`CSR_CRMD_DATM]
                      | ~csr_wmask[`CSR_CRMD_DATM] & csr_crmd_datm;
        csr_crmd_datf <= csr_wmask[`CSR_CRMD_DATF] & csr_wvalue[`CSR_CRMD_DATF]
                      | ~csr_wmask[`CSR_CRMD_DATF] & csr_crmd_datf;
        csr_crmd_pg   <= csr_wmask[`CSR_CRMD_PG] & csr_wvalue[`CSR_CRMD_PG]
                      | ~csr_wmask[`CSR_CRMD_PG] & csr_crmd_pg;
        csr_crmd_da   <= csr_wmask[`CSR_CRMD_DA] & csr_wvalue[`CSR_CRMD_DA]
                      | ~csr_wmask[`CSR_CRMD_DA] & csr_crmd_da;
    end
end

always @(posedge clk) begin
    if (wb_ex) begin
        csr_prmd_pplv <= csr_crmd_plv;
        csr_prmd_pie  <= csr_crmd_ie;
    end else if (csr_we && csr_wnum==`CSR_PRMD) begin
        csr_prmd_pplv <= csr_wmask[`CSR_PRMD_PPLV] & csr_wvalue[`CSR_PRMD_PPLV]
                      | ~csr_wmask[`CSR_PRMD_PPLV] & csr_prmd_pplv;
        csr_prmd_pie  <= csr_wmask[`CSR_PRMD_PIE] & csr_wvalue[`CSR_PRMD_PIE]
                      | ~csr_wmask[`CSR_PRMD_PIE] & csr_prmd_pie;
    end
end

always @(posedge clk) begin
    if (reset) begin
        csr_estat_is[1:0] <= 2'b0;
    end else if (csr_we && csr_wnum==`CSR_ESTAT) begin
        csr_estat_is[1:0] <= csr_wmask[`CSR_ESTAT_IS] & csr_wvalue[`CSR_ESTAT_IS]
                          | ~csr_wmask[`CSR_ESTAT_IS] & csr_estat_is[1:0];
    end

    csr_estat_is[9:2] <= hw_int_in[7:0];

    csr_estat_is[10] <= 1'b0;

    if (timer_cnt[31:0]==32'b0) begin
        csr_estat_is[11] <= 1'b1;
    end else if (csr_we && csr_wnum==`CSR_TICLR && csr_wmask[`CSR_TICLR_CLR]
                        && csr_wvalue[`CSR_TICLR_CLR]) begin
        csr_estat_is[11] <= 1'b0;
    end

    csr_estat_is[12] <= ipi_int_in;
end

always @(posedge clk) begin
    if (wb_ex) begin
        csr_estat_ecode     <= wb_ecode;
        csr_estat_esubcode  <= wb_esubcode;
    end
end

always @(posedge clk) begin
    if (wb_ex) begin
        csr_era_pc <= wb_pc;
    end else if (csr_we && csr_wnum==`CSR_ERA) begin
        csr_era_pc <= csr_wmask[`CSR_ERA_PC] & csr_wvalue[`CSR_ERA_PC]
                   | ~csr_wmask[`CSR_ERA_PC] & csr_era_pc;
    end
end

always @(posedge clk) begin
    if (csr_we && csr_wnum==`CSR_EENTRY) begin
        csr_eentry_va <= csr_wmask[`CSR_EENTRY_VA] & csr_wvalue[`CSR_EENTRY_VA]
                      | ~csr_wmask[`CSR_EENTRY_VA] & csr_eentry_va;
    end
end

always @(posedge clk) begin
    if (csr_we && csr_wnum==`CSR_SAVE0) begin
        csr_save0_data <= csr_wmask[`CSR_SAVE0_DATA] & csr_wvalue[`CSR_SAVE0_DATA]
                       | ~csr_wmask[`CSR_SAVE0_DATA] & csr_save0_data;
    end

    if (csr_we && csr_wnum==`CSR_SAVE1) begin
        csr_save1_data <= csr_wmask[`CSR_SAVE1_DATA] & csr_wvalue[`CSR_SAVE1_DATA]
                       | ~csr_wmask[`CSR_SAVE1_DATA] & csr_save1_data;
    end

    if (csr_we && csr_wnum==`CSR_SAVE2) begin
        csr_save2_data <= csr_wmask[`CSR_SAVE2_DATA] & csr_wvalue[`CSR_SAVE2_DATA]
                       | ~csr_wmask[`CSR_SAVE2_DATA] & csr_save2_data;
    end

    if (csr_we && csr_wnum==`CSR_SAVE3) begin
        csr_save3_data <= csr_wmask[`CSR_SAVE3_DATA] & csr_wvalue[`CSR_SAVE3_DATA]
                       | ~csr_wmask[`CSR_SAVE3_DATA] & csr_save3_data;
    end
end


assign csr_rvalue = {32{csr_rnum==`CSR_CRMD}}   & {23'b0, csr_crmd_datm, csr_crmd_datf, csr_crmd_pg, csr_crmd_da, csr_crmd_ie, csr_crmd_plv}
                  | {32{csr_rnum==`CSR_PRMD}}   & {29'b0, csr_prmd_pie, csr_prmd_pplv}
                  | {32{csr_rnum==`CSR_ESTAT}}  & {1'b0, csr_estat_esubcode, csr_estat_ecode, 3'b0, csr_estat_is}
                  | {32{csr_rnum==`CSR_ERA}}    & csr_era_pc
                  | {32{csr_rnum==`CSR_EENTRY}} & {csr_eentry_va, 6'b0}
                  | {32{csr_rnum==`CSR_SAVE0}}  & csr_save0_data
                  | {32{csr_rnum==`CSR_SAVE1}}  & csr_save1_data
                  | {32{csr_rnum==`CSR_SAVE2}}  & csr_save2_data
                  | {32{csr_rnum==`CSR_SAVE3}}  & csr_save3_data;


assign ex_entry = {csr_eentry_va, 6'b0};
assign ex_ra    = csr_era_pc;

endmodule