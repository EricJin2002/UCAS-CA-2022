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
    output wire [31: 0] ex_entry,
    output wire         has_int,
    input  wire         ertn_flush,
    input  wire         wb_ex,
    input  wire [ 5: 0] wb_ecode,
    input  wire [ 8: 0] wb_esubcode
);

// CRMD, PRMD, ESTAT, ERA, EENTRY, SAVE0~3
reg  [31: 0] csr_crmd;
reg  [31: 0] csr_prmd;
reg  [31: 0] csr_estat;
reg  [31: 0] csr_era;
reg  [31: 0] csr_eentry;
reg  [31: 0] csr_save0;
reg  [31: 0] csr_save1;
reg  [31: 0] csr_save2;
reg  [31: 0] csr_save3;



endmodule