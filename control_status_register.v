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


endmodule