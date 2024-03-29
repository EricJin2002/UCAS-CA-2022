`include "macro.vh"

module EXE_stage(
    input  wire        clk,
    input  wire        resetn,
    // bus
    input  wire [`ID_to_EXE_LEN  - 1: 0] ID_to_EXE_BUS,
    input  wire [                  6: 0] ID_to_EXE_mul_div_op,
    output wire [`EXE_to_MEM_LEN - 1: 0] EXE_to_MEM_BUS,
    output wire [`EXE_RF_LEN     - 1: 0] EXE_RF_BUS,
    // read csr
    output wire [13: 0] csr_num,
    input  wire [31: 0] csr_rvalue,
    // data sram interface
    // output wire        data_sram_en,     
    output wire        data_sram_req,
    output wire        data_sram_wr,
    output wire [ 1:0] data_sram_size,
    output wire [ 3:0] data_sram_wstrb,
    output wire [31:0] data_sram_addr,
    output wire [31:0] data_sram_wdata,
    input  wire        data_sram_addr_ok,
    
    input  wire ID_to_EXE_valid,
    input  wire MEM_allowin,
    output wire EXE_allowin,
    output wire EXE_to_MEM_valid,

    input  wire        ertn_flush,
    input  wire        wb_ex,
    input  wire        mem_ertn,
    input  wire        mem_ex,
    input  wire        wb_refetch,
    input  wire        mem_refetch,

    input  wire [63:0] stable_cnt,
    
    output wire [ 4:0] tlb_inst_op_to_csr,
    output wire [ 9:0] invtlb_asid,
    output wire [18:0] invtlb_vppn,
    output wire [ 4:0] invtlb_op,
    input  wire        MEM_to_EXE_block_tlbsrch,
    input  wire        WB_to_EXE_block_tlbsrch,

    output wire [31:0] mem_vaddr,
    input  wire [31:0] mem_paddr,
    output wire        mem_wr,
    input  wire        to_EXE_ex_ade,
    input  wire        to_EXE_ex_tlbr,
    input  wire        to_EXE_ex_pil,
    input  wire        to_EXE_ex_pis,
    input  wire        to_EXE_ex_ppi,
    input  wire        to_EXE_ex_pme
);


// EXE
reg  EXE_valid;
wire EXE_ready_go;
assign EXE_to_MEM_valid = EXE_valid && EXE_ready_go;
assign EXE_allowin = !EXE_valid || EXE_ready_go && MEM_allowin;

always @(posedge clk)begin
    if (~resetn) begin
        EXE_valid <= 1'b0;
    end else if (ertn_flush || wb_ex || wb_refetch) begin
        EXE_valid <= 1'b0;
    end else if (EXE_allowin) begin
        EXE_valid <= ID_to_EXE_valid;
    end
end


// ID to EXE
reg [`ID_to_EXE_LEN  - 1: 0] ID_to_EXE_BUS_temp;

always @(posedge clk)begin
    if (~resetn) begin
        ID_to_EXE_BUS_temp <= {`ID_to_EXE_LEN{1'b0}};
    end else if (ID_to_EXE_valid && EXE_allowin) begin
        ID_to_EXE_BUS_temp <= ID_to_EXE_BUS;
    end
end

wire [31: 0] exe_pc;
wire         gr_we;
wire [ 4: 0] dest;
wire [31: 0] mem_sum;
wire         mem_en;
wire [11: 0] alu_op;
wire [31: 0] alu_src1;
wire [31: 0] alu_src2;
wire [ 7: 0] store_load_op;
wire         rfrom_mem;
wire [ 6: 0] mul_div_op;
wire         rfrom_csr;
// wire [13: 0] csr_num;
wire         csr_we;
wire [31: 0] csr_wvalue;
wire [31: 0] csr_wmask;
wire         exe_ex_in;
wire         exe_ex_out;
wire [14: 0] exe_ex_code_in;
wire [14: 0] exe_ex_code_out;
wire [31: 0] exe_ex_vaddr_in;
wire [31: 0] exe_ex_vaddr_out;
wire         exe_refetch_in;
wire         exe_refetch_out;
wire         inst_ertn;
wire         rfrom_cntvl;
wire         rfrom_cntvh;
wire         rfrom_cntid;
wire [ 4: 0] tlb_inst_op;
assign {exe_pc,gr_we,dest,mem_sum,mem_en,alu_op,alu_src1,alu_src2,store_load_op,rfrom_mem,mul_div_op,rfrom_csr,csr_num,csr_we,csr_wvalue,csr_wmask,
    exe_ex_in,exe_ex_code_in,exe_ex_vaddr_in,inst_ertn,rfrom_cntvl,rfrom_cntvh,rfrom_cntid,exe_refetch_in,tlb_inst_op,invtlb_asid,invtlb_vppn,invtlb_op} = ID_to_EXE_BUS_temp;
//mul_div_op = {inst_mul_w,inst_mulh_w,inst_mulh_wu,inst_div_w,inst_mod_w,inst_div_wu,inst_mod_wu};


// alu
wire [31: 0] alu_result;
alu u_alu(
    .alu_op     (alu_op    ),
    .alu_src1   (alu_src1  ),
    .alu_src2   (alu_src2  ),
    .alu_result (alu_result)
);


// div
// signed
wire [63: 0] div_result_signed;
reg          div_valid_signed;
wire         div_divisor_ready_signed;
wire         div_dividend_ready_signed;
wire         div_dout_valid_signed;
div_gen_signed u_div_gen_signed(
    .aclk                   (clk),
    .s_axis_divisor_tdata   (alu_src2),
    .s_axis_divisor_tready  (div_divisor_ready_signed),
    .s_axis_divisor_tvalid  (div_valid_signed && EXE_valid && 
                                !(exe_ex_out || mem_ex || mem_ertn || wb_ex || ertn_flush || wb_refetch || mem_refetch)),
    .s_axis_dividend_tdata  (alu_src1),
    .s_axis_dividend_tready (div_dividend_ready_signed),
    .s_axis_dividend_tvalid (div_valid_signed && EXE_valid && 
                                !(exe_ex_out || mem_ex || mem_ertn || wb_ex || ertn_flush || wb_refetch || mem_refetch)),
    .m_axis_dout_tdata      (div_result_signed),
    .m_axis_dout_tvalid     (div_dout_valid_signed)
);

always @(posedge clk)begin
    if(~resetn)begin
        div_valid_signed <= 1'b0;
    end else if (ID_to_EXE_valid && EXE_allowin) begin
        div_valid_signed <= ID_to_EXE_mul_div_op[3] || ID_to_EXE_mul_div_op[2];
    end else if(div_divisor_ready_signed && div_dividend_ready_signed)begin
        div_valid_signed <= 1'b0;
    end
end

// unsigned
wire [63: 0] div_result_unsigned;
reg          div_valid_unsigned;
wire         div_divisor_ready_unsigned;
wire         div_dividend_ready_unsigned;
wire         div_dout_valid_unsigned;
div_gen_unsigned u_div_gen_unsigned(
    .aclk                   (clk),
    .s_axis_divisor_tdata   (alu_src2),
    .s_axis_divisor_tready  (div_divisor_ready_unsigned),
    .s_axis_divisor_tvalid  (div_valid_unsigned && EXE_valid && 
                                !(exe_ex_out || mem_ex || mem_ertn || wb_ex || ertn_flush || wb_refetch || mem_refetch)),
    .s_axis_dividend_tdata  (alu_src1),
    .s_axis_dividend_tready (div_dividend_ready_unsigned),
    .s_axis_dividend_tvalid (div_valid_unsigned && EXE_valid && 
                                !(exe_ex_out || mem_ex || mem_ertn || wb_ex || ertn_flush || wb_refetch || mem_refetch)),
    .m_axis_dout_tdata      (div_result_unsigned),
    .m_axis_dout_tvalid     (div_dout_valid_unsigned)
);

always @(posedge clk)begin
    if (~resetn) begin
        div_valid_unsigned <= 1'b0;
    end else if (ID_to_EXE_valid && EXE_allowin) begin
        div_valid_unsigned <= ID_to_EXE_mul_div_op[1] || ID_to_EXE_mul_div_op[0];
    end else if (div_divisor_ready_unsigned && div_dividend_ready_unsigned) begin
        div_valid_unsigned <= 1'b0;
    end
end


// mul
// wire [63: 0] unsigned_prod, signed_prod;
// assign unsigned_prod = alu_src1 * alu_src2;
// assign signed_prod = $signed(alu_src1) * $signed(alu_src2);

wire         mul_is_signed;
assign mul_is_signed = mul_div_op[5];

wire [65: 0] mul_result;
assign mul_result = $signed({mul_is_signed & alu_src1[31], alu_src1}) * $signed({mul_is_signed & alu_src2[31], alu_src2});

// wire [67: 0] mul_result_1;
// mul u_mul_1(
//     .mul1   ({{2{mul_is_signed & alu_src1[31]}}, alu_src1}),
//     .mul2   ({{2{mul_is_signed & alu_src2[31]}}, alu_src2}),
//     .ans    (mul_result_1)
// );

// wire [67: 0] mul_result_2;
// booth_multiplier u_mul_2(
//     .x  ({{2{mul_is_signed & alu_src1[31]}}, alu_src1}),
//     .y  ({{2{mul_is_signed & alu_src2[31]}}, alu_src2}),
//     .z  (mul_result_2)
// );

wire rfrom_mul;
assign rfrom_mul = mul_div_op[6] || mul_div_op[5] || mul_div_op[4];


// exe result
wire [31: 0] exe_result;
assign exe_result = /* {32{~rfrom_csr & mul_div_op[6]}}                    & mul_result[31: 0]
                  | {32{~rfrom_csr & (mul_div_op[5] | mul_div_op[4])}}  & mul_result[63:32]
                  |*/ 
                    {32{~rfrom_csr & mul_div_op[3]}}                    & div_result_signed[63:32]
                  | {32{~rfrom_csr & mul_div_op[2]}}                    & div_result_signed[31: 0]
                  | {32{~rfrom_csr & mul_div_op[1]}}                    & div_result_unsigned[63:32]
                  | {32{~rfrom_csr & mul_div_op[0]}}                    & div_result_unsigned[31: 0]
                  | {32{~rfrom_csr &  ~|mul_div_op}}                    & alu_result
                  | {32{rfrom_csr}}                                     & csr_rvalue
                  | {32{rfrom_cntvl}}                                   & stable_cnt[31: 0]
                  | {32{rfrom_cntvh}}                                   & stable_cnt[63:32];


// ready go
assign EXE_ready_go = (mul_div_op[3] || mul_div_op[2]) && div_dout_valid_signed 
                   || (mul_div_op[1] || mul_div_op[0]) && div_dout_valid_unsigned
                   || (|store_load_op) && data_sram_req && data_sram_addr_ok
                   || (tlb_inst_op[`TLB_INST_TLBSRCH] || tlb_inst_op[`TLB_INST_INVTLB]) && !MEM_to_EXE_block_tlbsrch && !WB_to_EXE_block_tlbsrch
                   || !(mul_div_op[3] || mul_div_op[2] || mul_div_op[1] || mul_div_op[0] || (|store_load_op) || tlb_inst_op[`TLB_INST_TLBSRCH] || tlb_inst_op[`TLB_INST_INVTLB])
                   || exe_ex_out;


// operand forwarding
// if EXE is invalid, EXE_dest is 0
assign EXE_RF_BUS = {{`DEST_LEN{gr_we & EXE_valid}} & dest,rfrom_mem,rfrom_mul,rfrom_cntid,exe_result,EXE_valid,csr_we,csr_num};


// for MMU
assign mem_vaddr = alu_result;
assign mem_wr    = data_sram_wr;


// data sram

assign data_sram_req   = (rfrom_mem || mem_en) && EXE_valid && MEM_allowin && resetn &&
                        !(exe_ex_out || mem_ex || mem_ertn || wb_ex || ertn_flush || wb_refetch || mem_refetch);

wire [3:0] mask;
assign mask = {
     alu_result[1] &  alu_result[0],
     alu_result[1] & ~alu_result[0],
    ~alu_result[1] &  alu_result[0],
    ~alu_result[1] & ~alu_result[0]
};

assign data_sram_wstrb = {4{store_load_op[`ST_W]}} & 4'b1111
                       | {4{store_load_op[`ST_H]}} & {{2{mask[2]}}, {2{mask[0]}}}
                       | {4{store_load_op[`ST_B]}} & mask;
assign data_sram_addr  = mem_paddr;
assign data_sram_wdata = {32{store_load_op[`ST_B]}} & {4{mem_sum[ 7:0]}} 
                       | {32{store_load_op[`ST_H]}} & {2{mem_sum[15:0]}}
                       | {32{store_load_op[`ST_W]}} & mem_sum[31:0];
assign data_sram_wr    = store_load_op[`ST_W] || store_load_op[`ST_H] || store_load_op[`ST_B];
assign data_sram_size  = {2{store_load_op[`LD_W] ||                          store_load_op[`ST_W]}} & 2'b10
                       | {2{store_load_op[`LD_H] || store_load_op[`LD_HU] || store_load_op[`ST_H]}} & 2'b01
                       | {2{store_load_op[`LD_B] || store_load_op[`LD_BU] || store_load_op[`ST_B]}} & 2'b00;


// exception
wire ex_ale;
assign ex_ale = (store_load_op[`ST_W] || store_load_op[`LD_W])                          && alu_result[1:0]!=2'b00
             || (store_load_op[`ST_H] || store_load_op[`LD_H] || store_load_op[`LD_HU]) && alu_result[0]!=1'b0;

wire ex_adem;
wire ex_tlbr;
wire ex_pil;
wire ex_pis;
wire ex_ppi;
wire ex_pme;
assign ex_adem  = to_EXE_ex_ade  && (rfrom_mem || mem_en) && EXE_valid && resetn;
assign ex_tlbr  = to_EXE_ex_tlbr && (rfrom_mem || mem_en) && EXE_valid && resetn; 
assign ex_pil   = to_EXE_ex_pil  && (rfrom_mem || mem_en) && EXE_valid && resetn;
assign ex_pis   = to_EXE_ex_pis  && (rfrom_mem || mem_en) && EXE_valid && resetn;
assign ex_ppi   = to_EXE_ex_ppi  && (rfrom_mem || mem_en) && EXE_valid && resetn;
assign ex_pme   = to_EXE_ex_pme  && (rfrom_mem || mem_en) && EXE_valid && resetn;

assign exe_ex_out       = exe_ex_in || ex_ale || ex_adem || ex_tlbr || ex_pil || ex_pis || ex_ppi || ex_pme;
assign exe_ex_code_out  = exe_ex_in ? exe_ex_code_in
                        : {15{ ex_adem}}                        & {`ESUBCODE_ADEM, `ECODE_ADE}
                        | {15{!ex_adem &&  ex_ale}}             & {9'b0, `ECODE_ALE}
                        | {15{!ex_adem && !ex_ale && ex_tlbr}}  & {9'b0, `ECODE_TLBR}
                        | {15{!ex_adem && !ex_ale && ex_pil}}   & {9'b0, `ECODE_PIL}
                        | {15{!ex_adem && !ex_ale && ex_pis}}   & {9'b0, `ECODE_PIS}
                        | {15{!ex_adem && !ex_ale && ex_ppi}}   & {9'b0, `ECODE_PPI}
                        | {15{!ex_adem && !ex_ale && ex_pme}}   & {9'b0, `ECODE_PME};
assign exe_ex_vaddr_out = exe_ex_in ? exe_ex_vaddr_in :
                          {32{ex_ale || ex_adem || ex_tlbr || ex_pil || ex_pis || ex_ppi || ex_pme}} & alu_result;

assign exe_refetch_out  = exe_refetch_in;


// tlb
assign tlb_inst_op_to_csr = {
    5{EXE_valid && MEM_allowin && resetn && !(exe_ex_in || mem_ex || mem_ertn || wb_ex || ertn_flush || wb_refetch || mem_refetch)}
} & tlb_inst_op & (`TLB_INST_TLBSRCH_MASK | `TLB_INST_INVTLB_MASK);


// EXE to MEM
wire [4:0] load_op;
assign load_op = store_load_op[4:0];

wire wait_store_ok;
assign wait_store_ok = store_load_op[`ST_B] || store_load_op[`ST_H] || store_load_op[`ST_W];

assign EXE_to_MEM_BUS = {exe_pc,gr_we,dest,exe_result,mem_sum,mem_en,load_op,rfrom_mem,csr_num,csr_we,csr_wvalue,csr_wmask,
    exe_ex_out,exe_ex_code_out,exe_ex_vaddr_out,inst_ertn,rfrom_cntid,mul_result[63:0],mul_div_op,wait_store_ok,exe_refetch_out,tlb_inst_op};

endmodule