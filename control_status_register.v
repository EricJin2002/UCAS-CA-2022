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
    input  wire [31: 0] wb_vaddr,
    input  wire [ 7: 0] hw_int_in,
    input  wire         ipi_int_in,
    output wire [31: 0] ex_ra,
    output wire [31: 0] ex_entry,
    output wire         has_int,
    input  wire         ertn_flush,
    input  wire         wb_ex,
    input  wire [ 5: 0] wb_ecode,
    input  wire [ 8: 0] wb_esubcode,

    output reg  [63: 0] stable_cnt,
    output wire [31: 0] stable_cnt_tid,

    input  wire [ 4: 0] tlb_inst_op,
    input  wire [ 9: 0] invtlb_asid,
    input  wire [18: 0] invtlb_vppn,
    input  wire [ 4: 0] invtlb_op,

    // to PreIF
    input  wire [31: 0] next_pc_vaddr,
    output wire [31: 0] next_pc_paddr,
    output wire         to_PreIF_ex_ade,
    output wire         to_PreIF_ex_tlbr,
    output wire         to_PreIF_ex_pif,
    output wire         to_PreIF_ex_ppi,

    // to EXE
    input  wire [31: 0] mem_vaddr,
    output wire [31: 0] mem_paddr,
    input  wire         mem_wr,
    output wire         to_EXE_ex_ade,
    output wire         to_EXE_ex_tlbr,
    output wire         to_EXE_ex_pil, // load
    output wire         to_EXE_ex_pis, // store
    output wire         to_EXE_ex_ppi,
    output wire         to_EXE_ex_pme
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

reg  [12:0] csr_ecfg_lie;

reg  [31:0] csr_badv_vaddr;

reg  [31:0] csr_tid_tid;

reg         csr_tcfg_en;
reg         csr_tcfg_periodic;
reg  [29:0] csr_tcfg_initval;

//reg  [31:0]   csr_tval_timeval;

wire        csr_ticlr_clr;

// vaddr
wire        wb_ex_addr_err;

// timer counter
wire [31:0] tcfg_next_value;
wire [31:0] csr_tval;
reg  [31:0] timer_cnt;

// tlb
reg  [ 3:0] csr_tlbidx_index;
reg  [ 5:0] csr_tlbidx_ps;
reg         csr_tlbidx_ne;

reg  [18:0] csr_tlbehi_vppn;

reg         csr_tlbelo0_v;
reg         csr_tlbelo0_d;
reg  [ 1:0] csr_tlbelo0_plv;
reg  [ 1:0] csr_tlbelo0_mat;
reg         csr_tlbelo0_g;
reg  [19:0] csr_tlbelo0_ppn;

reg         csr_tlbelo1_v;
reg         csr_tlbelo1_d;
reg  [ 1:0] csr_tlbelo1_plv;
reg  [ 1:0] csr_tlbelo1_mat;
reg         csr_tlbelo1_g;
reg  [19:0] csr_tlbelo1_ppn;

reg  [ 9:0] csr_asid_asid;
wire [ 7:0] csr_asid_asidbits;

reg  [25:0] csr_tlbrentry_pa;

reg         csr_dmw0_plv0;
reg         csr_dmw0_plv3;
reg  [ 1:0] csr_dmw0_mat;
reg  [ 2:0] csr_dmw0_pseg;
reg  [ 2:0] csr_dmw0_vseg;

reg         csr_dmw1_plv0;
reg         csr_dmw1_plv3;
reg  [ 1:0] csr_dmw1_mat;
reg  [ 2:0] csr_dmw1_pseg;
reg  [ 2:0] csr_dmw1_vseg;


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
    end else if (wb_ex && wb_ecode==`ECODE_TLBR) begin
        csr_crmd_datm <= csr_crmd_datm;
        csr_crmd_datf <= csr_crmd_datf;
        csr_crmd_pg   <= 1'b0;
        csr_crmd_da   <= 1'b1;
    end else if (ertn_flush && csr_estat_ecode==6'h3f) begin
        csr_crmd_datm <= csr_crmd_datm;
        csr_crmd_datf <= csr_crmd_datf;
        csr_crmd_pg   <= 1'b1;
        csr_crmd_da   <= 1'b0;
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

always @(posedge clk) begin
    if (reset) begin
        csr_ecfg_lie <= 13'b0;
    end else if (csr_we && csr_wnum==`CSR_ECFG) begin
        csr_ecfg_lie <= csr_wmask[`CSR_ECFG_LIE] & csr_wvalue[`CSR_ECFG_LIE]
                     | ~csr_wmask[`CSR_ECFG_LIE] & csr_ecfg_lie;
    end
end

assign wb_ex_addr_err = wb_ecode==`ECODE_ADE || wb_ecode==`ECODE_ALE
                     || wb_ecode==`ECODE_TLBR || wb_ecode==`ECODE_PIF || wb_ecode==`ECODE_PIL || wb_ecode==`ECODE_PIS
                     || wb_ecode==`ECODE_PPI || wb_ecode==`ECODE_PME;
always @(posedge clk) begin
    if (wb_ex && wb_ex_addr_err) begin
        csr_badv_vaddr <= (wb_ecode==`ECODE_ADE && wb_esubcode==`ESUBCODE_ADEF) ? wb_pc : wb_vaddr;
    end else if (csr_we && csr_wnum==`CSR_BADV) begin
        csr_badv_vaddr <= csr_wmask[`CSR_BADV_VADDR] & csr_wvalue[`CSR_BADV_VADDR]
                       | ~csr_wmask[`CSR_BADV_VADDR] & csr_badv_vaddr;
    end
end

wire [ 8:0] coreid_in;
assign coreid_in = 9'b0;
always @(posedge clk) begin
    if (reset) begin
        csr_tid_tid <= coreid_in;
    end else if (csr_we && csr_wnum==`CSR_TID) begin
        csr_tid_tid <= csr_wmask[`CSR_TID_TID] & csr_wvalue[`CSR_TID_TID]
                    | ~csr_wmask[`CSR_TID_TID] & csr_tid_tid;
    end
end

always @(posedge clk) begin
    if (reset) begin
        csr_tcfg_en <= 1'b0;
    end else if (csr_we && csr_wnum==`CSR_TCFG) begin
        csr_tcfg_en <= csr_wmask[`CSR_TCFG_EN] & csr_wvalue[`CSR_TCFG_EN]
                    | ~csr_wmask[`CSR_TCFG_EN] & csr_tcfg_en;
    end
    
    if (csr_we && csr_wnum==`CSR_TCFG) begin
        csr_tcfg_periodic <= csr_wmask[`CSR_TCFG_PERIODIC] & csr_wvalue[`CSR_TCFG_PERIODIC]
                          | ~csr_wmask[`CSR_TCFG_PERIODIC] & csr_tcfg_periodic;
        csr_tcfg_initval <= csr_wmask[`CSR_TCFG_INITVAL] & csr_wvalue[`CSR_TCFG_INITVAL]
                         | ~csr_wmask[`CSR_TCFG_INITVAL] & csr_tcfg_initval;
    end
end

assign tcfg_next_value = csr_wmask[31:0] & csr_wvalue[31:0]
                      | ~csr_wmask[31:0] & {csr_tcfg_initval, csr_tcfg_periodic, csr_tcfg_en};
always @(posedge clk) begin
    if (reset) begin
        timer_cnt <= 32'hffffffff;
    end else if (csr_we && csr_wnum==`CSR_TCFG && tcfg_next_value[`CSR_TCFG_EN]) begin
        timer_cnt <= {tcfg_next_value[`CSR_TCFG_INITVAL], 2'b0};
    end else if (csr_tcfg_en && timer_cnt!=32'hffffffff) begin
        if (timer_cnt[31:0]==32'b0 && csr_tcfg_periodic) begin
            timer_cnt <= {csr_tcfg_initval, 2'b0};
        end else begin
            timer_cnt <= timer_cnt - 1'b1;
        end
    end
end
assign csr_tval = timer_cnt[31:0];

assign csr_ticlr_clr = 1'b0;

// tlb
// search port 0 (for fetch)
wire [              18:0] s0_vppn;
wire                      s0_va_bit12;
wire [               9:0] s0_asid;
wire                      s0_found;
wire [               3:0] s0_index;
wire [              19:0] s0_ppn;
wire [               5:0] s0_ps;
wire [               1:0] s0_plv;
wire [               1:0] s0_mat;
wire                      s0_d;
wire                      s0_v;
// search port 1 (for load/store)
wire [              18:0] s1_vppn;
wire                      s1_va_bit12;
wire [               9:0] s1_asid;
wire                      s1_found;
wire [               3:0] s1_index;
wire [              19:0] s1_ppn;
wire [               5:0] s1_ps;
wire [               1:0] s1_plv;
wire [               1:0] s1_mat;
wire                      s1_d;
wire                      s1_v;
// invtlb opcode
wire                      invtlb_valid;
// wire [               4:0] invtlb_op;
// write port
wire                      we; //w(rite) e(nable)
wire [               3:0] w_index;
wire                      w_e;
wire [              18:0] w_vppn;
wire [               5:0] w_ps;
wire [               9:0] w_asid;
wire                      w_g;
wire [              19:0] w_ppn0;
wire [               1:0] w_plv0;
wire [               1:0] w_mat0;
wire                      w_d0;
wire                      w_v0;
wire [              19:0] w_ppn1;
wire [               1:0] w_plv1;
wire [               1:0] w_mat1;
wire                      w_d1;
wire                      w_v1;
// read port
wire [               3:0] r_index;
wire                      r_e;
wire [              18:0] r_vppn;
wire [               5:0] r_ps;
wire [               9:0] r_asid;
wire                      r_g;
wire [              19:0] r_ppn0;
wire [               1:0] r_plv0;
wire [               1:0] r_mat0;
wire                      r_d0;
wire                      r_v0;
wire [              19:0] r_ppn1;
wire [               1:0] r_plv1;
wire [               1:0] r_mat1;
wire                      r_d1;
wire                      r_v1;

tlb tlb_u(
    .clk(clk),
    .s0_vppn(s0_vppn),
    .s0_va_bit12(s0_va_bit12),
    .s0_asid(s0_asid),
    .s0_found(s0_found),
    .s0_index(s0_index),
    .s0_ppn(s0_ppn),
    .s0_ps(s0_ps),
    .s0_plv(s0_plv),
    .s0_mat(s0_mat),
    .s0_d(s0_d),
    .s0_v(s0_v),
    .s1_vppn(s1_vppn),
    .s1_va_bit12(s1_va_bit12),
    .s1_asid(s1_asid),
    .s1_found(s1_found),
    .s1_index(s1_index),
    .s1_ppn(s1_ppn),
    .s1_ps(s1_ps),
    .s1_plv(s1_plv),
    .s1_mat(s1_mat),
    .s1_d(s1_d),
    .s1_v(s1_v),
    .invtlb_valid(invtlb_valid),
    .invtlb_op(invtlb_op),
    .we(we),
    .w_index(w_index),
    .w_e(w_e),
    .w_vppn(w_vppn),
    .w_ps(w_ps),
    .w_asid(w_asid),
    .w_g(w_g),
    .w_ppn0(w_ppn0),
    .w_plv0(w_plv0),
    .w_mat0(w_mat0),
    .w_d0(w_d0),
    .w_v0(w_v0),
    .w_ppn1(w_ppn1),
    .w_plv1(w_plv1),
    .w_mat1(w_mat1),
    .w_d1(w_d1),
    .w_v1(w_v1),
    .r_index(r_index),
    .r_e(r_e),
    .r_vppn(r_vppn),
    .r_ps(r_ps),
    .r_asid(r_asid),
    .r_g(r_g),
    .r_ppn0(r_ppn0),
    .r_plv0(r_plv0),
    .r_mat0(r_mat0),
    .r_d0(r_d0),
    .r_v0(r_v0),
    .r_ppn1(r_ppn1),
    .r_plv1(r_plv1),
    .r_mat1(r_mat1),
    .r_d1(r_d1),
    .r_v1(r_v1)
);

// generate random index for tlbfill
reg  [3:0] rand_index;
always @(posedge clk) begin
    rand_index <= $random;
end

assign we = tlb_inst_op[`TLB_INST_TLBWR] || tlb_inst_op[`TLB_INST_TLBFILL];
assign invtlb_valid = tlb_inst_op[`TLB_INST_INVTLB];


// for preIF
wire next_pc_is_DA_mode;
wire next_pc_is_PG_DMW_mode;
wire next_pc_is_PG_TLB_mode;
wire next_pc_in_DMW0;
wire next_pc_in_DMW1;
assign next_pc_is_DA_mode       =  csr_crmd_da && ~csr_crmd_pg;
assign next_pc_is_PG_DMW_mode   = ~csr_crmd_da &&  csr_crmd_pg && (next_pc_in_DMW0 || next_pc_in_DMW1);
assign next_pc_is_PG_TLB_mode   = ~csr_crmd_da &&  csr_crmd_pg && ~next_pc_in_DMW0 && ~next_pc_in_DMW1;
assign next_pc_in_DMW0          = next_pc_vaddr[31:29]==csr_dmw0_vseg && (
    csr_dmw0_plv0 && csr_crmd_plv==2'b0 || csr_dmw0_plv3 && csr_crmd_plv==2'b11
);
assign next_pc_in_DMW1          = next_pc_vaddr[31:29]==csr_dmw1_vseg && (
    csr_dmw1_plv0 && csr_crmd_plv==2'b0 || csr_dmw1_plv3 && csr_crmd_plv==2'b11
);

assign s0_vppn      = next_pc_vaddr[31:13];
assign s0_va_bit12  = next_pc_vaddr[12];
assign s0_asid      = csr_asid_asid;

assign to_PreIF_ex_ade  = next_pc_vaddr[31] && next_pc_is_PG_TLB_mode; // || next_pc_is_PG_DMW_mode && (next_pc_in_DMW0 ? csr_dmw0_pseg[2] : csr_dmw1_pseg[2]);
assign to_PreIF_ex_tlbr = next_pc_is_PG_TLB_mode && ~s0_found;
assign to_PreIF_ex_pif  = next_pc_is_PG_TLB_mode &&  s0_found && ~s0_v;
assign to_PreIF_ex_ppi  = next_pc_is_PG_TLB_mode &&  s0_found &&  s0_v && (
    $unsigned(csr_crmd_plv) > $unsigned(s0_plv)
);

assign next_pc_paddr = wb_ex && wb_ecode==`ECODE_TLBR ? next_pc_vaddr   // due to the delayed write of CSR.CRMD.DA & CSR.CRMD.PG when TLBR
                     : {32{next_pc_is_DA_mode}}     & next_pc_vaddr
                     | {32{next_pc_is_PG_DMW_mode}} & {
                        next_pc_in_DMW0 ? csr_dmw0_pseg : csr_dmw1_pseg, 
                        next_pc_vaddr[28:0]}
                     | {32{next_pc_is_PG_TLB_mode}} & {
                        s0_ppn[19:10],
                        s0_ps==22 ? next_pc_vaddr[21:12] : s0_ppn[9:0],
                        next_pc_vaddr[11:0]
                    };


// for EXE
wire mem_addr_is_DA_mode;
wire mem_addr_is_PG_DMW_mode;
wire mem_addr_is_PG_TLB_mode;
wire mem_addr_in_DMW0;
wire mem_addr_in_DMW1;
assign mem_addr_is_DA_mode       =  csr_crmd_da && ~csr_crmd_pg;
assign mem_addr_is_PG_DMW_mode   = ~csr_crmd_da &&  csr_crmd_pg && (mem_addr_in_DMW0 || mem_addr_in_DMW1);
assign mem_addr_is_PG_TLB_mode   = ~csr_crmd_da &&  csr_crmd_pg && ~mem_addr_in_DMW0 && ~mem_addr_in_DMW1;
assign mem_addr_in_DMW0          = mem_vaddr[31:29]==csr_dmw0_vseg && (
    csr_dmw0_plv0 && csr_crmd_plv==2'b0 || csr_dmw0_plv3 && csr_crmd_plv==2'b11
);
assign mem_addr_in_DMW1          = mem_vaddr[31:29]==csr_dmw1_vseg && (
    csr_dmw1_plv0 && csr_crmd_plv==2'b0 || csr_dmw1_plv3 && csr_crmd_plv==2'b11
);

assign s1_vppn = {19{tlb_inst_op[`TLB_INST_TLBSRCH]}}                                       & csr_tlbehi_vppn
               | {19{tlb_inst_op[`TLB_INST_INVTLB]}}                                        & invtlb_vppn
               | {19{!tlb_inst_op[`TLB_INST_TLBSRCH] && !tlb_inst_op[`TLB_INST_INVTLB]}}    & mem_vaddr[31:13];
assign s1_va_bit12 = !tlb_inst_op[`TLB_INST_TLBSRCH] && !tlb_inst_op[`TLB_INST_INVTLB] && mem_vaddr[12];
assign s1_asid = {10{tlb_inst_op[`TLB_INST_TLBSRCH]}}                                       & csr_asid_asid
               | {10{tlb_inst_op[`TLB_INST_INVTLB]}}                                        & invtlb_asid
               | {10{!tlb_inst_op[`TLB_INST_TLBSRCH] && !tlb_inst_op[`TLB_INST_INVTLB]}}    & csr_asid_asid;

assign to_EXE_ex_ade  = mem_vaddr[31] && mem_addr_is_PG_TLB_mode; // || mem_addr_is_PG_DMW_mode && (mem_addr_in_DMW0 ? csr_dmw0_pseg[2] : csr_dmw1_pseg[2]);
assign to_EXE_ex_tlbr = mem_addr_is_PG_TLB_mode && ~s1_found;
assign to_EXE_ex_pil  = mem_addr_is_PG_TLB_mode &&  s1_found && ~s1_v && ~mem_wr;
assign to_EXE_ex_pis  = mem_addr_is_PG_TLB_mode &&  s1_found && ~s1_v &&  mem_wr;
assign to_EXE_ex_ppi  = mem_addr_is_PG_TLB_mode &&  s1_found &&  s1_v && (
    $unsigned(csr_crmd_plv) > $unsigned(s1_plv)
);
assign to_EXE_ex_pme  = mem_addr_is_PG_TLB_mode &&  s1_found &&  s1_v && !(
    $unsigned(csr_crmd_plv) > $unsigned(s1_plv)
) && mem_wr && ~s1_d;

assign mem_paddr = {32{mem_addr_is_DA_mode}}     & mem_vaddr
                 | {32{mem_addr_is_PG_DMW_mode}} & {
                    mem_addr_in_DMW0 ? csr_dmw0_pseg : csr_dmw1_pseg, 
                    mem_vaddr[28:0]}
                 | {32{mem_addr_is_PG_TLB_mode}} & {
                    s1_ppn[19:10],
                    s1_ps==22 ? mem_vaddr[21:12] : s1_ppn[9:0],
                    mem_vaddr[11:0]
                };


assign r_index = csr_tlbidx_index;
assign w_index = {4{tlb_inst_op[`TLB_INST_TLBWR]}} & csr_tlbidx_index
               | {4{tlb_inst_op[`TLB_INST_TLBFILL]}} & rand_index;
assign w_ps = csr_tlbidx_ps;
assign w_e = ~csr_tlbidx_ne || csr_estat_ecode==6'h3e && tlb_inst_op[`TLB_INST_TLBFILL];
always @(posedge clk) begin
    if (reset) begin
        csr_tlbidx_index <= 0;
        csr_tlbidx_ps <= 0;
        csr_tlbidx_ne <= 1;
    end else if (csr_we && csr_wnum==`CSR_TLBIDX) begin
        csr_tlbidx_index <= csr_wmask[`CSR_TLBIDX_INDEX] & csr_wvalue[`CSR_TLBIDX_INDEX]
                         | ~csr_wmask[`CSR_TLBIDX_INDEX] & csr_tlbidx_index;
        csr_tlbidx_ps <= csr_wmask[`CSR_TLBIDX_PS] & csr_wvalue[`CSR_TLBIDX_PS]
                      | ~csr_wmask[`CSR_TLBIDX_PS] & csr_tlbidx_ps;
        csr_tlbidx_ne <= csr_wmask[`CSR_TLBIDX_NE] & csr_wvalue[`CSR_TLBIDX_NE]
                      | ~csr_wmask[`CSR_TLBIDX_NE] & csr_tlbidx_ne;
    end else if (tlb_inst_op[`TLB_INST_TLBSRCH]) begin
        if (s1_found) begin
            csr_tlbidx_index <= s1_index;
            csr_tlbidx_ps <= csr_tlbidx_ps;
            csr_tlbidx_ne <= 0;
        end else begin
            csr_tlbidx_index <= csr_tlbidx_index;
            csr_tlbidx_ps <= csr_tlbidx_ps;
            csr_tlbidx_ne <= 1;
        end
    end else if (tlb_inst_op[`TLB_INST_TLBRD]) begin
        if (r_e) begin
            csr_tlbidx_index <= r_index;
            csr_tlbidx_ps <= r_ps;
            csr_tlbidx_ne <= 0;
        end else begin
            csr_tlbidx_index <= csr_tlbidx_index;
            csr_tlbidx_ps <= 0;
            csr_tlbidx_ne <= 1;
        end
    end
end

assign w_vppn = csr_tlbehi_vppn;
always @(posedge clk) begin
    if (reset) begin
        csr_tlbehi_vppn <= 0;
    end else if (csr_we && csr_wnum==`CSR_TLBEHI) begin
        csr_tlbehi_vppn <= csr_wmask[`CSR_TLBEHI_VPPN] & csr_wvalue[`CSR_TLBEHI_VPPN]
                        | ~csr_wmask[`CSR_TLBEHI_VPPN] & csr_tlbehi_vppn;
    end else if (wb_ex && (
        wb_ecode==`ECODE_TLBR || wb_ecode==`ECODE_PIF || wb_ecode==`ECODE_PIL || wb_ecode==`ECODE_PIS
        || wb_ecode==`ECODE_PPI || wb_ecode==`ECODE_PME
    )) begin
        csr_tlbehi_vppn <= wb_vaddr[31:13];
    end else if (tlb_inst_op[`TLB_INST_TLBRD]) begin
        if (r_e) begin
            csr_tlbehi_vppn <= r_vppn;
        end else begin
            csr_tlbehi_vppn <= 0;
        end
    end
end

assign w_g = csr_tlbelo0_g && csr_tlbelo1_g;
assign w_ppn0 = csr_tlbelo0_ppn;
assign w_v0 = csr_tlbelo0_v;
assign w_plv0 = csr_tlbelo0_plv;
assign w_mat0 = csr_tlbelo0_mat;
assign w_d0 = csr_tlbelo0_d;
assign w_ppn1 = csr_tlbelo1_ppn;
assign w_v1 = csr_tlbelo1_v;
assign w_plv1 = csr_tlbelo1_plv;
assign w_mat1 = csr_tlbelo1_mat;
assign w_d1 = csr_tlbelo1_d;
always @(posedge clk) begin
    if (reset) begin
        csr_tlbelo0_v <= 0;
        csr_tlbelo0_d <= 0;
        csr_tlbelo0_plv <= 0;
        csr_tlbelo0_mat <= 0;
        csr_tlbelo0_g <= 0;
        csr_tlbelo0_ppn <= 0;
    end else if (csr_we && csr_wnum==`CSR_TLBELO0) begin
        csr_tlbelo0_v <= csr_wmask[`CSR_TLBELO0_V] & csr_wvalue[`CSR_TLBELO0_V]
                      | ~csr_wmask[`CSR_TLBELO0_V] & csr_tlbelo0_v;
        csr_tlbelo0_d <= csr_wmask[`CSR_TLBELO0_D] & csr_wvalue[`CSR_TLBELO0_D]
                      | ~csr_wmask[`CSR_TLBELO0_D] & csr_tlbelo0_d;
        csr_tlbelo0_plv <= csr_wmask[`CSR_TLBELO0_PLV] & csr_wvalue[`CSR_TLBELO0_PLV]
                        | ~csr_wmask[`CSR_TLBELO0_PLV] & csr_tlbelo0_plv;
        csr_tlbelo0_mat <= csr_wmask[`CSR_TLBELO0_MAT] & csr_wvalue[`CSR_TLBELO0_MAT]
                        | ~csr_wmask[`CSR_TLBELO0_MAT] & csr_tlbelo0_mat;
        csr_tlbelo0_g <= csr_wmask[`CSR_TLBELO0_G] & csr_wvalue[`CSR_TLBELO0_G]
                      | ~csr_wmask[`CSR_TLBELO0_G] & csr_tlbelo0_g;
        csr_tlbelo0_ppn <= csr_wmask[`CSR_TLBELO0_PPN] & csr_wvalue[`CSR_TLBELO0_PPN]
                        | ~csr_wmask[`CSR_TLBELO0_PPN] & csr_tlbelo0_ppn;
    end else if (tlb_inst_op[`TLB_INST_TLBRD]) begin
        if (r_e) begin
            csr_tlbelo0_v <= r_v0;
            csr_tlbelo0_d <= r_d0;
            csr_tlbelo0_plv <= r_plv0;
            csr_tlbelo0_mat <= r_mat0;
            csr_tlbelo0_g <= r_g;
            csr_tlbelo0_ppn <= r_ppn0;
        end else begin
            csr_tlbelo0_v <= 0;
            csr_tlbelo0_d <= 0;
            csr_tlbelo0_plv <= 0;
            csr_tlbelo0_mat <= 0;
            csr_tlbelo0_g <= 0;
            csr_tlbelo0_ppn <= 0;
        end
    end

    if (reset) begin
        csr_tlbelo1_v <= 0;
        csr_tlbelo1_d <= 0;
        csr_tlbelo1_plv <= 0;
        csr_tlbelo1_mat <= 0;
        csr_tlbelo1_g <= 0;
        csr_tlbelo1_ppn <= 0;
    end else if (csr_we && csr_wnum==`CSR_TLBELO1) begin        
        csr_tlbelo1_v <= csr_wmask[`CSR_TLBELO1_V] & csr_wvalue[`CSR_TLBELO1_V]
                      | ~csr_wmask[`CSR_TLBELO1_V] & csr_tlbelo1_v;
        csr_tlbelo1_d <= csr_wmask[`CSR_TLBELO1_D] & csr_wvalue[`CSR_TLBELO1_D]
                      | ~csr_wmask[`CSR_TLBELO1_D] & csr_tlbelo1_d;
        csr_tlbelo1_plv <= csr_wmask[`CSR_TLBELO1_PLV] & csr_wvalue[`CSR_TLBELO1_PLV]
                        | ~csr_wmask[`CSR_TLBELO1_PLV] & csr_tlbelo1_plv;
        csr_tlbelo1_mat <= csr_wmask[`CSR_TLBELO1_MAT] & csr_wvalue[`CSR_TLBELO1_MAT]
                        | ~csr_wmask[`CSR_TLBELO1_MAT] & csr_tlbelo1_mat;
        csr_tlbelo1_g <= csr_wmask[`CSR_TLBELO1_G] & csr_wvalue[`CSR_TLBELO1_G]
                      | ~csr_wmask[`CSR_TLBELO1_G] & csr_tlbelo1_g;
        csr_tlbelo1_ppn <= csr_wmask[`CSR_TLBELO1_PPN] & csr_wvalue[`CSR_TLBELO1_PPN]
                        | ~csr_wmask[`CSR_TLBELO1_PPN] & csr_tlbelo1_ppn;
    end else if (tlb_inst_op[`TLB_INST_TLBRD]) begin
        if (r_e) begin
            csr_tlbelo1_v <= r_v1;
            csr_tlbelo1_d <= r_d1;
            csr_tlbelo1_plv <= r_plv1;
            csr_tlbelo1_mat <= r_mat1;
            csr_tlbelo1_g <= r_g;
            csr_tlbelo1_ppn <= r_ppn1;
        end else begin
            csr_tlbelo1_v <= 0;
            csr_tlbelo1_d <= 0;
            csr_tlbelo1_plv <= 0;
            csr_tlbelo1_mat <= 0;
            csr_tlbelo1_g <= 0;
            csr_tlbelo1_ppn <= 0;
        end
    end
end

assign w_asid = csr_asid_asid;
assign csr_asid_asidbits = 10;
always @(posedge clk) begin
    if (reset) begin
        csr_asid_asid <= 0;
    end else if (csr_we && csr_wnum==`CSR_ASID) begin
        csr_asid_asid <= csr_wmask[`CSR_ASID_ASID] & csr_wvalue[`CSR_ASID_ASID]
                      | ~csr_wmask[`CSR_ASID_ASID] & csr_asid_asid;
    end else if (tlb_inst_op[`TLB_INST_TLBRD]) begin
        if (r_e) begin
            csr_asid_asid <= r_asid;
        end else begin
            csr_asid_asid <= 0;
        end
    end
end

always @(posedge clk) begin
    if (reset) begin
        csr_tlbrentry_pa <= 0;
    end else if (csr_we && csr_wnum==`CSR_TLBRENTRY) begin
        csr_tlbrentry_pa <= csr_wmask[`CSR_TLBRENTRY_PA] & csr_wvalue[`CSR_TLBRENTRY_PA]
                         | ~csr_wmask[`CSR_TLBRENTRY_PA] & csr_tlbrentry_pa;
    end
end

always @(posedge clk) begin
    if (reset) begin
        csr_dmw0_plv0 <= 0;
        csr_dmw0_plv3 <= 0;
        csr_dmw0_mat <= 0;
        csr_dmw0_pseg <= 0;
        csr_dmw0_vseg <= 0;
    end else if (csr_we && csr_wnum==`CSR_DMW0) begin
        csr_dmw0_plv0 <= csr_wmask[`CSR_DMW0_PLV0] & csr_wvalue[`CSR_DMW0_PLV0]
                      | ~csr_wmask[`CSR_DMW0_PLV0] & csr_dmw0_plv0;
        csr_dmw0_plv3 <= csr_wmask[`CSR_DMW0_PLV3] & csr_wvalue[`CSR_DMW0_PLV3]
                      | ~csr_wmask[`CSR_DMW0_PLV3] & csr_dmw0_plv3;
        csr_dmw0_mat <= csr_wmask[`CSR_DMW0_MAT] & csr_wvalue[`CSR_DMW0_MAT]
                     | ~csr_wmask[`CSR_DMW0_MAT] & csr_dmw0_mat;
        csr_dmw0_pseg <= csr_wmask[`CSR_DMW0_PSEG] & csr_wvalue[`CSR_DMW0_PSEG]
                      | ~csr_wmask[`CSR_DMW0_PSEG] & csr_dmw0_pseg;
        csr_dmw0_vseg <= csr_wmask[`CSR_DMW0_VSEG] & csr_wvalue[`CSR_DMW0_VSEG]
                      | ~csr_wmask[`CSR_DMW0_VSEG] & csr_dmw0_vseg;
    end
end

always @(posedge clk) begin
    if (reset) begin
        csr_dmw1_plv0 <= 0;
        csr_dmw1_plv3 <= 0;
        csr_dmw1_mat <= 0;
        csr_dmw1_pseg <= 0;
        csr_dmw1_vseg <= 0;
    end else if (csr_we && csr_wnum==`CSR_DMW1) begin
        csr_dmw1_plv0 <= csr_wmask[`CSR_DMW1_PLV0] & csr_wvalue[`CSR_DMW1_PLV0]
                      | ~csr_wmask[`CSR_DMW1_PLV0] & csr_dmw1_plv0;
        csr_dmw1_plv3 <= csr_wmask[`CSR_DMW1_PLV3] & csr_wvalue[`CSR_DMW1_PLV3]
                      | ~csr_wmask[`CSR_DMW1_PLV3] & csr_dmw1_plv3;
        csr_dmw1_mat <= csr_wmask[`CSR_DMW1_MAT] & csr_wvalue[`CSR_DMW1_MAT]
                      | ~csr_wmask[`CSR_DMW1_MAT] & csr_dmw1_mat;
        csr_dmw1_pseg <= csr_wmask[`CSR_DMW1_PSEG] & csr_wvalue[`CSR_DMW1_PSEG]
                      | ~csr_wmask[`CSR_DMW1_PSEG] & csr_dmw1_pseg;
        csr_dmw1_vseg <= csr_wmask[`CSR_DMW1_VSEG] & csr_wvalue[`CSR_DMW1_VSEG]
                      | ~csr_wmask[`CSR_DMW1_VSEG] & csr_dmw1_vseg;
    end
end


// csr read
assign csr_rvalue = {32{csr_rnum==`CSR_CRMD}}       & {23'b0, csr_crmd_datm, csr_crmd_datf, csr_crmd_pg, csr_crmd_da, csr_crmd_ie, csr_crmd_plv}
                  | {32{csr_rnum==`CSR_PRMD}}       & {29'b0, csr_prmd_pie, csr_prmd_pplv}
                  | {32{csr_rnum==`CSR_ESTAT}}      & {1'b0, csr_estat_esubcode, csr_estat_ecode, 3'b0, csr_estat_is}
                  | {32{csr_rnum==`CSR_ERA}}        & csr_era_pc
                  | {32{csr_rnum==`CSR_EENTRY}}     & {csr_eentry_va, 6'b0}
                  | {32{csr_rnum==`CSR_SAVE0}}      & csr_save0_data
                  | {32{csr_rnum==`CSR_SAVE1}}      & csr_save1_data
                  | {32{csr_rnum==`CSR_SAVE2}}      & csr_save2_data
                  | {32{csr_rnum==`CSR_SAVE3}}      & csr_save3_data
                  | {32{csr_rnum==`CSR_ECFG}}       & {19'b0, csr_ecfg_lie}
                  | {32{csr_rnum==`CSR_BADV}}       & csr_badv_vaddr
                  | {32{csr_rnum==`CSR_TID}}        & csr_tid_tid
                  | {32{csr_rnum==`CSR_TCFG}}       & {csr_tcfg_initval, csr_tcfg_periodic, csr_tcfg_en}
                  | {32{csr_rnum==`CSR_TVAL}}       & csr_tval
                  | {32{csr_rnum==`CSR_TICLR}}      & {31'b0, csr_ticlr_clr}
                  | {32{csr_rnum==`CSR_TLBIDX}}     & {csr_tlbidx_ne, 1'b0, csr_tlbidx_ps, 20'b0, csr_tlbidx_index}
                  | {32{csr_rnum==`CSR_TLBEHI}}     & {csr_tlbehi_vppn, 13'b0}
                  | {32{csr_rnum==`CSR_TLBELO0}}    & {4'b0, csr_tlbelo0_ppn, 1'b0, csr_tlbelo0_g, csr_tlbelo0_mat, csr_tlbelo0_plv, csr_tlbelo0_d, csr_tlbelo0_v}
                  | {32{csr_rnum==`CSR_TLBELO1}}    & {4'b0, csr_tlbelo1_ppn, 1'b0, csr_tlbelo1_g, csr_tlbelo1_mat, csr_tlbelo1_plv, csr_tlbelo1_d, csr_tlbelo1_v}
                  | {32{csr_rnum==`CSR_ASID}}       & {8'b0, csr_asid_asidbits, 6'b0, csr_asid_asid}
                  | {32{csr_rnum==`CSR_TLBRENTRY}}  & {csr_tlbrentry_pa, 6'b0}
                  | {32{csr_rnum==`CSR_DMW0}}       & {csr_dmw0_vseg, 1'b0, csr_dmw0_pseg, 19'b0, csr_dmw0_mat, csr_dmw0_plv3, 2'b0, csr_dmw0_plv0}
                  | {32{csr_rnum==`CSR_DMW1}}       & {csr_dmw1_vseg, 1'b0, csr_dmw1_pseg, 19'b0, csr_dmw1_mat, csr_dmw1_plv3, 2'b0, csr_dmw1_plv0};


// exception
assign ex_entry = wb_ecode==`ECODE_TLBR ? {csr_tlbrentry_pa, 6'b0} : {csr_eentry_va, 6'b0};
assign ex_ra    = csr_era_pc;
assign has_int  = ((csr_estat_is[11:0] & csr_ecfg_lie[11:0]) != 12'b0) && (csr_crmd_ie==1'b1);


// stable counter
assign stable_cnt_tid = csr_tid_tid;

always @(posedge clk) begin
    if (reset) begin
        stable_cnt <= 32'h0;
    end else begin
        stable_cnt <= stable_cnt + 1'b1;
    end
end

endmodule