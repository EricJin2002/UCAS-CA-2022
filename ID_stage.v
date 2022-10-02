`include "marco.h"

module ID_stage(
    input  wire        clk,
    input  wire        resetn,
    //bus
    input  wire [`IF_to_ID_LEN  - 1: 0] IF_to_ID_BUS,
    input  wire [`RF_BUS_LEN    - 1: 0] RF_BUS,  
    input  wire [`EXE_RF_LEN    - 1: 0] EXE_RF_BUS,
    input  wire [`MEM_RF_LEN    - 1: 0] MEM_RF_BUS,
    input  wire [`WB_RF_LEN     - 1: 0] WB_RF_BUS,
    output wire [`ID_to_EXE_LEN - 1: 0] ID_to_EXE_BUS,
    output wire [`BR_BUS_LEN    - 1: 0] BR_BUS,
    //
    input  wire        EXE_allowin,
    input  wire        IF_to_ID_valid,
    output wire        ID_to_EXE_valid,
    output wire        ID_allowin
);
//IF_to_ID_BUS = {if_pc,if_inst}
//RF_BUS from WB to ID = {rf_we,dest,res}
//ID_to_EXE_BUS = {id_pc,gr_we,dest,data_addr,mem_en,mem_we,aluop,alusrc1,alusrc2,loadop,rfrom_mem}
//BR_BUS = {BR_target,BR_taken,BR_taken_cancel}
wire         br_taken;
wire         br_taken_cancel;
wire [31: 0] br_target;
wire ID_ready_go;





reg ID_valid;
assign ID_to_EXE_valid = ID_valid && ID_ready_go;
assign ID_allowin = !ID_valid || ID_ready_go && EXE_allowin;

always @(posedge clk)begin
    if(~resetn)begin
        ID_valid <= 1'b0;
    end
    else if(br_taken_cancel)begin
        ID_valid <= 1'b0;
    end
    else if(ID_allowin)begin
        ID_valid <= IF_to_ID_valid;
    end
end

wire [ 4:0] rf_raddr1;
wire [31:0] rf_rdata1;
wire [ 4:0] rf_raddr2;
wire [31:0] rf_rdata2;
wire rf_we;
wire [ 4: 0] rf_waddr;
wire [31: 0] rf_wdata;  
reg [`RF_BUS_LEN    - 1: 0] RF_BUS_temp;

//IF_to_ID_BUS = {if_pc,if_inst}
wire [31: 0] id_pc;
wire [31: 0] inst;
reg [`IF_to_ID_LEN  - 1: 0] IF_to_ID_BUS_temp;
assign {id_pc,inst} = IF_to_ID_BUS_temp;
assign {rf_we,rf_waddr,rf_wdata} = RF_BUS;
always @(posedge clk)begin
    if(~resetn)begin
        IF_to_ID_BUS_temp <= {`IF_to_ID_LEN{1'b0}};
    end
    else if(IF_to_ID_valid && ID_allowin)begin
        IF_to_ID_BUS_temp <= IF_to_ID_BUS;
    end
    // else if(~IF_to_ID_valid)begin
    //     IF_to_ID_BUS_temp <= {`IF_to_ID_LEN{1'b0}};    
    // end
end

wire [11:0] alu_op;
wire [31:0] alu_src1;
wire [31:0] alu_src2;
wire [ 3:0] load_op;
wire        rfrom_mem;
wire        store_op;
wire        src1_is_pc;//check alu src1 PC bl/jrl
wire        src2_is_imm;
wire        dst_is_r1;
wire        gr_we;
wire        mem_en;
wire [3: 0] mem_we;
wire        src_reg_is_rd;
wire        src_reg_is_rk;
wire        src_reg_is_rkd;
wire        src_reg_is_rj;
wire [4: 0] dest;
wire [31:0] rj_value;
wire [31:0] rkd_value;
wire [31:0] imm;
wire [31:0] br_offs;
wire [31:0] jirl_offs;

wire [ 5:0] op_31_26;
wire [ 3:0] op_25_22;
wire [ 1:0] op_21_20;
wire [ 4:0] op_19_15;
wire [ 4:0] rd;
wire [ 4:0] rj;
wire [ 4:0] rk;
wire [ 4:0] rkd;
wire [11:0] i12;
wire [19:0] i20;
wire [15:0] i16;
wire [25:0] i26;

wire [63:0] op_31_26_d;
wire [15:0] op_25_22_d;
wire [ 3:0] op_21_20_d;
wire [31:0] op_19_15_d;

wire        inst_add_w;
wire        inst_sub_w;
wire        inst_slt;
wire        inst_sltu;
wire        inst_nor;
wire        inst_and;
wire        inst_or;
wire        inst_xor;
wire        inst_slli_w;
wire        inst_srli_w;
wire        inst_srai_w;
wire        inst_addi_w;
wire        inst_ld_w;
wire        inst_st_w;
wire        inst_jirl;
wire        inst_b;
wire        inst_bl;
wire        inst_beq;
wire        inst_bne;
wire        inst_lu12i_w;

wire        need_ui5;
wire        need_ui12;

wire        need_si12;
wire        need_si16;
wire        need_si20;
wire        need_si26;
wire        src2_is_4;

assign op_31_26  = inst[31:26];
assign op_25_22  = inst[25:22];
assign op_21_20  = inst[21:20];
assign op_19_15  = inst[19:15];

assign rd   = inst[ 4: 0];
assign rj   = inst[ 9: 5];
assign rk   = inst[14:10];


assign i12  = inst[21:10];
assign i20  = inst[24: 5];
assign i16  = inst[25:10];
assign i26  = {inst[ 9: 0], inst[25:10]};

decoder_6_64 u_dec0(.in(op_31_26 ), .out(op_31_26_d ));
decoder_4_16 u_dec1(.in(op_25_22 ), .out(op_25_22_d ));
decoder_2_4  u_dec2(.in(op_21_20 ), .out(op_21_20_d ));
decoder_5_32 u_dec3(.in(op_19_15 ), .out(op_19_15_d ));

assign inst_add_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h00];
assign inst_sub_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h02];
assign inst_slt    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h04];
assign inst_sltu   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h05];
assign inst_nor    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h08];
assign inst_and    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h09];
assign inst_or     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0a];
assign inst_xor    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0b];
assign inst_slli_w = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h01];
assign inst_srli_w = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h09];
assign inst_srai_w = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h11];
assign inst_addi_w = op_31_26_d[6'h00] & op_25_22_d[4'ha];
assign inst_ld_w   = op_31_26_d[6'h0a] & op_25_22_d[4'h2];
assign inst_st_w   = op_31_26_d[6'h0a] & op_25_22_d[4'h6];
assign inst_jirl   = op_31_26_d[6'h13];
assign inst_b      = op_31_26_d[6'h14];
assign inst_bl     = op_31_26_d[6'h15];
assign inst_beq    = op_31_26_d[6'h16];
assign inst_bne    = op_31_26_d[6'h17];
assign inst_lu12i_w= op_31_26_d[6'h05] & ~inst[25];

assign alu_op[ 0] = inst_add_w | inst_addi_w | inst_ld_w | inst_st_w
                    | inst_jirl | inst_bl;
assign alu_op[ 1] = inst_sub_w;
assign alu_op[ 2] = inst_slt;
assign alu_op[ 3] = inst_sltu;
assign alu_op[ 4] = inst_and;
assign alu_op[ 5] = inst_nor;
assign alu_op[ 6] = inst_or;
assign alu_op[ 7] = inst_xor;
assign alu_op[ 8] = inst_slli_w;
assign alu_op[ 9] = inst_srli_w;
assign alu_op[10] = inst_srai_w;
assign alu_op[11] = inst_lu12i_w;

assign need_ui5   =  inst_slli_w | inst_srli_w | inst_srai_w;

assign need_si12  =  inst_addi_w | inst_ld_w | inst_st_w;
assign need_si16  =  inst_jirl | inst_beq | inst_bne;
assign need_si20  =  inst_lu12i_w;
assign need_si26  =  inst_b | inst_bl;
assign src2_is_4  =  inst_jirl | inst_bl;

assign imm = {32{src2_is_4}} & 32'h4                     |
             {32{need_si20}} & {i20[19:0], 12'b0}        |
             {32{need_ui5 }} & {{27{1'b0}}, i12[4:0]}    |
             {32{need_si12}} & {{20{i12[11]}}, i12[11:0]}
             ;

assign src_reg_is_rd  = inst_beq   | inst_bne   | inst_st_w;
assign src_reg_is_rk  = inst_add_w | inst_sub_w | inst_slt | inst_sltu | inst_and | inst_or | inst_nor | inst_xor;   
assign src_reg_is_rj  = ~inst_b & ~inst_bl & ~inst_lu12i_w;
assign src_reg_is_rkd = src_reg_is_rd | src_reg_is_rk;
assign rkd = {5{src_reg_is_rk}} & rk |
             {5{src_reg_is_rd}} & rd ;

assign src1_is_pc    = inst_jirl | inst_bl;

assign src2_is_imm   = inst_slli_w |
                       inst_srli_w |
                       inst_srai_w |
                       inst_addi_w |
                       inst_ld_w   |
                       inst_st_w   |
                       inst_lu12i_w|
                       inst_jirl   |
                       inst_bl     ;

assign load_op       = {4{inst_ld_w}};
assign rfrom_mem     = inst_ld_w;
assign store_op      = inst_st_w;
assign dst_is_r1     = inst_bl;
assign gr_we         = ~inst_st_w & ~inst_beq & ~inst_bne & ~inst_b;
assign mem_en        = inst_st_w;
assign mem_we        = {4{inst_st_w}};
assign dest          = dst_is_r1 ? 5'd1 : rd;


assign rf_raddr1 = rj;
assign rf_raddr2 = src_reg_is_rd ? rd :rk;



assign jirl_offs = {{14{i16[15]}}, i16[15:0], 2'b0};
assign br_offs = need_si26 ? {{ 4{i26[25]}}, i26[25:0], 2'b0} :
                             {{14{i16[15]}}, i16[15:0], 2'b0} ;



assign alu_src1 = src1_is_pc  ? id_pc[31:0] : rj_value;
assign alu_src2 = src2_is_imm ? imm : rkd_value;

assign rj_eq_rd = (rj_value == rkd_value);
assign br_taken = (   inst_beq  &&  rj_eq_rd
                   || inst_bne  && !rj_eq_rd
                   || inst_jirl
                   || inst_bl
                   || inst_b
                  ) && ID_valid;
assign br_target = (inst_beq || inst_bne || inst_bl || inst_b) ? (id_pc + br_offs) :
                                                   /*inst_jirl*/ (rj_value + jirl_offs);
//BR_BUS = {BR_target,BR_taken,BR_taken_cancel}
assign BR_BUS = {br_target,br_taken,br_taken_cancel};

//ID_to_EXE_BUS = {id_pc,gr_we,dest,data_sum,mem_en,mem_we,aluop,alusrc1,alusrc2,loadop,rfrom_mem}
assign ID_to_EXE_BUS = {id_pc,gr_we,dest,rkd_value,mem_en,mem_we,alu_op,alu_src1,alu_src2,load_op,rfrom_mem};


// ready go
wire [ 4: 0] EXE_dest;
wire EXE_rfrom_mem;
wire [31: 0] EXE_alu_result;

wire [ 4: 0] MEM_dest;
wire MEM_rfrom_mem;
wire [31: 0] MEM_result;

wire [ 4: 0] WB_dest ;
wire WB_rfrom_mem;
wire [31: 0] WB_result;

assign {EXE_dest,EXE_rfrom_mem,EXE_alu_result} = EXE_RF_BUS;
assign {MEM_dest,MEM_rfrom_mem,MEM_result    } = MEM_RF_BUS;
assign {WB_dest ,WB_rfrom_mem ,WB_result     } = WB_RF_BUS ;

// wire        src_reg_is_rd;
// wire        src_reg_is_rk;
// wire        src_reg_is_rj;

wire checkrj;
wire checkrkd;

assign checkrj  = (src_reg_is_rj  && (rj  != 5'b00000)) ? ~(rj ==EXE_dest && EXE_rfrom_mem) : 1;
assign checkrkd = (src_reg_is_rkd && (rkd != 5'b00000)) ? ~(rkd==EXE_dest && EXE_rfrom_mem) : 1;
assign ID_ready_go = checkrj && checkrkd;

assign br_taken_cancel = br_taken & checkrj & checkrkd;

regfile u_regfile(
    .clk    (clk      ),
    .raddr1 (rf_raddr1),
    .rdata1 (rf_rdata1),
    .raddr2 (rf_raddr2),
    .rdata2 (rf_rdata2),
    .we     (rf_we    ),
    .waddr  (rf_waddr ),
    .wdata  (rf_wdata )
    );
assign rj_value  = (rj  == 5'b00000) ? 32'b0 : (rj  == EXE_dest) ? EXE_alu_result : (rj  == MEM_dest) ? MEM_result : (rj  == WB_dest) ? WB_result : rf_rdata1;
assign rkd_value = (rkd == 5'b00000) ? 32'b0 : (rkd == EXE_dest) ? EXE_alu_result : (rkd == MEM_dest) ? MEM_result : (rkd == WB_dest) ? WB_result : rf_rdata2;
// assign rj_value  = (rf_we && rf_waddr!=0 && (rf_raddr1 == rf_waddr)) ? rf_wdata : rf_rdata1; 
// assign rkd_value = (rf_we && rf_waddr!=0 && (rf_raddr2 == rf_waddr)) ? rf_wdata : rf_rdata2;
endmodule