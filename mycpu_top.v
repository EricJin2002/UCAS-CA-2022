`include "macro.vh"

module mycpu_top(
    input  wire        clk,
    input  wire        resetn,
    // inst sram interface
    output wire        inst_sram_en,//片选信号
    output wire [ 3:0] inst_sram_we,//字节写使能
    output wire [31:0] inst_sram_addr,
    output wire [31:0] inst_sram_wdata,
    input  wire [31:0] inst_sram_rdata,
    // data sram interface
    output wire        data_sram_en,     
    output wire [ 3:0] data_sram_we,
    output wire [31:0] data_sram_addr,
    output wire [31:0] data_sram_wdata,
    input  wire [31:0] data_sram_rdata,
    // trace debug interface
    output wire [31:0] debug_wb_pc,
    output wire [ 3:0] debug_wb_rf_we,
    output wire [ 4:0] debug_wb_rf_wnum,
    output wire [31:0] debug_wb_rf_wdata
);
/*
pipe1 IF
pipe2 ID
pipe3 EXE
pipe4 MEM
pipe5 WB
*/

//allowin 第X级传递给第X-1级的信号，值为1表示下一时钟X级流水线可以更新X-1级流水线的数据
wire IF_allowin;
wire ID_allowin;
wire EXE_allowin;
wire MEM_allowin;
wire WB_allowin;

//ready_go 第X级处理任务的完成状态，1表示任务完成，可以传递到X+1级
// wire IF_ready_go;
// wire ID_ready_go;
// wire EXE_ready_go;
// wire MEM_ready_go;
// wire WB_ready_go;

//valid 第X级传递给第X+1级的信号，值为1表示第X级数据下一拍想进入X+1级
wire IF_to_ID_valid;
wire ID_to_EXE_valid;
wire EXE_to_MEM_valid;
wire MEM_to_WB_valid;
// wire        IF_valid;

//BUS
//BR_BUS = {BR_target,BR_taken,BR_taken_cancel}
wire [`BR_BUS_LEN     - 1: 0] BR_BUS;

//IF_to_ID_BUS = {if_pc,if_inst}
wire [`IF_to_ID_LEN   - 1: 0] IF_to_ID_BUS;

//ID_to_EXE_BUS = {id_pc,gr_we,dest,data_addr,mem_en,mem_we,aluop,alusrc1,alusrc2,loadop,rfrom_mem}
wire [`ID_to_EXE_LEN  - 1: 0] ID_to_EXE_BUS;

//RF_BUS from WB to ID = {rf_we,dest,res}
wire [`RF_BUS_LEN     - 1: 0] RF_BUS;

//EXE_to_MEM_BUS = {exe_pc,gr_we,dest,alu_res,data_addr,mem_en,mem_we,loadop,rfrom_mem}
wire [`EXE_to_MEM_LEN - 1: 0] EXE_to_MEM_BUS;

//MEM_to_WB_BUS = {mem_pc,gr_we,dest,memresult,loadop,rfrom_mem}
wire [`MEM_to_WB_LEN  - 1: 0] MEM_to_WB_BUS;

//BUS for WAR BUG
//{dest,op,data}
wire [`EXE_RF_LEN     - 1: 0] EXE_RF_BUS;
//{dest,op,data}
wire [`MEM_RF_LEN     - 1: 0] MEM_RF_BUS;
//{dest,op,data}
wire [`WB_RF_LEN      - 1: 0] WB_RF_BUS;

//for CSR
wire [13: 0] csr_wnum;
wire         csr_we;
wire [31: 0] csr_wvalue;
wire [31: 0] csr_wmask;
wire [13: 0] csr_rnum;
wire [31: 0] csr_rvalue;

wire [31: 0] wb_pc;
wire [ 7: 0] hw_int_in;
wire [31: 0] ex_entry;
wire         has_int;
wire         ertn_flush;
wire         wb_ex;
wire [ 5: 0] wb_ecode;
wire [ 8: 0] wb_esubcod;

IF_stage myIF(
    .clk(clk),
    .resetn(resetn),
    .inst_sram_en(inst_sram_en),
    .inst_sram_we(inst_sram_we),
    .inst_sram_addr(inst_sram_addr),
    .inst_sram_wdata(inst_sram_wdata),
    .inst_sram_rdata(inst_sram_rdata),
    .BR_BUS(BR_BUS),
    .IF_to_ID_BUS(IF_to_ID_BUS),
    .ID_allowin(ID_allowin),
    .IF_to_ID_valid(IF_to_ID_valid),
    .IF_allowin(IF_allowin)
);

ID_stage myID(
    .clk(clk),
    .resetn(resetn),
    .IF_to_ID_BUS(IF_to_ID_BUS),
    .RF_BUS(RF_BUS),
    .EXE_RF_BUS(EXE_RF_BUS),
    .MEM_RF_BUS(MEM_RF_BUS),
    .WB_RF_BUS(WB_RF_BUS),
    .ID_to_EXE_BUS(ID_to_EXE_BUS),
    .BR_BUS(BR_BUS),
    .EXE_allowin(EXE_allowin),
    .IF_to_ID_valid(IF_to_ID_valid),
    .ID_to_EXE_valid(ID_to_EXE_valid),
    .ID_allowin(ID_allowin)
);

EXE_stage myEXE(
    .clk(clk),
    .resetn(resetn),
    .ID_to_EXE_BUS(ID_to_EXE_BUS),
    .EXE_to_MEM_BUS(EXE_to_MEM_BUS),
    .EXE_RF_BUS(EXE_RF_BUS),
    .csr_num(csr_rnum),
    .csr_rvalue(csr_rvalue),
    .data_sram_en(data_sram_en),
    .data_sram_we(data_sram_we),
    .data_sram_addr(data_sram_addr),
    .data_sram_wdata(data_sram_wdata),
    .ID_to_EXE_valid(ID_to_EXE_valid),
    .MEM_allowin(MEM_allowin),
    .EXE_allowin(EXE_allowin),
    .EXE_to_MEM_valid(EXE_to_MEM_valid)
);

MEM_stage myMEM(
    .clk(clk),
    .resetn(resetn),
    .data_sram_rdata(data_sram_rdata),
    .EXE_to_MEM_BUS(EXE_to_MEM_BUS),
    .MEM_to_WB_BUS(MEM_to_WB_BUS),
    .MEM_RF_BUS(MEM_RF_BUS),
    .EXE_to_MEM_valid(EXE_to_MEM_valid),
    .WB_allowin(WB_allowin),
    .MEM_allowin(MEM_allowin),
    .MEM_to_WB_valid(MEM_to_WB_valid)
);

WB_stage myWB(
    .clk(clk),
    .resetn(resetn),
    .csr_num(csr_wnum),
    .csr_we(csr_we),
    .csr_wvalue(csr_wvalue),
    .csr_wmask(csr_wmask),
    .debug_wb_pc(debug_wb_pc),
    .debug_wb_rf_we(debug_wb_rf_we),
    .debug_wb_rf_wnum(debug_wb_rf_wnum),
    .debug_wb_rf_wdata(debug_wb_rf_wdata),
    .MEM_to_WB_BUS(MEM_to_WB_BUS),
    .RF_BUS(RF_BUS),
    .WB_RF_BUS(WB_RF_BUS),
    .MEM_to_WB_valid(MEM_to_WB_valid),
    .WB_allowin(WB_allowin)
);

control_status_register myCSR(
    .clk(clk),
    .resetn(resetn),
    .csr_wnum(csr_wnum),
    .csr_we(csr_we),
    .csr_wvalue(csr_wvalue),
    .csr_wmask(csr_wmask),
    .csr_rnum(csr_rnum),
    .csr_rvalue(csr_rvalue),
    .wb_pc(wb_pc),
    .hw_int_in(hw_int_in),
    .ex_entry(ex_entry),
    .has_int(has_int),
    .ertn_flush(ertn_flush),
    .wb_ex(wb_ex),
    .wb_ecode(wb_ecode),
    .wb_esubcode(wb_esubcode)
);
endmodule
