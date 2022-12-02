`include "macro.vh"

module mycpu_top(
    input  wire        aclk,
    input  wire        aresetn,

    output wire [ 3:0] arid,
    output wire [31:0] araddr,
    output wire [ 7:0] arlen,
    output wire [ 2:0] arsize,
    output wire [ 1:0] arburst,
    output wire [ 1:0] arlock,
    output wire [ 3:0] arcache,
    output wire [ 2:0] arprot,
    output wire        arvalid,
    input  wire        arready,

    input  wire [ 3:0] rid,
    input  wire [31:0] rdata,
    input  wire [ 1:0] rresp,
    input  wire        rlast,
    input  wire        rvalid,
    output wire        rready,

    output wire [ 3:0] awid,
    output wire [31:0] awaddr,
    output wire [ 7:0] awlen,
    output wire [ 2:0] awsize,
    output wire [ 1:0] awburst,
    output wire [ 1:0] awlock,
    output wire [ 3:0] awcache,
    output wire [ 2:0] awprot,
    output wire        awvalid,
    input  wire        awready,

    output wire [ 3:0] wid,
    output wire [31:0] wdata,
    output wire [ 3:0] wstrb,
    output wire        wlast,
    output wire        wvalid,
    input  wire        wready,

    input  wire [ 3:0] bid,
    input  wire [ 1:0] bresp,
    input  wire        bvalid,
    output wire        bready,

    //debug interface
    output wire [31:0] debug_wb_pc,
    output wire [ 3:0] debug_wb_rf_we,
    output wire [ 4:0] debug_wb_rf_wnum,
    output wire [31:0] debug_wb_rf_wdata


//    input  wire        clk,
//    input  wire        resetn,
//    
//    // inst sram interface
//    output wire        inst_sram_req,
//    output wire        inst_sram_wr,
//    output wire [ 1:0] inst_sram_size,
//    output wire [ 3:0] inst_sram_wstrb,
//    output wire [31:0] inst_sram_addr,
//    output wire [31:0] inst_sram_wdata,
//    input  wire        inst_sram_addr_ok,
//    input  wire        inst_sram_data_ok,
//    input  wire [31:0] inst_sram_rdata,
//    // output wire        inst_sram_en,//片选信号
//    // output wire [ 3:0] inst_sram_we,//字节写使能
//
//    // data sram interface
//    output wire        data_sram_req,
//    output wire        data_sram_wr,
//    output wire [ 1:0] data_sram_size,
//    output wire [ 3:0] data_sram_wstrb,
//    output wire [31:0] data_sram_addr,
//    output wire [31:0] data_sram_wdata,
//    input  wire        data_sram_addr_ok,
//    input  wire        data_sram_data_ok,
//    input  wire [31:0] data_sram_rdata,
//    // output wire        data_sram_en,     
//    // output wire [ 3:0] data_sram_we,
//    
//    // trace debug interface
//    output wire [31:0] debug_wb_pc,
//    output wire [ 3:0] debug_wb_rf_we,
//    output wire [ 4:0] debug_wb_rf_wnum,
//    output wire [31:0] debug_wb_rf_wdata
);

wire        clk;
wire        resetn;

wire        inst_sram_req;
wire        inst_sram_wr;
wire [ 1:0] inst_sram_size;
wire [ 3:0] inst_sram_wstrb;
wire [31:0] inst_sram_addr;
wire [31:0] inst_sram_wdata;
wire        inst_sram_addr_ok;
wire        inst_sram_data_ok;
wire [31:0] inst_sram_rdata;
wire        inst_sram_en;//片选信号
wire [ 3:0] inst_sram_we;//字节写使能

wire        data_sram_req;
wire        data_sram_wr;
wire [ 1:0] data_sram_size;
wire [ 3:0] data_sram_wstrb;
wire [31:0] data_sram_addr;
wire [31:0] data_sram_wdata;
wire        data_sram_addr_ok;
wire        data_sram_data_ok;
wire [31:0] data_sram_rdata;

assign clk = aclk;
assign resetn = aresetn;

wire data_sram_rd_req;
wire data_sram_wr_req;
assign data_sram_rd_req = data_sram_req && !data_sram_wr;
assign data_sram_wr_req = data_sram_req && data_sram_wr;


// AR
reg  [1:0] ar_next_state;
reg  [1:0] ar_current_state;
always @(posedge clk) begin
    if (~resetn) begin
        ar_current_state <= `AR_INIT;
    end else begin
        ar_current_state <= ar_next_state;
    end
end
always @(*) begin
    case (ar_current_state)
        `AR_INIT: begin
            if (!inst_sram_req && !data_sram_rd_req) begin
                ar_next_state <= `AR_INIT;
            end else begin
                if(arready) begin
                    ar_next_state <= `AR_INIT;
                end else begin
                    ar_next_state <= `AR_WAIT;
                end
            end
        end
        `AR_WAIT: begin
            if (!arready) begin
                ar_next_state <= `AR_WAIT;
            end else begin
                if(inst_sram_req || data_sram_rd_req) begin
                    ar_next_state <= `AR_WAIT;
                end else begin
                    ar_next_state <= `AR_INIT;
                end
            end
        end
        default: ar_next_state <= `AR_INIT;
    endcase
end

wire ar_waiting_ready;
assign ar_waiting_ready = ar_current_state == `AR_WAIT && !arready;

reg  [ 3:0] arid_r;
reg  [31:0] araddr_r;
reg  [ 2:0] arsize_r;
always @(posedge clk) begin
    if (~resetn) begin
        arid_r <= 4'b0;
        araddr_r <= 32'b0;
        arsize_r <= 3'b0;
    end else begin
        if (!ar_waiting_ready) begin
            arid_r <= data_sram_rd_req;
            araddr_r <= data_sram_rd_req ? data_sram_addr : inst_sram_addr;
            arsize_r <= data_sram_rd_req ? data_sram_size : inst_sram_size;
        end
    end
end

assign arid     = ar_current_state == `AR_INIT ? data_sram_rd_req                                   : arid_r;
assign araddr   = ar_current_state == `AR_INIT ? data_sram_rd_req ? data_sram_addr : inst_sram_addr : araddr_r;
assign arlen    = 8'b0;
assign arsize   = ar_current_state == `AR_INIT ? data_sram_rd_req ? data_sram_size : inst_sram_size : arsize_r;
assign arburst  = 2'b01;
assign arlock   = 2'b00;
assign arcache  = 4'b0000;
assign arprot   = 3'b000;
assign arvalid  = ar_current_state == `AR_WAIT || ar_current_state == `AR_INIT && (inst_sram_req || data_sram_rd_req);


// R/B
assign rready = 1'b1;
assign bready = 1'b1;


// AW/W
reg  [1:0] aw_w_next_state;
reg  [1:0] aw_w_current_state;
always @(posedge clk) begin
    if (~resetn) begin
        aw_w_current_state <= `AW_W_INIT;
    end else begin
        aw_w_current_state <= aw_w_next_state;
    end
end
always @(*) begin
    case (aw_w_current_state)
        `AW_W_INIT: begin
            if (!data_sram_wr_req || data_sram_wr_req && wready && awready) begin
                aw_w_next_state <= `AW_W_INIT;
            end else if (data_sram_wr_req && !wready && !awready) begin
                aw_w_next_state <= `AW_W_WAIT;
            end else if (data_sram_wr_req && wready && !awready) begin
                aw_w_next_state <= `AW_W_WAIT_AW;
            end else if (data_sram_wr_req && !wready && awready) begin
                aw_w_next_state <= `AW_W_WAIT_W;
            end
        end
        `AW_W_WAIT: begin
            if (!wready && !awready || data_sram_wr_req && wready && awready) begin
                aw_w_next_state <= `AW_W_WAIT;
            end else if (wready && !awready) begin
                aw_w_next_state <= `AW_W_WAIT_AW;
            end else if (!wready && awready) begin
                aw_w_next_state <= `AW_W_WAIT_W;
            end else if (!data_sram_wr_req && wready && awready) begin
                aw_w_next_state <= `AW_W_INIT;
            end
        end
        `AW_W_WAIT_AW: begin
            if (!awready) begin
                aw_w_next_state <= `AW_W_WAIT_AW;
            end else if (data_sram_wr_req && awready) begin
                aw_w_next_state <= `AW_W_WAIT;
            end else if (!data_sram_wr_req && awready) begin
                aw_w_next_state <= `AW_W_INIT;
            end
        end
        `AW_W_WAIT_W: begin
            if (!wready) begin
                aw_w_next_state <= `AW_W_WAIT_W;
            end else if (data_sram_wr_req && wready) begin
                aw_w_next_state <= `AW_W_WAIT;
            end else if (!data_sram_wr_req && wready) begin
                aw_w_next_state <= `AW_W_INIT;
            end
        end
        default: aw_w_next_state <= `AW_W_INIT;
    endcase
end

wire aw_w_waiting_ready;
assign aw_w_waiting_ready = aw_w_current_state == `AW_W_WAIT    && !(awready && wready) 
                         || aw_w_current_state == `AW_W_WAIT_AW && !awready
                         || aw_w_current_state == `AW_W_WAIT_W  && !wready;

reg  [31:0] awaddr_r;
reg  [ 2:0] awsize_r;
reg  [31:0] wdata_r;    
reg  [ 3:0] wstrb_r;
always @(posedge clk) begin
    if (~resetn) begin
        awaddr_r <= 32'b0;
        awsize_r <= 3'b0;
        wdata_r  <= 32'b0;
        wstrb_r  <= 4'b0;
    end else begin
        if (!aw_w_waiting_ready) begin
            awaddr_r <= data_sram_addr;
            awsize_r <= data_sram_size;
            wdata_r  <= data_sram_wdata;
            wstrb_r  <= data_sram_wstrb;
        end
    end
end

assign awid     = 4'b1;
assign awaddr   = aw_w_current_state == `AW_W_INIT ? data_sram_addr : awaddr_r;
assign awlen    = 8'b0;
assign awsize   = aw_w_current_state == `AW_W_INIT ? data_sram_size : awsize_r;
assign awburst  = 2'b01;
assign awlock   = 2'b00;
assign awcache  = 4'b0000;
assign awprot   = 3'b000;
assign awvalid  = aw_w_current_state == `AW_W_WAIT || aw_w_current_state == `AW_W_WAIT_AW
               || aw_w_current_state == `AW_W_INIT && data_sram_wr_req;

assign wid      = 4'b1;
assign wdata    = aw_w_current_state == `AW_W_INIT ? data_sram_wdata : wdata_r;
assign wstrb    = aw_w_current_state == `AW_W_INIT ? data_sram_wstrb : wstrb_r;
assign wlast    = 1'b1;
assign wvalid   = aw_w_current_state == `AW_W_WAIT || aw_w_current_state == `AW_W_WAIT_W
               || aw_w_current_state == `AW_W_INIT && data_sram_wr_req;


// to CPU
assign inst_sram_addr_ok = !data_sram_rd_req && !ar_waiting_ready;
assign data_sram_addr_ok = data_sram_rd_req && !ar_waiting_ready || data_sram_wr_req && !aw_w_waiting_ready;
assign inst_sram_data_ok = rvalid && !rid;
assign data_sram_data_ok = rvalid && rid || bvalid;
assign inst_sram_rdata = rdata;
assign data_sram_rdata = rdata;

/*
pipe0 preIF
pipe1 IF
pipe2 ID
pipe3 EXE
pipe4 MEM
pipe5 WB
*/

//allowin 第X级传递给第X-1级的信号，值为1表示下一时钟X级流水线可以更新X-1级流水线的数据
wire preIF_allowin;
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
wire preIF_to_IF_valid;
wire IF_to_ID_valid;
wire ID_to_EXE_valid;
wire EXE_to_MEM_valid;
wire MEM_to_WB_valid;
// wire        IF_valid;

wire preIF_validin;
assign preIF_validin = resetn;

//BUS
//BR_BUS = {BR_target,BR_taken,BR_taken_cancel}
wire [`BR_BUS_LEN       - 1: 0] BR_BUS;

wire [`preIF_to_IF_LEN  - 1: 0] preIF_to_IF_BUS;

//IF_to_ID_BUS = {if_pc,if_inst}
wire [`IF_to_ID_LEN     - 1: 0] IF_to_ID_BUS;

//ID_to_EXE_BUS = {id_pc,gr_we,dest,data_addr,mem_en,mem_we,aluop,alusrc1,alusrc2,loadop,rfrom_mem}
wire [`ID_to_EXE_LEN    - 1: 0] ID_to_EXE_BUS;

//RF_BUS from WB to ID = {rf_we,dest,res}
wire [`RF_BUS_LEN       - 1: 0] RF_BUS;

//EXE_to_MEM_BUS = {exe_pc,gr_we,dest,alu_res,data_addr,mem_en,mem_we,loadop,rfrom_mem}
wire [`EXE_to_MEM_LEN   - 1: 0] EXE_to_MEM_BUS;

//MEM_to_WB_BUS = {mem_pc,gr_we,dest,memresult,loadop,rfrom_mem}
wire [`MEM_to_WB_LEN    - 1: 0] MEM_to_WB_BUS;

//BUS for WAR BUG
//{dest,op,data}
wire [`EXE_RF_LEN       - 1: 0] EXE_RF_BUS;
//{dest,op,data}
wire [`MEM_RF_LEN       - 1: 0] MEM_RF_BUS;
//{dest,op,data}
wire [`WB_RF_LEN        - 1: 0] WB_RF_BUS;

wire [ 6: 0] ID_to_EXE_mul_div_op;

//for CSR
wire [13: 0] csr_wnum;
wire         csr_we;
wire [31: 0] csr_wvalue;
wire [31: 0] csr_wmask;
wire [13: 0] csr_rnum;
wire [31: 0] csr_rvalue;

// for tlb
wire [ 4: 0] tlb_inst_op_from_EXE;
wire [ 4: 0] tlb_inst_op_from_WB;
wire [ 9: 0] invtlb_asid;
wire [18: 0] invtlb_vppn;
wire [ 4: 0] invtlb_op;

wire [31: 0] wb_pc;
wire [31: 0] wb_vaddr;
wire [ 7: 0] hw_int_in;
wire         ipi_int_in;
wire [31: 0] ex_entry;
wire [31: 0] ex_ra;
wire         has_int;
wire         ertn_flush;
wire         wb_ex;
wire [ 5: 0] wb_ecode;
wire [ 8: 0] wb_esubcode;
wire         wb_refetch;
wire         ID_to_IF_refetch;
wire         MEM_to_EXE_block_tlbsrch;
wire         WB_to_EXE_block_tlbsrch;

wire [31: 0] if_pc;

wire         mem_ertn;
wire         mem_ex;
wire         mem_refetch;

wire [31: 0] next_pc_vaddr;
wire [31: 0] next_pc_paddr;
wire         to_PreIF_ex_ade;
wire         to_PreIF_ex_tlbr;
wire         to_PreIF_ex_pif;
wire         to_PreIF_ex_ppi;

wire [31: 0] mem_vaddr;
wire [31: 0] mem_paddr;
wire         mem_wr;
wire         to_EXE_ex_ade;
wire         to_EXE_ex_tlbr;
wire         to_EXE_ex_pil;
wire         to_EXE_ex_pis;
wire         to_EXE_ex_ppi;
wire         to_EXE_ex_pme;

wire [63: 0] stable_cnt;
wire [31: 0] stable_cnt_tid;

reg  [ 3: 0] inst_IO_cnt;
always @(posedge clk)begin
    if(~resetn)begin
        inst_IO_cnt <= 4'b0;
    end else begin
        if(inst_sram_addr_ok && inst_sram_req && ~inst_sram_data_ok) begin
            inst_IO_cnt <= inst_IO_cnt + 1;
        end else if( !(inst_sram_addr_ok && inst_sram_req) && inst_sram_data_ok) begin
            inst_IO_cnt <= inst_IO_cnt - 1;
        end else begin
            inst_IO_cnt <= inst_IO_cnt;
        end
    end
end

reg  [ 3: 0] data_IO_cnt;
always @(posedge clk)begin
    if(~resetn)begin
        data_IO_cnt <= 4'b0;
    end else begin
        if(data_sram_addr_ok && data_sram_req && ~data_sram_data_ok) begin
            data_IO_cnt <= data_IO_cnt + 1;
        end else if( !(data_sram_addr_ok && data_sram_req) && data_sram_data_ok) begin
            data_IO_cnt <= data_IO_cnt - 1;
        end else begin
            data_IO_cnt <= data_IO_cnt;
        end
    end
end

preIF_stage mypreIF(
    .clk(clk),
    .resetn(resetn),
    .inst_sram_req(inst_sram_req),
    .inst_sram_wr(inst_sram_wr),
    .inst_sram_size(inst_sram_size),
    .inst_sram_wstrb(inst_sram_wstrb),
    .inst_sram_addr(inst_sram_addr),
    .inst_sram_wdata(inst_sram_wdata),
    .inst_sram_addr_ok(inst_sram_addr_ok),
    .BR_BUS(BR_BUS),
    .preIF_to_IF_BUS(preIF_to_IF_BUS),
    .IF_allowin(IF_allowin),
    .preIF_validin(preIF_validin),
    .preIF_to_IF_valid(preIF_to_IF_valid),
    .preIF_allowin(preIF_allowin),
    .if_pc(if_pc),
    .ertn_flush(ertn_flush),
    .wb_ex(wb_ex),
    .wb_refetch(wb_refetch),
    .ex_ra(ex_ra),
    .ex_entry(ex_entry),
    .wb_pc(wb_pc),
    .next_pc_vaddr(next_pc_vaddr),
    .next_pc_paddr(next_pc_paddr),
    .to_PreIF_ex_ade(to_PreIF_ex_ade),
    .to_PreIF_ex_tlbr(to_PreIF_ex_tlbr),
    .to_PreIF_ex_pif(to_PreIF_ex_pif),
    .to_PreIF_ex_ppi(to_PreIF_ex_ppi)
);

IF_stage myIF(
    .clk(clk),
    .resetn(resetn),
    .inst_sram_data_ok(inst_sram_data_ok),
    .inst_sram_rdata(inst_sram_rdata),
    .BR_BUS(BR_BUS),
    .preIF_to_IF_BUS(preIF_to_IF_BUS),
    .IF_to_ID_BUS(IF_to_ID_BUS),
    .ID_allowin(ID_allowin),
    .preIF_to_IF_valid(preIF_to_IF_valid),
    .IO_cnt(inst_IO_cnt),
    .IF_to_ID_valid(IF_to_ID_valid),
    .IF_allowin(IF_allowin),
    .if_pc(if_pc),
    .ertn_flush(ertn_flush),
    .wb_ex(wb_ex),
    .wb_refetch(wb_refetch),
    .ID_to_IF_refetch(ID_to_IF_refetch)
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
    .ID_to_EXE_mul_div_op(ID_to_EXE_mul_div_op),
    .BR_BUS(BR_BUS),
    .EXE_allowin(EXE_allowin),
    .IF_to_ID_valid(IF_to_ID_valid),
    .ID_to_EXE_valid(ID_to_EXE_valid),
    .ID_allowin(ID_allowin),
    .ertn_flush(ertn_flush),
    .wb_ex(wb_ex),
    .has_int(has_int),
    .wb_refetch(wb_refetch),
    .ID_to_IF_refetch(ID_to_IF_refetch)
);

EXE_stage myEXE(
    .clk(clk),
    .resetn(resetn),
    .ID_to_EXE_BUS(ID_to_EXE_BUS),
    .ID_to_EXE_mul_div_op(ID_to_EXE_mul_div_op),
    .EXE_to_MEM_BUS(EXE_to_MEM_BUS),
    .EXE_RF_BUS(EXE_RF_BUS),
    .csr_num(csr_rnum),
    .csr_rvalue(csr_rvalue),
    // .data_sram_en(data_sram_en),
    .data_sram_req(data_sram_req),
    .data_sram_wr(data_sram_wr),
    .data_sram_size(data_sram_size),
    .data_sram_wstrb(data_sram_wstrb),
    .data_sram_addr(data_sram_addr),
    .data_sram_wdata(data_sram_wdata),
    .data_sram_addr_ok(data_sram_addr_ok),
    .ID_to_EXE_valid(ID_to_EXE_valid),
    .MEM_allowin(MEM_allowin),
    .EXE_allowin(EXE_allowin),
    .EXE_to_MEM_valid(EXE_to_MEM_valid),
    .ertn_flush(ertn_flush),
    .wb_ex(wb_ex),
    .mem_ertn(mem_ertn),
    .mem_ex(mem_ex),
    .wb_refetch(wb_refetch),
    .mem_refetch(mem_refetch),
    .stable_cnt(stable_cnt),
    .tlb_inst_op_to_csr(tlb_inst_op_from_EXE),
    .invtlb_asid(invtlb_asid),
    .invtlb_vppn(invtlb_vppn),
    .invtlb_op(invtlb_op),
    .MEM_to_EXE_block_tlbsrch(MEM_to_EXE_block_tlbsrch),
    .WB_to_EXE_block_tlbsrch(WB_to_EXE_block_tlbsrch),
    .mem_vaddr(mem_vaddr),
    .mem_paddr(mem_paddr),
    .mem_wr(mem_wr),
    .to_EXE_ex_ade(to_EXE_ex_ade),
    .to_EXE_ex_tlbr(to_EXE_ex_tlbr),
    .to_EXE_ex_pil(to_EXE_ex_pil),
    .to_EXE_ex_pis(to_EXE_ex_pis),
    .to_EXE_ex_ppi(to_EXE_ex_ppi),
    .to_EXE_ex_pme(to_EXE_ex_pme)
);

MEM_stage myMEM(
    .clk(clk),
    .resetn(resetn),
    .data_sram_data_ok(data_sram_data_ok),
    .data_sram_rdata(data_sram_rdata),
    .EXE_to_MEM_BUS(EXE_to_MEM_BUS),
    .MEM_to_WB_BUS(MEM_to_WB_BUS),
    .MEM_RF_BUS(MEM_RF_BUS),
    .EXE_to_MEM_valid(EXE_to_MEM_valid),
    .WB_allowin(WB_allowin),
    .MEM_allowin(MEM_allowin),
    .MEM_to_WB_valid(MEM_to_WB_valid),
    .IO_cnt(data_IO_cnt),
    .ertn_flush(ertn_flush),
    .wb_ex(wb_ex),
    .wb_refetch(wb_refetch),
    .mem_ertn(mem_ertn),
    .mem_ex(mem_ex),
    .mem_refetch(mem_refetch),
    .MEM_to_EXE_block_tlbsrch(MEM_to_EXE_block_tlbsrch)
);

WB_stage myWB(
    .clk(clk),
    .resetn(resetn),
    .csr_num(csr_wnum),
    .csr_we(csr_we),
    .csr_wvalue(csr_wvalue),
    .csr_wmask(csr_wmask),
    .wb_pc(wb_pc),
    .wb_vaddr(wb_vaddr),
    .ertn_flush(ertn_flush),
    .wb_ex(wb_ex),
    .wb_ecode(wb_ecode),
    .wb_esubcode(wb_esubcode),
    .wb_refetch(wb_refetch),
    .debug_wb_pc(debug_wb_pc),
    .debug_wb_rf_we(debug_wb_rf_we),
    .debug_wb_rf_wnum(debug_wb_rf_wnum),
    .debug_wb_rf_wdata(debug_wb_rf_wdata),
    .MEM_to_WB_BUS(MEM_to_WB_BUS),
    .RF_BUS(RF_BUS),
    .WB_RF_BUS(WB_RF_BUS),
    .MEM_to_WB_valid(MEM_to_WB_valid),
    .WB_allowin(WB_allowin),
    .stable_cnt_tid(stable_cnt_tid),
    .tlb_inst_op_to_csr(tlb_inst_op_from_WB),
    .WB_to_EXE_block_tlbsrch(WB_to_EXE_block_tlbsrch)
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
    .wb_vaddr(wb_vaddr),
    .hw_int_in(hw_int_in),
    .ipi_int_in(ipi_int_in),
    .ex_ra(ex_ra),
    .ex_entry(ex_entry),
    .has_int(has_int),
    .ertn_flush(ertn_flush),
    .wb_ex(wb_ex),
    .wb_ecode(wb_ecode),
    .wb_esubcode(wb_esubcode),
    .stable_cnt(stable_cnt),
    .stable_cnt_tid(stable_cnt_tid),
    .tlb_inst_op(tlb_inst_op_from_EXE | tlb_inst_op_from_WB),
    .invtlb_asid(invtlb_asid),
    .invtlb_vppn(invtlb_vppn),
    .invtlb_op(invtlb_op),
    .next_pc_vaddr(next_pc_vaddr),
    .next_pc_paddr(next_pc_paddr),
    .to_PreIF_ex_ade(to_PreIF_ex_ade),
    .to_PreIF_ex_tlbr(to_PreIF_ex_tlbr),
    .to_PreIF_ex_pif(to_PreIF_ex_pif),
    .to_PreIF_ex_ppi(to_PreIF_ex_ppi),
    .mem_vaddr(mem_vaddr),
    .mem_paddr(mem_paddr),
    .mem_wr(mem_wr),
    .to_EXE_ex_ade(to_EXE_ex_ade),
    .to_EXE_ex_tlbr(to_EXE_ex_tlbr),
    .to_EXE_ex_pil(to_EXE_ex_pil),
    .to_EXE_ex_pis(to_EXE_ex_pis),
    .to_EXE_ex_ppi(to_EXE_ex_ppi),
    .to_EXE_ex_pme(to_EXE_ex_pme)
);

assign hw_int_in = 8'b0;
assign ipi_int_in = 1'b0;
endmodule
