module cache(
    input  wire         clk,
    input  wire         resetn,

    // cache - cpu
    input  wire         valid,
    input  wire         op,
    input  wire [ 7: 0] index,
    input  wire [19: 0] tag,
    input  wire [ 3: 0] offset,
    input  wire [ 3: 0] wstrb,
    input  wire [31: 0] wdata,

    output wire         addr_ok,
    output wire         data_ok,
    output wire [31: 0] rdata,

    // cache - axi
    output wire         rd_req,
    output wire [ 2: 0] rd_type,
    output wire [31: 0] rd_addr,
    input  wire         rd_rdy,
    input  wire         ret_valid,
    input  wire         ret_last,
    input  wire [31: 0] ret_data,

    output wire         wr_req,
    output wire [ 2: 0] wr_type,
    output wire [31: 0] wr_addr,
    output wire [ 3: 0] wr_wstrb,
    output wire [127:0] wr_data,
    input  wire         wr_rdy
);


// tagv ram

wire [ 7:0] way0_tagv_addr;
wire [20:0] way0_tagv_din;
wire [20:0] way0_tagv_dout;
wire        way0_tagv_we;

wire [ 7:0] way1_tagv_addr;
wire [20:0] way1_tagv_din;
wire [20:0] way1_tagv_dout;
wire        way1_tagv_we;

tagv_ram way0_tagv(
    .addra  (way0_tagv_addr),
    .clka   (clk),
    .dina   (way0_tagv_din),
    .douta  (way0_tagv_dout),
    .ena    (1'b1),
    .wea    (way0_tagv_we)
);

tagv_ram way1_tagv(
    .addra  (way1_tagv_addr),
    .clka   (clk),
    .dina   (way1_tagv_din),
    .douta  (way1_tagv_dout),
    .ena    (1'b1),
    .wea    (way1_tagv_we)
);


// dirty array

reg  [255:0] way0_d;
reg  [255:0] way1_d;


// data bank ram

wire [ 7:0] way0_bank_addr  [3:0];
wire [31:0] way0_bank_din   [3:0];
wire [31:0] way0_bank_dout  [3:0];
wire [ 3:0] way0_bank_we    [3:0];

wire [ 7:0] way1_bank_addr  [3:0];
wire [31:0] way1_bank_din   [3:0];
wire [31:0] way1_bank_dout  [3:0];
wire [ 3:0] way1_bank_we    [3:0];

genvar i;
generate
    for (i=0;i<4;i=i+1) begin
        data_bank_ram way0_bank(
            .addra  (way0_bank_addr[i]),
            .clka   (clk),
            .dina   (way0_bank_din[i]),
            .douta  (way0_bank_dout[i]),
            .ena    (1'b1),
            .wea    (way0_bank_we[i])
        );

        data_bank_ram way1_bank(
            .addra  (way1_bank_addr[i]),
            .clka   (clk),
            .dina   (way1_bank_din[i]),
            .douta  (way1_bank_dout[i]),
            .ena    (1'b1),
            .wea    (way1_bank_we[i])
        );
    end
endgenerate


// todo

wire hit_write_conflict;
wire way0_hit;
wire way1_hit;
wire cache_hit;
wire hit_write;

reg  replace_req;
reg  replace_way;

reg [1:0] ret_bank_cnt;


// fsm

`define CACHE_MAIN_IDLE     5'b00001
`define CACHE_MAIN_LOOKUP   5'b00010
`define CACHE_MAIN_MISS     5'b00100
`define CACHE_MAIN_REPLACE  5'b01000
`define CACHE_MAIN_REFILL   5'b10000

`define CACHE_MAIN_IS_IDLE      0
`define CACHE_MAIN_IS_LOOKUP    1
`define CACHE_MAIN_IS_MISS      2
`define CACHE_MAIN_IS_REPLACE   3
`define CACHE_MAIN_IS_REFILL    4

`define CACHE_WBUF_IDLE     2'b01
`define CACHE_WBUF_WRITE    2'b10

`define CACHE_WBUF_IS_IDLE      0
`define CACHE_WBUF_IS_WRITE     1

reg  [4:0] main_curr_state;
reg  [4:0] main_next_state;
reg  [1:0] wbuf_curr_state;
reg  [1:0] wbuf_next_state;

always @ (posedge clk) begin
    if (~resetn) begin
        main_curr_state <= `CACHE_MAIN_IDLE;
        wbuf_curr_state <= `CACHE_WBUF_IDLE;
    end else begin
        main_curr_state <= main_next_state;
        wbuf_curr_state <= wbuf_next_state;
    end
end

always @ (*) begin
    if (~resetn) begin
        main_next_state <= `CACHE_MAIN_IDLE;
    end else begin
        case (main_curr_state)
            `CACHE_MAIN_IDLE: begin
                if (valid && !hit_write_conflict) begin
                    main_next_state <= `CACHE_MAIN_LOOKUP;
                end else begin
                    main_next_state <= `CACHE_MAIN_IDLE;
                end
            end

            `CACHE_MAIN_LOOKUP: begin
            if (cache_hit && (!valid || hit_write_conflict)) begin 
                    // no new req / new req conflict
                    main_next_state <= `CACHE_MAIN_IDLE;
                end else if (cache_hit && valid && !hit_write_conflict) begin
                    // new req
                    main_next_state <= `CACHE_MAIN_LOOKUP;
                end else begin
                    main_next_state <= `CACHE_MAIN_MISS;
                end
            end

            `CACHE_MAIN_MISS: begin
                if (!wr_rdy) begin
                    main_next_state <= `CACHE_MAIN_MISS;
                end else begin
                    main_next_state <= `CACHE_MAIN_REPLACE;
                end
            end

            `CACHE_MAIN_REPLACE: begin
                if (!rd_rdy) begin
                    main_next_state <= `CACHE_MAIN_REPLACE;
                end else begin
                    main_next_state <= `CACHE_MAIN_REFILL;
                end
            end

            `CACHE_MAIN_REFILL: begin
                if (ret_valid && ret_last) begin
                    main_next_state <= `CACHE_MAIN_IDLE;
                end else begin
                    main_next_state <= `CACHE_MAIN_REFILL;
                end
            end

            default: begin
                main_next_state <= `CACHE_MAIN_IDLE;
            end
        endcase
    end
end

always @ (*) begin
    if (~resetn) begin
        wbuf_next_state <= `CACHE_WBUF_IDLE;
    end else begin
        case (wbuf_curr_state)
            `CACHE_WBUF_IDLE: begin
                if (hit_write) begin
                    wbuf_next_state <= `CACHE_WBUF_WRITE;
                end else begin
                    wbuf_next_state <= `CACHE_WBUF_IDLE;
                end
            end

            `CACHE_WBUF_WRITE: begin
                if (hit_write) begin
                    wbuf_next_state <= `CACHE_WBUF_WRITE;
                end else begin
                    wbuf_next_state <= `CACHE_WBUF_IDLE;
                end
            end

            default: begin
                wbuf_next_state <= `CACHE_WBUF_IDLE;
            end
        endcase
    end
end


// request buffer

reg         op_r;
reg  [ 7:0] index_r;
reg  [19:0] tag_r;
reg  [ 3:0] offset_r;
reg  [ 3:0] wstrb_r;
reg  [31:0] wdata_r;

always @ (posedge clk) begin
    if (~resetn) begin
        op_r     <= 1'b0;
        index_r  <= 8'b0;
        tag_r    <= 20'b0;
        offset_r <= 4'b0;
        wstrb_r  <= 4'b0;
        wdata_r  <= 32'b0;
    end else begin
        if (valid && addr_ok) begin
            op_r     <= op;
            index_r  <= index;
            tag_r    <= tag;
            offset_r <= offset;
            wstrb_r  <= wstrb;
            wdata_r  <= wdata;
        end
    end
end


// write buffer

reg         wbuf_way0_hit;
reg         wbuf_way1_hit;
reg  [ 7:0] wbuf_index;
reg  [ 3:0] wbuf_offset;
reg  [ 3:0] wbuf_wstrb;
reg  [31:0] wbuf_wdata;

always @ (posedge clk) begin
    if (~resetn) begin
        wbuf_way0_hit <= 1'b0;
        wbuf_way1_hit <= 1'b0;
        wbuf_index  <= 8'b0;
        wbuf_offset <= 4'b0;
        wbuf_wstrb  <= 4'b0;
        wbuf_wdata  <= 32'b0;
    end else begin
        if (hit_write) begin
            wbuf_way0_hit <= way0_hit;
            wbuf_way1_hit <= way1_hit;
            wbuf_index  <= index_r;
            wbuf_offset <= offset_r;
            wbuf_wstrb  <= wstrb_r;
            wbuf_wdata  <= wdata_r;
        end
    end
end

// LOOKUP

assign addr_ok = main_curr_state[`CACHE_MAIN_IS_IDLE] && !hit_write_conflict                    // IDLE -> LOOKUP
              || main_curr_state[`CACHE_MAIN_IS_LOOKUP] && cache_hit && !hit_write_conflict;    // LOOKUP -> LOOKUP

assign way0_tagv_addr = addr_ok ? index : index_r;
assign way1_tagv_addr = addr_ok ? index : index_r;

generate
    for (i=0;i<4;i=i+1) begin
        assign way0_bank_addr[i] = wbuf_way0_hit && (wbuf_offset[3:2] == i) ? wbuf_index : addr_ok ? index : index_r;
        assign way1_bank_addr[i] = wbuf_way1_hit && (wbuf_offset[3:2] == i) ? wbuf_index : addr_ok ? index : index_r;
    end
endgenerate

wire [19:0] way0_tag;
wire        way0_v;
wire [19:0] way1_tag;
wire        way1_v;
assign {way0_tag, way0_v} = way0_tagv_dout;
assign {way1_tag, way1_v} = way1_tagv_dout;

assign way0_hit = way0_v && (way0_tag == tag_r);
assign way1_hit = way1_v && (way1_tag == tag_r);
assign cache_hit = way0_hit || way1_hit;

assign data_ok = main_curr_state[`CACHE_MAIN_IS_LOOKUP] && cache_hit    // LOOKUP -> LOOKUP/IDLE   
    // even if write cache （因为hit write冲突包含了额外的写后读检查，所以无需等到实际写入数据后才返回data_ok）
    || main_curr_state[`CACHE_MAIN_IS_REFILL] && ret_valid && (ret_bank_cnt == offset_r[3:2]);

assign rdata = {32{main_curr_state[`CACHE_MAIN_IS_LOOKUP] && way0_hit}} & way0_bank_dout[offset_r[3:2]]
             | {32{main_curr_state[`CACHE_MAIN_IS_LOOKUP] && way1_hit}} & way1_bank_dout[offset_r[3:2]]
             | {32{main_curr_state[`CACHE_MAIN_IS_REFILL]}} & ret_data;

assign hit_write = main_curr_state[`CACHE_MAIN_IS_LOOKUP] && cache_hit && op_r;    // LOOKUP -> LOOKUP/IDLE

assign hit_write_conflict = hit_write && valid && !op && (offset[3:2] == offset_r[3:2]) && (index == index_r)               // 实际要读的数据还没写进去
                         || wbuf_curr_state[`CACHE_WBUF_IS_WRITE] && valid && !op && (offset[3:2] == wbuf_offset[3:2]);     // 读写落在了同个bank

// WRITE

generate
    for (i=0;i<4;i=i+1) begin
        assign way0_bank_we[i] = {4{wbuf_curr_state[`CACHE_WBUF_IS_WRITE] && wbuf_way0_hit && wbuf_offset[3:2] == i}} & wbuf_wstrb
                               | {4{main_curr_state[`CACHE_MAIN_IS_REFILL] && (replace_way == 0) && ret_valid && (ret_bank_cnt == i)}};
        assign way1_bank_we[i] = {4{wbuf_curr_state[`CACHE_WBUF_IS_WRITE] && wbuf_way1_hit && wbuf_offset[3:2] == i}} & wbuf_wstrb
                               | {4{main_curr_state[`CACHE_MAIN_IS_REFILL] && (replace_way == 1) && ret_valid && (ret_bank_cnt == i)}}; 

        assign way0_bank_din[i] = {32{wbuf_curr_state[`CACHE_WBUF_IS_WRITE]}} & wbuf_wdata
                                | {32{main_curr_state[`CACHE_MAIN_IS_REFILL]}} & (
                                    offset_r[3:2] == i && op_r ? {
                                        wstrb_r[3] ? wdata_r[31:24] : ret_data[31:24],
                                        wstrb_r[2] ? wdata_r[23:16] : ret_data[23:16],
                                        wstrb_r[1] ? wdata_r[15: 8] : ret_data[15: 8],
                                        wstrb_r[0] ? wdata_r[ 7: 0] : ret_data[ 7: 0]
                                    } : ret_data
                                );
        assign way1_bank_din[i] = {32{wbuf_curr_state[`CACHE_WBUF_IS_WRITE]}} & wbuf_wdata
                                | {32{main_curr_state[`CACHE_MAIN_IS_REFILL]}} & (
                                    offset_r[3:2] == i && op_r ? {
                                        wstrb_r[3] ? wdata_r[31:24] : ret_data[31:24],
                                        wstrb_r[2] ? wdata_r[23:16] : ret_data[23:16],
                                        wstrb_r[1] ? wdata_r[15: 8] : ret_data[15: 8],
                                        wstrb_r[0] ? wdata_r[ 7: 0] : ret_data[ 7: 0]
                                    } : ret_data
                                );
    end
endgenerate

always @ (posedge clk) begin
    if (~resetn) begin
        way0_d <= 256'b0;
        way1_d <= 256'b0;
    end else begin
        if (wbuf_curr_state[`CACHE_WBUF_IS_WRITE] && wbuf_way0_hit) begin
            way0_d[wbuf_index] <= 1'b1;
        end else if (main_curr_state[`CACHE_MAIN_IS_REFILL] && (replace_way == 0) && ret_valid && ret_last && op_r) begin
            way0_d[index_r] <= 1'b1;
        end

        if (wbuf_curr_state[`CACHE_WBUF_IS_WRITE] && wbuf_way1_hit) begin
            way1_d[wbuf_index] <= 1'b1;
        end else if (main_curr_state[`CACHE_MAIN_IS_REFILL] && (replace_way == 1) && ret_valid && ret_last && op_r) begin
            way1_d[index_r] <= 1'b1;
        end
    end
end

// miss buffer

reg  random;

always @ (posedge clk) begin
    random <= $random % 2;
end

always @ (posedge clk) begin
    if (~resetn) begin
        replace_req <= 1'b0;
    end else if (main_curr_state[`CACHE_MAIN_IS_MISS] && wr_rdy) begin  // MISS -> REPLACE
        replace_req <= (random == 0) ? way0_v && way0_d[index_r] : way1_v && way1_d[index_r];
        replace_way <= random;
    end else begin
        replace_req <= 1'b0;
        replace_way <= replace_way;
    end
end

// MISS

assign wr_req = replace_req;
assign wr_type = 3'b100;
assign wr_addr = (replace_way == 0) ? {way0_tag, index_r, 4'b0} : {way1_tag, index_r, 4'b0};
assign wr_wstrb = 4'b1111;
assign wr_data = (replace_way == 0) ? 
    {way0_bank_dout[3], way0_bank_dout[2], way0_bank_dout[1], way0_bank_dout[0]} :
    {way1_bank_dout[3], way1_bank_dout[2], way1_bank_dout[1], way1_bank_dout[0]};

// REPLACE

assign rd_req = main_curr_state[`CACHE_MAIN_IS_REPLACE];
assign rd_type = 3'b100;
assign rd_addr = {tag_r, index_r, 4'b0};

// REFILL

always @ (posedge clk) begin
    if (~resetn) begin
        ret_bank_cnt <= 2'b0;
    end else if (main_curr_state[`CACHE_MAIN_IS_REFILL] && ret_valid && ret_last) begin
        ret_bank_cnt <= 2'b0;
    end else if (main_curr_state[`CACHE_MAIN_IS_REFILL] && ret_valid && !ret_last) begin
        ret_bank_cnt <= ret_bank_cnt + 2'b1;
    end else begin
        ret_bank_cnt <= ret_bank_cnt;
    end
end

generate
    for (i=0;i<4;i=i+1) begin
        assign way0_tagv_we = main_curr_state[`CACHE_MAIN_IS_REFILL] && (replace_way == 0) && ret_valid && ret_last;
        assign way1_tagv_we = main_curr_state[`CACHE_MAIN_IS_REFILL] && (replace_way == 1) && ret_valid && ret_last;

        assign way0_tagv_din = {tag_r, 1'b1};
        assign way1_tagv_din = {tag_r, 1'b1};
    end
endgenerate

endmodule