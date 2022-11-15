//注释中包含中文，可能需要使用GB编码打开,如果显示正常可以无视
//if the notes are wrong characters in Chinese
//please turn 'UTF-8' to 'GB2312',and please save the file correctly

module transfer_bridge(
    input wire              aclk,
    input wire              aresetn,

    //read request
    output wire  [ 3:0]      arid,
    output wire [31:0]      araddr,
    output wire [ 7:0]      arlen,
    output wire [ 2:0]      arsize,
    output wire [ 1:0]      arburst,
    output wire [ 1:0]      arlock,
    output wire [ 3:0]      arcache,
    output wire [ 2:0]      arprot,
    output wire             arvalid,
    input  wire             arready,

    //read response
    input  wire [ 3:0]      rid,
    input  wire [31:0]      rdata,
    input  wire [ 1:0]      rresp,
    input  wire             rlast,
    input  wire             rvalid,
    output wire             rready,

    //write request
    output wire [ 3:0]      awid,
    output wire [31:0]      awaddr,
    output wire [ 7:0]      awlen,
    output wire [ 2:0]      awsize,
    output wire [ 1:0]      awburst,
    output wire [ 1:0]      awlock,
    output wire [ 3:0]      awcache,
    output wire [ 2:0]      awprot,
    output wire             awvalid,
    input  wire             awready,

    //write data
    output wire [ 3:0]      wid,
    output wire [31:0]      wdata,
    output wire [ 3:0]      wstrb,
    output wire             wlast,
    output wire             wvalid,
    input  wire             wready,

    //write response
    input  wire [ 3:0]      bid,
    input  wire [ 1:0]      bresp,
    input  wire             bvalid,
    output wire             bready,

    //scram inst
    input  wire             inst_sram_req,
    input  wire             inst_sram_wr,
    input  wire [ 1:0]      inst_sram_size,
    input  wire [31:0]      inst_sram_addr,
    input  wire [ 3:0]      inst_sram_wstrb,
    input  wire [31:0]      inst_sram_wdata,
    output wire             inst_sram_addr_ok,
    output wire             inst_sram_data_ok,
    output wire [31:0]      inst_sram_rdata,

    //scram data
    input  wire              data_sram_req,
    input  wire              data_sram_wr,
    input  wire  [ 1:0]      data_sram_size,
    input  wire  [31:0]      data_sram_addr,
    input  wire [31:0]      data_sram_wdata,
    input  wire [ 3:0]      data_sram_wstrb,
    output wire             data_sram_addr_ok,
    output wire             data_sram_data_ok,
    output wire [31:0]      data_sram_rdata
);

//read request stable part
assign arlen = 8'h00;
assign arburst = 2'b01;
assign arlock = 2'b00;
assign arcache = 1'h0;
assign arprot = 3'b000;

//read response stable part
/*----ignore 'rresp' and 'rlast'----*/

//wirte request stable part
assign awid = 4'h1;
assign awlen = 2'h00;
assign awburst = 2'b01;
assign awlock = 2'b00;
assign awcache = 1'h0;
assign awprot = 3'b000;

//write data stable part
assign wid = 4'h1;
assign wlast = 1'b1;

//write response stable part
/*----ignore 'bid' and 'bresp'----*/

/*--- regs of the output signal for axi master ---*/
//read request 'ar'
reg [ 3:0] axi_ar_id;
reg [31:0] axi_ar_addr;
reg [ 2:0] axi_ar_size;
reg        axi_ar_valid;

//read response 'r'
reg        axi_r_ready;

//write resquest 'aw'and 'w'
reg [31:0] axi_aw_addr;
reg [ 2:0] axi_aw_size;
reg        axi_aw_valid;
reg [31:0] axi_w_data;
reg [ 3:0] axi_w_strb;
reg        axi_w_valid;

//write reponse 'b'
reg        axi_b_ready;

/*--- definition of other signals ---*/

/*--- read request ---*/
wire        read_req_valid;
wire        read_req_inst_valid;
wire        read_req_data_valid;
wire [31:0] read_req_addr;
wire [ 3:0] read_req_id;
wire [ 1:0] read_req_size;

/*--- read request completed ---*/
wire        read_inst_req_ok;
wire        read_data_req_ok;

/*--- read response ---*/
wire inst_sram_r_wait;
wire data_sram_r_wait;

wire read_ready;

wire read_inst_wait;
wire read_data_wait;

wire read_resp_inst_valid;
wire read_resp_data_valid;

wire [31:0] read_data;

/*--- write request ---*/
wire        write_req_valid;
wire [31:0] write_req_addr;
wire [ 1:0] write_req_size;
wire [ 3:0] write_req_strb;
wire [31:0] write_req_data;

/*--- write request completed ---*/
wire        write_data_req_ok;

/*--- write response completed ---*/
wire axi_b_ok;

/*--- data response buffer control sig---*/
wire data_resp_wen;
wire data_resp_ren;
wire data_resp_empty;
wire data_resp_full;

/*--- write request buffer control sig---*/
wire write_resp_empty;
wire write_resp_full;
wire data_resp_write;

/*--- RAW related judge ---*/
wire related;

/*--- read or write resp for data_sram ---*/
wire data_resp_RorW;
wire data_FIFO_ren;




/*--- State Machine ---*/

/*--- read request ---*/
//考虑当前没有正在等待握手的信号(arready)
//相关握手信号有效时(DATA > INST 且无相关)再发出请求
//握手成功后恢复至原状态
always @(posedge aclk) begin
    if(!aresetn)begin
        axi_ar_id <= 4'h0;
        axi_ar_addr <= 32'h0000;
        axi_ar_size <= 3'b000;
        axi_ar_valid <= 1'b0;
    end
    else if(!axi_ar_valid && read_req_valid)begin
        axi_ar_id <= read_req_id;
        axi_ar_addr <= read_req_addr;
        axi_ar_size <= read_req_size;
        axi_ar_valid <= 1'b1;
    end
    else if(axi_ar_valid && arready)begin
        axi_ar_id <= 4'h0;
        axi_ar_addr <= 32'h0000;
        axi_ar_size <= 3'b000;
        axi_ar_valid <= 1'b0;
    end 
end

assign arid = axi_ar_id;
assign araddr = axi_ar_addr;
assign arsize = axi_ar_size;
assign arvalid = axi_ar_valid;


assign read_req_valid = read_req_inst_valid || read_req_data_valid;
//judge inst or data req valid, data > inst
assign read_req_inst_valid = inst_sram_req && ~read_req_data_valid;
//judge RAW with sig 'related'
assign read_req_data_valid = data_sram_req && ~data_sram_wr && !related;
//judge inst or data data > inst
assign read_req_id = read_req_data_valid ? 4'h1 : 4'h0;
assign read_req_addr = read_req_data_valid ? data_sram_addr : inst_sram_addr;
assign read_req_size = read_req_data_valid ? data_sram_size : inst_sram_size;

//读请求握手成功，握手成功后一拍arvalid将变低，此时请求完成
//即aready拉高的后一拍表明握手成功,并发出相关的addr_ok信号
assign read_inst_req_ok = read_req_inst_valid && ~arvalid;
assign read_data_req_ok = read_req_data_valid && ~arvalid;

assign inst_sram_addr_ok = read_inst_req_ok;
assign data_sram_addr_ok = read_data_req_ok || write_data_req_ok;


/*--- read response ---*/
//由于维护两个读数据的队列，分别用于存储inst和data读数据的队列
//从而采取队列为非满时表明已经准备好接受数据
always @(posedge aclk) begin
    if(!aresetn)begin
        axi_r_ready <= 1'b0;
    end
    else begin
        axi_r_ready <= read_ready;
    end
end

assign rready = axi_r_ready;

//判断是否准备好接受读数据
assign read_ready = ~read_inst_wait && ~read_data_wait;
//读响应握手成功，且根据rid区分inst和data
assign read_resp_inst_valid = rvalid && rready && rid == 4'h0;
assign read_resp_data_valid = rvalid && rready && rid == 4'h1;
assign read_data = rdata;

//为data读数据buffer准备的判断条件
//由于data部分可能会出现st和ld的先后
//为保证返回时的顺序正确，从而当且仅当data_sram的等待响应队列为读取请求时才允许从buffer中读取数据
assign data_FIFO_ren = ~data_sram_r_wait && ~data_resp_RorW;

/*--- inst_read_buffer ---*/
FIFO inst_buffer(
    .clk(aclk),                         
    .resetn(aresetn),
    .FIFO_in(read_resp_inst_valid),         //读响应握手成功时写入队列
    .FIFO_out(1'b1),                        //由于inst buffer只有读请求出现，从而允许一直读取
    .empty(inst_sram_r_wait),               //用于表示是否有数据存储，用于判断inst_data_ok
    .full(read_inst_wait),                  //判断队列是否为满，判断是否准备好接受数据
    .data_in(read_data),
    .data_out(inst_sram_rdata)
);

/*--- data_read_buffer ---*/
FIFO data_buffer(
    .clk(aclk),
    .resetn(aresetn),
    .FIFO_in(read_resp_data_valid),
    .FIFO_out(data_FIFO_ren),               //只有data_sram等待读响应时才允许读出
    .empty(data_sram_r_wait),
    .full(read_data_wait),
    .data_in(read_data),
    .data_out(data_sram_rdata)
);


//write request and data
//相关判断逻辑与读请求类似，握手请求需要两个通道均无等待请求才能执行
always @(posedge aclk) begin
    if(!aresetn)begin
        axi_aw_addr <= 32'h0000;
        axi_aw_size <= 3'b000;
        axi_aw_valid <= 1'b0;
    end
    else if(!axi_aw_valid && !axi_w_valid && write_req_valid) begin
        axi_aw_addr <= write_req_addr;
        axi_aw_size <= write_req_size;
        axi_aw_valid <= 1'b1;
    end
    else if(axi_aw_valid && awready)begin
        axi_aw_addr <= 32'h0000;
        axi_aw_size <= 3'b000;
        axi_aw_valid <= 1'b0;
    end

    if(!aresetn)begin
        axi_w_data <= 32'h0000;
        axi_w_strb <= 4'h0;
        axi_w_valid <= 1'b0;
    end
    else if(!axi_w_valid && !axi_aw_valid && write_req_valid) begin
        axi_w_data <= write_req_data;
        axi_w_strb <= write_req_strb;
        axi_w_valid <= 1'b1;
    end
    else if(axi_w_valid && wready)begin
        axi_w_data <= 32'h0000;
        axi_w_strb <= 4'h0;
        axi_w_valid <= 1'b0;
    end 
end

//写请求判断，不能发生RAW相关
assign write_req_valid = data_sram_req && data_sram_wr && !related;
//写请求和写数据握手成功
assign write_data_req_ok = write_req_valid && ~axi_aw_valid && ~axi_w_valid;

assign write_req_addr = data_sram_addr;
assign write_req_size = data_sram_size;
assign write_req_strb = data_sram_wstrb;
assign write_req_data = data_sram_wdata;

assign awaddr = axi_aw_addr;
assign awsize = axi_aw_size;
assign awvalid = axi_aw_valid;

assign wdata = axi_w_data;
assign wstrb = axi_w_strb;
assign wvalid = axi_w_valid;

//write response

always @(posedge aclk) begin
    if(!aresetn)begin
        axi_b_ready <= 1'b0;
    end
    else begin
        axi_b_ready <= !write_resp_full;
    end
    
end

assign bready = axi_b_ready;
assign axi_b_ok = bvalid & bready;

//由于原有的写请求队列扩充为读写请求队列
//从而增加了一个队列用于存储写相应判断，用于判断data_ok
//当写响应握手成功时，表明请求队列中的某一个写请求已经完成
FIFO  write_data_resp_ok(
    .clk(aclk),
    .resetn(aresetn),
    .FIFO_in(axi_b_ok),
    .FIFO_out(data_resp_write),
    .empty(write_resp_empty),
    .full(write_resp_full),
    .data_in(),
    .data_out()
);

//当存储的data端的请求队列非空且队首为写请求时，允许单独计数
assign data_resp_write = !data_resp_empty & data_resp_RorW;

//用于存储data请求握手成功但等待返回相应的部分，同时用于判断RAW
FIFO_RAW data_write_resp(
    .clk(aclk),
    .resetn(aresetn),
    .FIFO_in(data_resp_wen),
    .FIFO_out(data_resp_ren),
    .empty(data_resp_empty),
    .full(data_resp_full),
    .data_in(axi_aw_addr),
    .data_wr(data_sram_wr),
    .data_out(),
    .wr_out(data_resp_RorW),
    .addr_related(data_sram_addr),
    .related(related)
);

//对data端传来请求的判断
//所有读写请求握手成功后都加入队列中，其中写请求同时用来判断数据相关
//出队要求为data_ok准备好时才能出队
assign data_resp_wen = data_sram_addr_ok;
assign data_resp_ren = data_sram_data_ok;

//对inst
//当相应buffer非空即可发出data_ok
assign inst_sram_data_ok = !inst_sram_r_wait;
//对data
//若当前等待返回的为读请求，则根据相应buffer非空来判断
//否则若等待返回的为写请求，则根据是否存在等待的写请求来判断
assign data_sram_data_ok = !data_sram_r_wait && ~data_resp_RorW || !write_resp_empty && data_resp_RorW;

endmodule