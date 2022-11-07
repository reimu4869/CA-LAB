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
wire        read_req_valid;
wire        read_inst_req_valid;
wire        read_data_req_valid;
wire [31:0] read_req_addr;
wire [ 3:0] read_req_id;
wire [ 1:0] read_req_size;

wire        read_inst_req_ok;
wire        read_data_req_ok;

wire        write_req_valid;
wire [31:0] write_req_addr;
wire [ 1:0] write_req_size;
wire [ 3:0] write_req_strb;
wire [31:0] write_req_data;
wire        write_data_req_ok;

wire axi_b_ok;

wire data_resp_wen;
wire write_data_resp_empty;
wire write_data_resp_full;

wire related;

wire data_resp_RorW;
wire data_FIFO_ren;


/*--- State Machine ---*/
//read request
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

assign read_req_valid = read_inst_req_valid || read_data_req_valid;
//judge inst or data req valid, data > inst
assign read_inst_req_valid = inst_sram_req && ~read_data_req_valid;
assign read_data_req_valid = data_sram_req && ~data_sram_wr && !related;

assign read_req_id = read_data_req_valid ? 4'h1 : 4'h0;
assign read_req_addr = read_data_req_valid ? data_sram_addr : inst_sram_addr;
assign read_req_size = read_data_req_valid ? data_sram_size : inst_sram_size;

//read response
wire inst_sram_r_wait;
wire data_sram_r_wait;

wire read_ready;

wire read_inst_wait;
wire read_data_wait;

wire read_inst_resp_valid;
wire read_data_resp_valid;

wire [31:0] read_data;

always @(posedge aclk) begin
    if(!aresetn)begin
        axi_r_ready <= 1'b0;
    end
    else if(read_ready)begin
        axi_r_ready <= 1'b1;
    end
    else begin
        axi_r_ready <= read_ready;
    end
end

assign rready = axi_r_ready;

assign read_ready = ~read_inst_wait && ~read_data_wait;
assign read_inst_resp_valid = rvalid && rready && rid == 4'h0;
assign read_data_resp_valid = rvalid && rready && rid == 4'h1;
assign read_data = rdata;

assign data_FIFO_ren = ~data_sram_r_wait && ~data_resp_RorW;

/*--- inst_read_buffer ---*/
FIFO inst_buffer(
    .clk(aclk),
    .resetn(aresetn),
    .FIFO_in(read_inst_resp_valid),
    .FIFO_out(1'b1),
    .empty(inst_sram_r_wait),
    .full(read_inst_wait),
    .data_in(read_data),
    .data_out(inst_sram_rdata)
);

/*--- data_read_buffer ---*/
FIFO data_buffer(
    .clk(aclk),
    .resetn(aresetn),
    .FIFO_in(read_data_resp_valid),
    .FIFO_out(data_FIFO_ren),
    .empty(data_sram_r_wait),
    .full(read_data_wait),
    .data_in(read_data),
    .data_out(data_sram_rdata)
);


assign read_inst_req_ok = read_inst_req_valid && ~arvalid;
assign read_data_req_ok = read_data_req_valid && ~arvalid;

assign inst_sram_addr_ok = read_inst_req_ok;
assign data_sram_addr_ok = read_data_req_ok || write_data_req_ok;

//write request and data
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

assign write_req_valid = data_sram_req && data_sram_wr && !related;
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
    else if(axi_b_ready && bvalid)begin
        axi_b_ready <= 1'b0;
    end
    else if(!write_data_resp_empty)begin
        axi_b_ready <= 1'b1;
    end
    
end

wire data_resp_ren;
wire write_req_empty;
wire data_resp_write;

assign data_resp_wen = data_sram_addr_ok;
assign data_resp_ren = data_sram_data_ok;
assign bready = axi_b_ready;
assign axi_b_ok = bvalid & bready;


assign data_resp_write = !write_data_resp_empty & data_resp_RorW;

FIFO  write_data_req(
    .clk(aclk),
    .resetn(aresetn),
    .FIFO_in(axi_b_ok),
    .FIFO_out(data_resp_write),
    .empty(write_req_empty),
    .full(),
    .data_in(),
    .data_out()
);

FIFO_RAW data_write_resp(
    .clk(aclk),
    .resetn(aresetn),
    .FIFO_in(data_resp_wen),
    .FIFO_out(data_resp_ren),
    .empty(write_data_resp_empty),
    .full(write_data_resp_full),
    .data_in(axi_aw_addr),
    .data_wr(data_sram_wr),
    .data_out(),
    .wr_out(data_resp_RorW),
    .addr_related(data_sram_addr),
    .related(related)
);

assign inst_sram_data_ok = !inst_sram_r_wait;
assign data_sram_data_ok = !data_sram_r_wait && ~data_resp_RorW || !write_req_empty && data_resp_RorW;

endmodule