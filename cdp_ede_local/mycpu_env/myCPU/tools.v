module decoder_2_4(
    input  wire [ 1:0] in,
    output wire [ 3:0] out
);

genvar i;
generate for (i=0; i<4; i=i+1) begin : gen_for_dec_2_4
    assign out[i] = (in == i);
end endgenerate

endmodule


module decoder_4_16(
    input  wire [ 3:0] in,
    output wire [15:0] out
);

genvar i;
generate for (i=0; i<16; i=i+1) begin : gen_for_dec_4_16
    assign out[i] = (in == i);
end endgenerate

endmodule


module decoder_5_32(
    input  wire [ 4:0] in,
    output wire [31:0] out
);

genvar i;
generate for (i=0; i<32; i=i+1) begin : gen_for_dec_5_32
    assign out[i] = (in == i);
end endgenerate

endmodule


module decoder_6_64(
    input  wire [ 5:0] in,
    output wire [63:0] out
);

genvar i;
generate for (i=0; i<63; i=i+1) begin : gen_for_dec_6_64
    assign out[i] = (in == i);
end endgenerate

endmodule

module FIFO #(
    parameter DATA_WIDTH = 32,
    parameter BUFF_DEPTH = 4,
    parameter ADDR_WIDTH = 2
)(
    input wire clk, 
    input wire resetn,
    
    input wire FIFO_in,
    input wire FIFO_out,

    output wire empty,
    output wire full,

    input wire [DATA_WIDTH-1:0] data_in,
    output wire [DATA_WIDTH-1:0] data_out
);

reg [ADDR_WIDTH-1 : 0] FIFO_count;
reg [ADDR_WIDTH-1 : 0] FIFO_head, FIFO_tail;
reg [DATA_WIDTH-1 : 0] buffer [BUFF_DEPTH-1 : 0];

wire in_valid;
wire out_valid;

assign in_valid = FIFO_in && ~full;
assign out_valid = FIFO_out && ~empty;

assign empty = FIFO_count == 0;
assign full = FIFO_count == BUFF_DEPTH-1;

assign data_out = buffer[FIFO_tail];

always @ (posedge clk) begin
    if (!resetn)
        FIFO_count <= 0;
    else if (in_valid && ~out_valid)
        FIFO_count <= FIFO_count + 1'b1;
    else if (out_valid && ~in_valid)
        FIFO_count <= FIFO_count - 1'b1;
end

always @ (posedge clk) begin
    if (!resetn)
        buffer[FIFO_head] <= 0;
    else if (in_valid)
        buffer[FIFO_head] <= data_in;
end

always @ (posedge clk) begin
    if (!resetn)
        FIFO_head <= 0;
    else if(in_valid && (FIFO_head == (BUFF_DEPTH-1)))
        FIFO_head <= 0;
    else if (in_valid)
        FIFO_head <= FIFO_head + 1'b1;
end

always @ (posedge clk) begin
    if (!resetn)
        FIFO_tail <= 0;
    else if(out_valid && (FIFO_tail == (BUFF_DEPTH-1)))
        FIFO_tail <= 0;
    else if (out_valid)
        FIFO_tail <= FIFO_tail + 1'b1;
end

endmodule

module FIFO_RAW #(
    parameter DATA_WIDTH = 32,
    parameter BUFF_DEPTH = 4,
    parameter ADDR_WIDTH = 2
)(
    input wire clk, 
    input wire resetn,
    
    input wire FIFO_in,
    input wire FIFO_out,

    output wire empty,
    output wire full,

    input wire [DATA_WIDTH-1:0] data_in,
    input wire                  data_wr,

    output wire [DATA_WIDTH-1:0] data_out,
    output wire                  wr_out, 

    input wire [DATA_WIDTH-1:0] addr_related,
    output wire related 
);

reg [ADDR_WIDTH-1 : 0] FIFO_count;
reg [ADDR_WIDTH-1 : 0] FIFO_head, FIFO_tail;
reg [DATA_WIDTH-1 : 0] buffer [BUFF_DEPTH-1 : 0];
reg                    valid_buffer [BUFF_DEPTH-1 : 0];
reg                    RorW [BUFF_DEPTH-1 : 0];
wire in_valid;
wire out_valid;

wire [BUFF_DEPTH-1 : 0] related_vector ;

assign in_valid = FIFO_in && ~full;
assign out_valid = FIFO_out && ~empty;

assign empty = FIFO_count == 0;
assign full = FIFO_count == BUFF_DEPTH-1;

assign data_out = buffer[FIFO_tail];
assign related = |related_vector;
assign wr_out = RorW[FIFO_tail];


always @ (posedge clk) begin
    if (!resetn)
        FIFO_count <= 0;
    else if (in_valid && ~out_valid)
        FIFO_count <= FIFO_count + 1'b1;
    else if (out_valid && ~in_valid)
        FIFO_count <= FIFO_count - 1'b1;
end

/*always @ (posedge clk) begin
    if (!resetn)begin
        buffer[FIFO_head] <= 0;
        valid_buffer[FIFO_head] <= 0;
    end
    else if (in_valid)begin
        buffer[FIFO_head] <= data_in;
        buffer[FIFO_head] <= 1'b1;
    end
end*/

always @ (posedge clk) begin
    if (!resetn)
        FIFO_head <= 0;
    else if(in_valid && (FIFO_head == (BUFF_DEPTH-1)))
        FIFO_head <= 0;
    else if (in_valid)
        FIFO_head <= FIFO_head + 1'b1;
end

always @ (posedge clk) begin
    if (!resetn)
        FIFO_tail <= 0;
    else if(out_valid && (FIFO_tail == (BUFF_DEPTH-1)))
        FIFO_tail <= 0;
    else if (out_valid)
        FIFO_tail <= FIFO_tail + 1'b1;
end

genvar i;
generate for (i = 0; i < BUFF_DEPTH; i = i + 1) begin:gen_buff
    always @ (posedge clk) begin
        if (!resetn) begin
            buffer[i] <= 0;
            RorW[i] <= 0;
            valid_buffer[i] <= 0;
        end else if (out_valid && FIFO_tail == i) begin
            buffer[i] <= 0;
            RorW[i] <= 0;
            valid_buffer[i] <= 0;
        end else if (in_valid && FIFO_head == i) begin
            buffer[i] <= data_in;
            valid_buffer[i] <= 1;
            RorW[i] <= data_wr;
        end
    end
    assign related_vector[i] = 
        valid_buffer[i] && addr_related == buffer[i][DATA_WIDTH - 1:0] && RorW[i];
end
    
endgenerate

endmodule
