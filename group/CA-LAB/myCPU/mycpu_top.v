module mycpu_top(
    input  wire        clk,
    input  wire        resetn,
    // inst sram interface
    output wire        inst_sram_en,
    output wire [ 3:0] inst_sram_we,                        
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


wire [31:0] seq_pc;
wire [31:0] fs_pc;
wire [31:0] ds_pc;
wire [31:0] nextpc;
wire        br_taken;
wire        br_taken_cancel;
wire [31:0] br_target;
wire        rj_eq_rd;


wire [11:0] alu_op;
wire        load_op;
wire        src1_is_pc;
wire        src2_is_imm;
wire        res_from_mem;
wire        dst_is_r1;
wire        gr_we;
wire        mem_we;
wire        src_reg_is_rd;
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
wire [11:0] i12;
wire [19:0] i20;
wire [15:0] i16;
wire [25:0] i26;

wire [63:0] op_31_26_d;    //指令码不同段
wire [15:0] op_25_22_d;
wire [ 3:0] op_21_20_d;
wire [31:0] op_19_15_d;

wire        inst_add_w;    //指令码判断
wire        inst_sub_w;
wire        inst_slt;
wire        inst_slti;
wire        inst_sltu;
wire        inst_sltui;
wire        inst_nor;
wire        inst_and;
wire        inst_andi;
wire        inst_or;
wire        inst_ori;
wire        inst_xor;
wire        inst_xori;
wire        inst_slli_w;
wire        inst_srli_w;
wire        inst_srai_w;
wire        inst_sll_w;
wire        inst_srl_w;
wire        inst_sra_w;
wire        inst_addi_w;
wire        inst_ld_w;
wire        inst_st_w;
wire        inst_jirl;
wire        inst_b;
wire        inst_bl;
wire        inst_beq;
wire        inst_bne;
wire        inst_lu12i_w;
wire        inst_pcaddu12i;
wire        inst_mul_w;
wire        inst_mulh_w;
wire        inst_mulh_wu;
wire        inst_div_w;                         
wire        inst_div_wu;
wire        inst_mod_w;
wire        inst_mod_wu;

wire        need_ui5;
wire        need_ui12;
wire        need_si12;
wire        need_si16;
wire        need_si20;
wire        need_si26;
wire        src2_is_4;

wire [ 4:0] rf_raddr1;
wire [31:0] rf_rdata1;
wire [ 4:0] rf_raddr2;
wire [31:0] rf_rdata2;
wire        rf_we   ;
wire [ 4:0] rf_waddr;
wire [31:0] rf_wdata;

wire [31:0] alu_src1   ;
wire [31:0] alu_src2   ;
wire [31:0] alu_result ;
wire [32:0] mul_src1   ;
wire [32:0] mul_src2   ;
wire [65:0] mul_result ;
wire [3:0]  alu_op2;       
wire        dividend_valid;
wire        dividend_ready;
wire        divisor_valid;
wire        divisor_ready;
wire        dividendu_valid;
wire        dividendu_ready;
wire        divisoru_valid;
wire        divisoru_ready;
wire        dout_valid ;
wire        doutu_valid;
wire [63:0] div_result ;
wire [63:0] divu_result;
wire [63:0] div_mul_result;

wire [31:0] result;
wire [31:0] mem_result;
wire [31:0] final_result;

//IFreg
wire if_to_id_valid;
wire if_ready_go;
wire if_allowin;
wire validin;
reg  [31:0] if_pc;
reg         if_valid;

//IDreg
wire id_ready_go;
wire id_allowin;
wire id_to_exe_valid;
reg        id_valid;
reg [31:0] id_pc;
reg [31:0] inst;

//EXEreg
wire exe_ready_go;
wire exe_allowin;
wire exe_to_mem_valid;
reg exe_valid;
reg [11:0] exe_alu_op;
reg [31:0] exe_pc;
reg [ 4:0] exe_dest;
reg [31:0] exe_src1;
reg [31:0] exe_src2;
reg        exe_rf_we;
reg        exe_res_from_mem;
reg [11:0] exe_alu_op;
reg [ 3:0] exe_data_sram_we;
reg        exe_data_sram_en;
reg [31:0] exe_data_sram_wdata;
reg [ 3:0] exe_op;
reg        exe_dividend_valid;
reg        exe_divisor_valid;
reg        exe_dividendu_valid;
reg        exe_divisoru_valid;


//MEMreg
wire mem_ready_go;
wire mem_allowin;
wire mem_to_wb_valid;
reg mem_valid;
reg [31:0] mem_pc;
reg        mem_rf_we;
reg [ 4:0] mem_dest;
reg        mem_res_from_mem;
reg [31:0] mem_alu_result;

//WBreg
wire wb_ready_go;
wire wb_allowin;
wire wb_validout;
wire allowout;
reg wb_valid;
reg [31:0] wb_pc;
reg        wb_rf_we;
reg [ 4:0] wb_dest;
reg [31:0] wb_final_result;

wire rj_read_need;
wire rk_read_need;
wire rd_read_need;

wire [4:0] exe_target;  
wire [4:0] mem_target;
wire [4:0] wb_target;

wire exe_eq_target;
wire mem_eq_target;
wire wb_eq_target;

always @(posedge clk) begin
    if(!resetn) begin
        if_valid <= 1'b0; 
    end
    else if(if_allowin)begin
        if_valid <= validin;
    end
    else if(br_taken_cancel)begin
        if_valid <= 1'b0;
    end
    
    if (!resetn) begin
        if_pc <= 32'h1Bfffffc;
    end
    else if(validin && if_allowin)  begin
        if_pc <= nextpc;
    end
end

always @(posedge clk) begin
    if(!resetn) begin
        id_valid <= 1'b0;
    end
    else if(br_taken_cancel && id_valid)begin
        id_valid <= 1'b0;
    end
    else if(id_allowin)begin
        id_valid <= if_to_id_valid;
    end
    

    if(if_to_id_valid && id_allowin)begin
        inst <= inst_sram_rdata;
        id_pc <= if_pc;
    end
end

always @(posedge clk) begin
    if(!resetn) begin
        exe_valid <= 1'b0;
        exe_op <=4'b0000;
    end
    else if(exe_allowin)begin
        exe_valid <= id_to_exe_valid;
    end

    if(id_to_exe_valid && exe_allowin)begin
        exe_pc <= id_pc;
        exe_alu_op <= alu_op;
        exe_dest <= dest;
        exe_rf_we <= gr_we;
        exe_src1 <= alu_src1;
        exe_src2 <= alu_src2;
        exe_data_sram_en <= res_from_mem || mem_we;
        exe_data_sram_we <= {4{mem_we}};
        exe_data_sram_wdata <= rkd_value;
        exe_res_from_mem <= res_from_mem;
        exe_op <= alu_op2 ;
        exe_divisor_valid <= alu_op2[0] && ~alu_op2[1] && ~alu_op2[2];
        exe_dividend_valid <= alu_op2[0] && ~alu_op2[1] && ~alu_op2[2];
        exe_divisoru_valid  <= alu_op2[0] && ~alu_op2[1] && alu_op2[2];
        exe_dividendu_valid  <= alu_op2[0] && ~alu_op2[1] && alu_op2[2];
        
        
    end
    else if((dividend_valid && dividend_ready) || (divisor_valid && divisor_ready) )begin
         exe_divisor_valid <= !(divisor_valid && divisor_ready);
         exe_dividend_valid <= !(dividend_valid && dividend_ready);
    end
    else if((dividendu_valid && dividendu_ready) || (divisoru_valid && divisoru_ready))begin
         exe_divisoru_valid  <= !(divisoru_valid && divisoru_ready);
         exe_dividendu_valid  <= !(dividendu_valid && dividendu_ready);
    end
end

always @(posedge clk) begin
    if(!resetn) begin
        mem_valid <= 1'b0;
    end
    else if(mem_allowin)begin
        mem_valid <= exe_to_mem_valid;
    end

    if(exe_to_mem_valid && mem_allowin)begin
        mem_pc <= exe_pc;
        mem_dest <= exe_dest;
        mem_res_from_mem <= exe_res_from_mem;
        mem_rf_we <= exe_rf_we;
        mem_alu_result <= result;
    end
end

always @(posedge clk) begin
    if(!resetn) begin
        wb_valid <= 1'b0;
    end
    else if(wb_allowin)begin
        wb_valid <= mem_to_wb_valid;
    end

    if(mem_to_wb_valid && wb_allowin)begin
        wb_pc <= mem_pc;
        wb_dest <= mem_dest;
        wb_final_result <= final_result;
        wb_rf_we <= mem_rf_we;
    end
end

assign br_taken_cancel = id_ready_go && id_valid && br_taken ;

assign validin =1'b1;
assign if_ready_go = 1'b1;
assign if_allowin = !if_valid || if_ready_go && id_allowin;
assign if_to_id_valid = if_valid && if_ready_go;

assign id_ready_go = !(exe_eq_target && exe_res_from_mem && id_valid);
assign id_allowin = !id_valid || id_ready_go && exe_allowin;
assign id_to_exe_valid = id_valid & id_ready_go;

assign exe_ready_go = (exe_op[0]& !exe_op[1] & exe_op[2])? douu_valid :
                      (exe_op[0]& !exe_op[1] & !exe_op[2])? dou_valid :
                                                            1'b1  ;
assign exe_allowin = !exe_valid || exe_ready_go && mem_allowin; 
assign exe_to_mem_valid = exe_valid && exe_ready_go;

assign mem_ready_go = 1'b1;
assign mem_allowin = !mem_valid || mem_ready_go && wb_allowin;
assign mem_to_wb_valid = mem_valid && mem_ready_go;

assign wb_ready_go = 1'b1;
assign allowout = 1'b1;
assign wb_allowin = !wb_valid || wb_ready_go && allowout;
assign wb_validout = wb_valid && wb_ready_go;

//nextPC
assign seq_pc   = if_pc + 3'h4;                 
assign nextpc   = br_taken_cancel ? br_target : seq_pc;   //如果br_taken为1，跳转到目标pc，否则+4

assign inst_sram_en    = resetn && id_ready_go && exe_ready_go;
assign inst_sram_we    = 4'b0;
assign inst_sram_addr  = nextpc;
assign inst_sram_wdata = 32'b0;

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
assign inst_sll_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0e];
assign inst_srl_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0f];
assign inst_sra_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h10];
assign inst_mul_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h18];
assign inst_mulh_w = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h19];
assign inst_mulh_wu= op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h1a];
assign inst_div_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h00];
assign inst_div_wu = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h02];
assign inst_mod_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h01];
assign inst_mod_wu = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h03];
assign inst_addi_w = op_31_26_d[6'h00] & op_25_22_d[4'ha];
assign inst_ld_w   = op_31_26_d[6'h0a] & op_25_22_d[4'h2];
assign inst_st_w   = op_31_26_d[6'h0a] & op_25_22_d[4'h6];
assign inst_slti   = op_31_26_d[6'h00] & op_25_22_d[4'h8];
assign inst_sltui  = op_31_26_d[6'h00] & op_25_22_d[4'h9];
assign inst_andi   = op_31_26_d[6'h00] & op_25_22_d[4'hd];
assign inst_ori    = op_31_26_d[6'h00] & op_25_22_d[4'he];
assign inst_xori   = op_31_26_d[6'h00] & op_25_22_d[4'hf];
assign inst_jirl   = op_31_26_d[6'h13];
assign inst_b      = op_31_26_d[6'h14];
assign inst_bl     = op_31_26_d[6'h15];
assign inst_beq    = op_31_26_d[6'h16];
assign inst_bne    = op_31_26_d[6'h17];
assign inst_lu12i_w= op_31_26_d[6'h05] & ~inst[25];
assign inst_pcaddu12i=op_31_26_d[6'h07] & ~inst[25];

assign alu_op[ 0] = inst_add_w | inst_addi_w | inst_ld_w | inst_st_w
                    | inst_jirl | inst_bl | inst_pcaddu12i;
assign alu_op[ 1] = inst_sub_w;
assign alu_op[ 2] = inst_slt | inst_slti;
assign alu_op[ 3] = inst_sltu | inst_sltui;
assign alu_op[ 4] = inst_and | inst_andi;
assign alu_op[ 5] = inst_nor;
assign alu_op[ 6] = inst_or | inst_ori;
assign alu_op[ 7] = inst_xor | inst_xori;
assign alu_op[ 8] = inst_slli_w | inst_sll_w;
assign alu_op[ 9] = inst_srli_w | inst_srl_w;
assign alu_op[10] = inst_srai_w | inst_sra_w;
assign alu_op[11] = inst_lu12i_w;
assign alu_op2[0] = inst_mul_w | inst_mulh_w | inst_mulh_wu | inst_mod_w | inst_mod_wu | inst_div_w | inst_div_wu;
assign alu_op2[1] = inst_mul_w | inst_mulh_w | inst_mulh_wu;  //结果是否为乘法器输出
assign alu_op2[2] = inst_mulh_wu | inst_div_wu | inst_mod_wu; //是否为无符号数
assign alu_op2[3] = inst_mulh_w | inst_mulh_wu | inst_div_wu | inst_div_w; //结果是否位于高位

assign need_ui5   =  inst_slli_w | inst_srli_w | inst_srai_w;
assign need_ui12  =  inst_andi | inst_ori | inst_xori;
assign need_si12  =  inst_addi_w | inst_ld_w | inst_st_w | inst_slti | inst_sltui ;
assign need_si16  =  inst_jirl | inst_beq | inst_bne;
assign need_si20  =  inst_lu12i_w | inst_pcaddu12i;
assign need_si26  =  inst_b | inst_bl;
assign src2_is_4  =  inst_jirl | inst_bl;

assign rj_read_need = inst_add_w | inst_sub_w | inst_slt | inst_sltu | inst_or | inst_and | inst_xor | inst_nor |
                       inst_slli_w | inst_srli_w | inst_srai_w | inst_beq | inst_bne | inst_jirl | inst_ld_w | inst_st_w | inst_addi_w |
                       inst_ori | inst_xori | inst_andi | inst_sltui | inst_slti | inst_mul_w | inst_mulh_w | inst_mulh_wu | 
                       inst_div_w | inst_mod_w | inst_div_wu | inst_mod_wu | inst_sll_w | inst_srl_w | inst_sra_w;
assign rd_read_need = inst_beq | inst_bne | inst_st_w;
assign rk_read_need = inst_add_w | inst_sub_w | inst_slt | inst_sltu | inst_or | inst_and | inst_xor | inst_nor | 
                      inst_jirl | inst_mul_w | inst_mulh_w | inst_mulh_wu | inst_div_w | inst_mod_w | inst_div_wu | inst_mod_wu
                      | inst_sll_w | inst_srl_w | inst_sra_w;
                      
assign exe_target ={5{exe_rf_we}} & exe_dest;
assign mem_target ={5{mem_rf_we}} & mem_dest;
assign wb_target ={5{wb_rf_we}} & wb_dest;
 
assign exe_eq_target = (!exe_valid)? 1'b0:                          
                      (exe_target==5'b00000)? 1'b0:                                                             
                      (rj_read_need & (rj==exe_target))| (rk_read_need & (rk==exe_target))| (rd_read_need & (rd==exe_target)) ;

assign mem_eq_target = (!mem_valid)? 1'b0:                                 
                      (mem_target==5'b00000)? 1'b0:                       
                      (rj_read_need & (rj==mem_target)) | (rk_read_need & (rk==mem_target)) | (rd_read_need & (rd==mem_target)) ;                                                       

assign wb_eq_target = (!wb_valid)? 1'b0:                                                                   
                      (wb_target==5'b00000)? 1'b0:                                           
                      (rj_read_need & (rj==wb_target))| (rk_read_need & (rk==wb_target)) | (rd_read_need & (rd==wb_target)) ;                    
                      
assign imm = src2_is_4 ?  32'h4                     :
             need_si20 ? {i20[19:0], 12'b0}         :
             need_ui12 ? {{20'b0},i12[11:0]}        :
                          {{20{i12[11]}}, i12[11:0]};/*need_ui5 || need_si12*/

assign br_offs = need_si26 ? {{ 4{i26[25]}}, i26[25:0], 2'b0} :
                             {{14{i16[15]}}, i16[15:0], 2'b0} ;

assign jirl_offs = {{14{i16[15]}}, i16[15:0], 2'b0};

assign src_reg_is_rd = inst_beq | inst_bne | inst_st_w;

assign src1_is_pc    = inst_jirl | inst_bl | inst_pcaddu12i;

assign src2_is_imm   = inst_slli_w |
                       inst_srli_w |
                       inst_srai_w |
                       inst_addi_w |
                       inst_slti   |
                       inst_sltui  |
                       inst_andi   |
                       inst_ori    |
                       inst_xori   |
                       inst_pcaddu12i|
                       inst_ld_w   |
                       inst_st_w   |
                       inst_lu12i_w|
                       inst_jirl   |
                       inst_bl     ;

assign res_from_mem  = inst_ld_w;
assign dst_is_r1     = inst_bl;

assign gr_we         = ~inst_st_w & ~inst_beq & ~inst_bne & ~inst_b;
assign mem_we        = inst_st_w;
assign dest          = dst_is_r1 ? 5'd1 : rd;

assign rf_raddr1 = rj;
assign rf_raddr2 = src_reg_is_rd ? rd :rk;
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

assign rj_value  =(exe_eq_target && rj_read_need && ~exe_res_from_mem && (rj==exe_target))? result :
                  (mem_eq_target && rj_read_need && (rj==mem_target))? final_result  :
                  (wb_eq_target && rj_read_need && (rj==wb_target))? wb_final_result :rf_rdata1;
assign rkd_value =(exe_eq_target && ~exe_res_from_mem && ((rk_read_need && (rk==exe_target)) || (rd_read_need && (rd==exe_target))))? result :
                  (mem_eq_target && ((rk_read_need && (rk==mem_target)) || (rd_read_need && (rd==mem_target))))? final_result  :
                  (wb_eq_target && ((rk_read_need && (rk==wb_target)) || (rd_read_need && (rd==wb_target))))? wb_final_result :rf_rdata2;
assign rj_eq_rd = (rj_value == rkd_value);

assign br_taken =  inst_beq  &&  rj_eq_rd
                   || inst_bne  && !rj_eq_rd
                   || inst_jirl
                   || inst_bl
                   || inst_b;            

assign br_target = (inst_beq || inst_bne || inst_bl || inst_b) ? (id_pc + br_offs) :
                                                   /*inst_jirl*/ (rj_value + jirl_offs);

assign alu_src1 = src1_is_pc  ? id_pc[31:0] : rj_value;
assign alu_src2 = src2_is_imm ? imm : rkd_value;

assign mul_src1 = !exe_op[2] ? {exe_src1[31],exe_src1[31:0]} : {1'b0,exe_src1[31:0]} ;
assign mul_src2 = !exe_op[2] ? {exe_src2[31],exe_src2[31:0]} : {1'b0,exe_src2[31:0]} ;
assign mul_result = $signed(mul_src1) * $signed(mul_src2);

assign divisor_valid = exe_divisor_valid;
assign dividend_valid = exe_dividend_valid;
assign divisoru_valid = exe_divisoru_valid;
assign dividendu_valid = exe_dividendu_valid;

div_1 div (
  .aclk(clk),                                      // input wire aclk
  .s_axis_divisor_tvalid(divisor_valid),    // input wire s_axis_divisor_tvalid
  .s_axis_divisor_tready(divisor_ready),    // output wire s_axis_divisor_tready
  .s_axis_divisor_tdata(exe_src2),      // input wire [31 : 0] s_axis_divisor_tdata
  .s_axis_dividend_tvalid(dividend_valid),  // input wire s_axis_dividend_tvalid
  .s_axis_dividend_tready(dividend_ready),  // output wire s_axis_dividend_tready
  .s_axis_dividend_tdata(exe_src1),    // input wire [31 : 0] s_axis_dividend_tdata
  .m_axis_dout_tvalid(dou_valid),          // output wire m_axis_dout_tvalid
  .m_axis_dout_tdata(div_result)            // output wire [63 : 0] m_axis_dout_tdata
);

div_gen_0 divu (
  .aclk(clk),                                      // input wire aclk
  .s_axis_divisor_tvalid(divisoru_valid),    // input wire s_axis_divisor_tvalid
  .s_axis_divisor_tready(divisoru_ready),    // output wire s_axis_divisor_tready
  .s_axis_divisor_tdata(exe_src2),      // input wire [31 : 0] s_axis_divisor_tdata
  .s_axis_dividend_tvalid(dividendu_valid),  // input wire s_axis_dividend_tvalid
  .s_axis_dividend_tready(dividendu_ready),  // output wire s_axis_dividend_tready
  .s_axis_dividend_tdata(exe_src1),    // input wire [31 : 0] s_axis_dividend_tdata
  .m_axis_dout_tvalid(douu_valid),          // output wire m_axis_dout_tvalid
  .m_axis_dout_tdata(divu_result)            // output wire [63 : 0] m_axis_dout_tdata
);

assign div_mul_result =exe_op[1]? mul_result[63:0] :
                       exe_op[2]? divu_result[63:0] :
                                  div_result;


alu u_alu(
    .alu_op     (exe_alu_op    ),
    .alu_src1   (exe_src1  ),               
    .alu_src2   (exe_src2  ),               
    .alu_result (alu_result)                
    );
    
assign result= (exe_op[0] & exe_op[3])? div_mul_result[63:32] :
               (exe_op[0] & !exe_op[3])? div_mul_result[31:0] :
                                         alu_result;

assign data_sram_en    = exe_data_sram_en && exe_valid;
assign data_sram_we    = exe_data_sram_we;
assign data_sram_addr  = alu_result;
assign data_sram_wdata = exe_data_sram_wdata;

assign mem_result   = data_sram_rdata; 
assign final_result = mem_res_from_mem ? mem_result : mem_alu_result;                

assign rf_we    = wb_rf_we && wb_valid;
assign rf_waddr = wb_dest;
assign rf_wdata = wb_final_result;

// debug info generate
assign debug_wb_pc       = wb_pc;
assign debug_wb_rf_we   = {4{rf_we}};                   
assign debug_wb_rf_wnum  = wb_dest;
assign debug_wb_rf_wdata = rf_wdata;

endmodule