//注释中包含中文，如果乱码，则可能�?要使用GB编码打开,如果显示正常可以无视
//if the notes are wrong characters in Chinese，please turn 'UTF-8' to 'GB2312',
//else ignore this message and please save the file correctly
`include "csr_head.v"

module mycpu_core(
    input  wire        clk,
    input  wire        resetn,  
    // inst sram interface
    output wire        inst_sram_en,
    output wire [ 3:0] inst_sram_we,                        
    output wire [31:0] inst_sram_addr,
    output wire [31:0] inst_sram_wdata,
    input  wire [31:0] inst_sram_rdata,
    //add:
    output wire        inst_sram_req,  // 
    output wire        inst_sram_wr,  //
    output wire [ 1:0] inst_sram_size,//
    output wire [ 3:0] inst_sram_wstrb,//
    input              inst_sram_addr_ok,
    input              inst_sram_data_ok,
    // data sram interface
    output wire        data_sram_en,
    output wire [ 3:0] data_sram_we,
    output wire [31:0] data_sram_addr,
    output wire [31:0] data_sram_wdata,
    input  wire [31:0] data_sram_rdata,
    //add:
    output wire        data_sram_req,     
    output wire        data_sram_wr,      
    output wire [ 1:0] data_sram_size,     
    output wire [ 3:0] data_sram_wstrb,    
    input              data_sram_addr_ok,   
    input              data_sram_data_ok,
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

wire        rj_lt_rd;
wire        rj_ge_rd;
wire        rj_ge_rd_u;
wire        rj_lt_rd_u;
wire [11:0] alu_op;
wire        load_op;
wire        src1_is_pc;
wire        src2_is_imm;
wire        res_from_mem;
wire        dst_is_r1;
wire        gr_we;
wire        mem_we;
wire        src_reg_is_rd;
wire        br_stall;
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

wire [63:0] op_31_26_d;    
wire [15:0] op_25_22_d;
wire [ 3:0] op_21_20_d;
wire [31:0] op_19_15_d;
wire [31:0] op_14_10_d;
wire [31:0] op_9_5_d; 
wire [31:0] op_4_0_d;

wire        inst_add_w;    
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
//blt, bge, bltu, bgeu
wire        inst_blt;
wire        inst_bge;
wire        inst_bltu;
wire        inst_bgeu;
wire        br_wait;
//ld.b, ld.h, ld.bu, ld.hu
wire        inst_ld_b;
wire        inst_ld_h;
wire        inst_ld_bu;
wire        inst_ld_hu;
//st.b, st.h
wire        inst_st_b;
wire        inst_st_h;
//CSR
wire        inst_csrrd;
wire        inst_csrwd;
wire        inst_csrxchg;
//ERTN
wire        inst_ertn;
//syscall
wire        inst_syscall;
//break
wire        inst_break;
//rdcntvl_w inst_rdcntvh_w inst_rdcntid_w
wire        inst_rdcntvl_w;
wire        inst_rdcntvh_w;
wire        inst_rdcntid_w;
//tlbsrch tlbwr tlbfill tlbrd invtlb
wire        inst_tlbsrch;
wire        inst_tlbwr;
wire        inst_tlbfill;
wire        inst_tlbrd;
wire        inst_invtlb;

reg         exe_inst_tlbsrch;
reg         exe_inst_tlbwr;
reg         exe_inst_tlbfill;
reg         exe_inst_tlbrd;
reg         exe_inst_invtlb;

reg         mem_inst_tlbsrch;
reg         mem_inst_tlbwr;
reg         mem_inst_tlbfill;
reg         mem_inst_tlbrd;
reg         mem_inst_invtlb;

reg         wb_inst_tlbsrch;
reg         wb_inst_tlbwr;
reg         wb_inst_tlbfill;
reg         wb_inst_tlbrd;
reg         wb_inst_invtlb;

wire        res_from_counter;
reg [63:0]  counter;

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

wire        ADEF;
wire        ALE;
wire        INE;
wire        ertn_block;
wire        exe_refill;
wire        exe_PIL;
wire        exe_PIS;
wire        exe_PPI;
wire        exe_PME;
//to_fs signals
//used in  pre-IF stage
wire        to_if_valid;
wire        to_if_ready_go;
wire        pre_if_valid;
wire        pre_if_ready_go;
reg  [31:0] pre_if_pc;
//IFreg
wire        if_to_id_valid;
wire        if_ready_go;
wire        if_allowin;
wire        validin;
reg  [31:0] if_pc;
reg         if_valid;

//exp19 add
wire        pre_if_ex;
reg         if_ex;

reg         if_adef_ex;
reg         if_refill_ex;
reg         if_PPI_ex;
reg         if_PIF_ex;

wire        mapping_ADEF;



//IDreg
wire id_ready_go;
wire id_allowin;
wire id_to_exe_valid;
reg        id_valid;
reg [31:0] id_pc;
reg [31:0] inst;

reg id_adef_ex;
//exp19 add 
reg id_refill_ex;
reg id_PPI_ex;
reg id_PIF_ex;

wire sys_ex;
wire brk_ex;
wire id_ex;

reg if_to_id_ex;

//EXEreg
reg exe_mem_we;
wire exe_en;
wire exe_ready_go;
wire exe_allowin;
wire exe_to_mem_valid;
reg exe_valid;
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
reg [ 1:0] exe_counter;//exe_counter[1]
reg        exe_dividend_valid;
reg        exe_divisor_valid;
reg        exe_dividendu_valid;
reg        exe_divisoru_valid;
reg        exe_inst_ld_b;
reg        exe_inst_ld_bu;
reg        exe_inst_ld_h;
reg        exe_inst_ld_hu;
reg        exe_inst_ld_w;
reg        exe_inst_st_b;
reg        exe_inst_st_h;
reg        exe_inst_st_w;
reg        exe_inst_rdcntid_w;
reg [31:0] exe_rkd_value;
reg [31:0] exe_rj_value;
//CSR MEM
reg [13:0] exe_csr_num;
reg	       exe_csr_we;
reg	[31:0] exe_csr_wmask;
reg	[31:0] exe_csr_wvalue;
reg	       exe_ertn_flush;
reg        exe_inst_csr;

reg	       exe_sys_ex;
reg	       exe_brk_ex;
reg        exe_ine_ex;
reg        exe_adef_ex;
reg        exe_ale_ex;
reg        exe_int_ex;
//exp19add
reg        exe_refill_ex;
reg        exe_PPI_ex;
reg        exe_PIF_ex;
reg        id_to_exe_ex;


wire       exe_tlb_ex;
wire       mapping_ADEM;

wire       exe_ex;

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
reg        mem_inst_ld_b;
reg        mem_inst_ld_bu;
reg        mem_inst_ld_h;
reg        mem_inst_ld_hu;
reg        mem_inst_ld_w;
reg        mem_inst_rdcntid_w;
//CSR MEM
reg [13:0] mem_csr_num;
reg	       mem_csr_we;
reg	[31:0] mem_csr_wmask;
reg	[31:0] mem_csr_wvalue;
reg	       mem_ertn_flush;
reg        mem_inst_csr;

reg	[31:0] mem_vaddr;
reg	       mem_sys_ex;
reg	       mem_brk_ex;
reg        mem_ine_ex;
reg        mem_adef_ex;
reg        mem_ale_ex;
reg        mem_int_ex;
//exp19 add
reg        mem_adem_ex;
reg        mem_refill_ex;
reg        mem_PPI_ex;
reg        mem_PIF_ex;
reg        mem_PIL_ex;
reg        mem_PIS_ex;
reg        mem_PME_ex;
reg        mem_exe_tlb_ex;

wire       mem_ex;

wire    wb_tlbsrch_hit;
wire  [3:0]  wb_tlbsrch_hit_idx;
//reg        wb_tlbsrch_hit;//
//reg        wb_tlbsrch_hit_idx;

reg       mem_is_ldst;
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
//CSR WB
reg [13:0]  wb_csr_num;
reg	        wb_csr_we;
reg	[31:0]  wb_csr_wmask;
reg	[31:0]  wb_csr_wvalue;
reg	        wb_ertn_flush;
reg         wb_inst_csr;
reg         wb_inst_rdcntid_w;

wire	    wb_ex;
wire [5:0]	wb_ecode;
wire [8:0]	wb_esubcode;
wire        csr_wr_en;

reg	 [31:0] wb_vaddr;
reg	       wb_sys_ex;
reg	       wb_brk_ex;
reg        wb_ine_ex;
reg        wb_adef_ex;
reg        wb_ale_ex;
reg        wb_int_ex;
//exp19 add
reg        wb_adem_ex;
reg        wb_refill_ex;
reg        wb_PPI_ex;
reg        wb_PIF_ex;
reg        wb_PIL_ex;
reg        wb_PIS_ex;
reg        wb_PME_ex;
reg        wb_exe_tlb_ex;

wire [31:0] csr_rvalue;
wire [31:0] ex_entry;
wire 	    has_int;

wire rj_read_need;
wire rk_read_need;
wire rd_read_need;

wire [4:0] exe_target;  
wire [4:0] mem_target;
wire [4:0] wb_target;

wire exe_eq_target;
wire mem_eq_target;
wire fwd_mem;
wire wb_eq_target;

//CSR RAW
wire csr_exe_eq_target;                                 
wire csr_mem_eq_target;
wire csr_wb_eq_target;
wire block_ld_eq_target;
wire ex_sig;

//load and store
wire  [31:0] load_word;
wire  [ 1:0] last;
wire  bz,fz,lz,nz;
wire  [15:0] half_word;
wire  [7 :0] byte;
wire  [31:0] extended_half;
wire  [31:0] extended_byte;
wire  [31:0] st_data;
wire  [3 :0] st_b_strb;
wire  [3 :0] st_h_strb;
wire  [3 :0] st_w_strb;
wire  [3 :0] st_strb; 

//CSR ds
wire csr_eq_target;

wire         inst_csr;
wire         ertn_flush;
wire         csr_we;
wire  [31:0] csr_mask;
wire  [14:0] csr_num;
wire  [31:0] csr_wvalue;
wire  [31:0] ertn_pc;
//
reg          reg_exe_addr_ok;
wire         pre_if_addr_ok;
reg          if_addr_ok;
reg          id_inst_sram_addr_ok;
//inst_buffer
reg  [31:0] if_inst_buff;
reg         if_inst_valid;
wire [31:0] if_inst;
//cancel
wire pre_if_cancel;
reg if_ex_ertn_cancel;
wire if_to_pre_if_allowin;
wire exc_cancel;
//br control and int
reg reg_ertn_flush, reg_wb_ex, reg_br_taken;
reg [31:0] reg_ertn_pc, reg_ex_entry, reg_br_target;
reg exe_reg_ertn_flush,exe_reg_wb_ex;
reg mem_reg_ertn_flush,mem_reg_wb_ex;

reg br_cancel;
reg if_ex_or_ertn_bk;

//TLB signals
// search port 0 (for fetch)·
wire  [ 18:0] s0_vppn;
wire          s0_va_bit12;      
wire   [ 9:0] s0_asid;           
wire         s0_found;        
wire [3:0] s0_index;
wire [ 19:0] s0_ppn;         
wire  [ 5:0] s0_ps;          
wire  [ 1:0] s0_plv;           
wire  [ 1:0] s0_mat;
wire         s0_d;          
wire         s0_v;         
// search port 1 (for load/store)      
wire  [ 18:0] s1_vppn;
wire          s1_va_bit12;
wire   [ 9:0] s1_asid;
wire         s1_found;
wire [3:0] s1_index;
wire [ 19:0] s1_ppn;
wire  [ 5:0] s1_ps;
wire  [ 1:0] s1_plv;
wire  [ 1:0] s1_mat;
wire         s1_d;
wire         s1_v;
// invtlb opcode   //use for INVTLB
wire         invtlb_valid;
reg   [ 4:0] invtlb_op;
// write port //used for TLBWR and TLBFILL
wire         we; //w(rite) e(nable)
wire [3:0]   w_index;
wire         w_e;  
wire [ 18:0] w_vppn;
wire  [ 5:0] w_ps;
wire  [ 9:0] w_asid;
wire         w_g;
wire [ 19:0] w_ppn0;
wire  [ 1:0] w_plv0;
wire  [ 1:0] w_mat0;
wire         w_d0;
wire         w_v0;
wire [ 19:0] w_ppn1;
wire  [ 1:0] w_plv1;
wire  [ 1:0] w_mat1;
wire         w_d1;
wire         w_v1;
     // read port   //used for TLBRD
wire [3:0] r_index;
wire         r_e;
wire [ 18:0] r_vppn;
wire  [ 5:0] r_ps;
wire  [ 9:0] r_asid;
wire         r_g;
wire [ 19:0] r_ppn0;
wire  [ 1:0] r_plv0;
wire  [ 1:0] r_mat0;
wire         r_d0;
wire         r_v0;
wire [ 19:0] r_ppn1;
wire  [ 1:0] r_plv1;
wire  [ 1:0] r_mat1;
wire         r_d1;
wire         r_v1;


wire tlbsrch_valid;
wire tlbrd_valid;
wire tlbwr_valid;
wire tlbfill_valid;

wire inst_tlb;

wire tlbsrch_hit;
wire [3:0 ] tlbsrch_hit_idx;
//重取指标志与pc
wire id_refetch_flag;
reg exe_refetch_flag;
reg mem_refetch_flag;
reg wb_refetch_flag;
wire wb_refetch;
reg reg_wb_refetch;
reg [31:0] reg_refetch_pc;
wire [31:0] refetch_pc;

//vaddr_trans sigs
wire [31:0]  tlb_rentry; 

wire        da;
wire        pg;

wire [ 1:0] crmd_plv;
wire [ 2:0] dmw0_pseg;
wire [ 2:0] dmw0_vseg;
wire [ 3:0] dmw0_plv;
wire [ 2:0] dmw1_pseg;
wire [ 2:0] dmw1_vseg;
wire [ 3:0] dmw1_plv;

wire [31:0] f_paddr;
wire [18:0] f_vppn;
wire        f_va_bit12;
wire [ 9:0] f_asid;

wire        f_refill;
wire        f_PPI;
wire        f_PIF;

wire [31:0] s_paddr;
wire [18:0] s_vppn;
wire        s_va_bit12;
wire [ 9:0] s_asid;
wire [ 1:0] mem_type;
wire        s_refill;
wire        s_PIL;
wire        s_PIS;
wire        s_PPI;
wire        s_PME;



//wire is_ex_or_ertn;
//assign is_ex_or_ertn = exe_ex && mem_ex && wb_ex && mem_ertn_flush && wb_ertn_flush;
//reset
reg reset;
always @ (posedge clk)
    reset <= ~resetn;


//COUNTER
always @(posedge clk) begin
    if(!resetn) begin
        counter <= 64'b0; 
    end
    else begin
        counter <= counter + 1'b1;
    end
end


always @(posedge clk) begin
    if(!resetn) begin
        if_valid <= 1'b0; 
    end
    else if(if_allowin)begin
        if_valid <= to_if_valid;
    end
    else if(wb_ex || ertn_flush || wb_refetch)begin
        if_valid <= 1'b0;
    end
    else if(br_taken_cancel/* || ertn_block*/)begin
        if_valid <= 1'b0;
    end
    
    if (!resetn) begin
        if_pc <= 32'h1Bfffffc;
    end
    else if(pre_if_ready_go && if_allowin)  begin
        if_pc <= nextpc;
        if_adef_ex <= ADEF;
        if_refill_ex <= f_refill & ~ADEF;   //根据异常优先级 ADEF > TLB相关异常
        if_PIF_ex    <= f_PIF & ~ADEF;
        if_PPI_ex    <= f_PPI & ~ADEF;
        if_ex        <= pre_if_ex; 
    end
end

always @(posedge clk) begin
    if(!resetn) begin
        id_valid <= 1'b0;
    end
    else if(wb_ex || ertn_flush || wb_refetch)begin
        id_valid <= 1'b0;
    end
    else if(br_taken_cancel)begin
        id_valid <= 1'b0;
    end
    else if(id_allowin)begin
        id_valid <= if_to_id_valid;
    end
    

    if(if_ready_go && id_allowin)begin
        inst <= if_inst;//inst
        id_pc <= if_pc;//pc
        id_adef_ex <= if_adef_ex;
        id_refill_ex <= if_refill_ex;
        id_PIF_ex    <= if_PIF_ex;
        id_PPI_ex    <= if_PPI_ex;

        if_to_id_ex  <= if_ex;          //用于在ID级判断IF级是否发生异常
    end
end

always @(posedge clk) begin
    if(!resetn) begin
        exe_valid <= 1'b0;
        exe_op <=4'b0000;
        exe_inst_tlbsrch <= 1'b0;
    end
    else if((wb_ex || ertn_flush || wb_refetch) && wb_valid)begin
        exe_valid <= 1'b0;
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
        exe_data_sram_we <= {4{mem_we}};// & st_strb;
        
        exe_rj_value <= rj_value;
        exe_rkd_value <= rkd_value;
        //exe_data_sram_wdata <= st_data;
        exe_mem_we <= mem_we;
        exe_res_from_mem <= res_from_mem;
        exe_op <= alu_op2 ;
        exe_divisor_valid <= alu_op2[0] && ~alu_op2[1] && ~alu_op2[2];
        exe_dividend_valid <= alu_op2[0] && ~alu_op2[1] && ~alu_op2[2];
        exe_divisoru_valid  <= alu_op2[0] && ~alu_op2[1] && alu_op2[2];
        exe_dividendu_valid  <= alu_op2[0] && ~alu_op2[1] && alu_op2[2];
        exe_inst_ld_b <= inst_ld_b;
        exe_inst_ld_bu <= inst_ld_bu;
        exe_inst_ld_h <= inst_ld_h;
        exe_inst_ld_hu <= inst_ld_hu;
        exe_inst_ld_w <= inst_ld_w;
        exe_inst_st_b <= inst_st_b;
        exe_inst_st_w <= inst_st_w;
        exe_inst_st_h <= inst_st_h;
        exe_inst_rdcntid_w <= inst_rdcntid_w;

        id_to_exe_ex  <= if_to_id_ex;       //用于在EXE级判断ID级是否发生异常       
        exe_sys_ex <= sys_ex;
        exe_brk_ex <= brk_ex;
        exe_ine_ex <= INE;
        exe_int_ex <= has_int;
        exe_adef_ex <= id_adef_ex;
        exe_ertn_flush <= inst_ertn;

        exe_refill_ex <= id_refill_ex;
        exe_PIF_ex    <= id_PIF_ex;
        exe_PPI_ex    <= id_PPI_ex;

        exe_counter <= {res_from_counter,inst_rdcntvh_w};
        
        exe_csr_we <= csr_we;
        exe_csr_num <= csr_num;
        exe_csr_wmask <= csr_mask;
        exe_csr_wvalue <= csr_wvalue;
        exe_inst_csr <= inst_csr;

        exe_inst_tlbsrch <= inst_tlbsrch;
        exe_inst_tlbwr <= inst_tlbwr;
        exe_inst_tlbfill <= inst_tlbfill;
        exe_inst_tlbrd <= inst_tlbrd;
        exe_inst_invtlb <= inst_invtlb;
        invtlb_op <= id_invtlb_op;

        exe_refetch_flag <= id_refetch_flag;
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
    else if(wb_valid && (wb_ex || ertn_flush || wb_refetch))begin
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
        mem_alu_result <= result;  //counter
        mem_inst_ld_b <= exe_inst_ld_b;
        mem_inst_ld_bu <= exe_inst_ld_bu;
        mem_inst_ld_h <= exe_inst_ld_h;
        mem_inst_ld_hu <= exe_inst_ld_hu;
        mem_inst_ld_w <= exe_inst_ld_w;

        mem_is_ldst <= (!ld_cancel && exe_res_from_mem || !st_cancel && exe_mem_we);
        mem_ld_cancel <= ld_cancel;
        mem_st_cancel <= st_cancel;

        mem_csr_num <= exe_csr_num;
        mem_csr_we <= exe_csr_we;
        mem_csr_wmask <= exe_csr_wmask;
        mem_csr_wvalue <= exe_csr_wvalue;
        mem_ertn_flush <= exe_ertn_flush;
        mem_inst_csr <= exe_inst_csr;
        mem_inst_rdcntid_w <= exe_inst_rdcntid_w;
        
        mem_vaddr <= alu_result;
        mem_sys_ex <= exe_sys_ex;
        mem_brk_ex <= exe_brk_ex;
        mem_ale_ex <= ALE;
        mem_ine_ex <= exe_ine_ex;
        mem_int_ex <= exe_int_ex;
        mem_adef_ex <= exe_adef_ex;

        mem_adem_ex     <= ADEM;
        mem_refill_ex <= exe_refill_ex | exe_refill;    //该处refill包含取指或访存的refill，采用
        mem_PIF_ex    <= exe_PIF_ex;
        mem_PPI_ex    <= exe_PPI_ex | exe_PPI;
        mem_PIL_ex    <= exe_PIL;
        mem_PIS_ex    <= exe_PIS;
        mem_PME_ex    <=  exe_PME;
        mem_exe_tlb_ex <= exe_tlb_ex;
        

        mem_inst_tlbsrch <= exe_inst_tlbsrch;
        mem_inst_tlbwr <= exe_inst_tlbwr;
        mem_inst_tlbfill <= exe_inst_tlbfill;
        mem_inst_tlbrd <= exe_inst_tlbrd;
        mem_inst_invtlb <= exe_inst_invtlb;

        mem_refetch_flag <= exe_refetch_flag;
    end
end

always @(posedge clk) begin
    if(!resetn) begin
        wb_valid <= 1'b0;
    end
    else if(wb_ex | ertn_flush)begin
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

        wb_csr_num <= mem_csr_num;
        wb_csr_we  <= mem_csr_we;
        wb_csr_wmask <= mem_csr_wmask;
        wb_csr_wvalue <= mem_csr_wvalue;
        wb_ertn_flush <= mem_ertn_flush;
        wb_inst_csr <= mem_inst_csr;
        wb_inst_rdcntid_w <= mem_inst_rdcntid_w;

        wb_vaddr <= mem_vaddr;
        wb_sys_ex <= mem_sys_ex;
        wb_brk_ex <= mem_brk_ex;
        wb_ale_ex <= mem_ale_ex;
        wb_ine_ex <= mem_ine_ex;
        wb_int_ex <= mem_int_ex;
        wb_adef_ex <= mem_adef_ex;

        wb_adem_ex     <= mem_adem_ex;
        wb_refill_ex <= mem_refill_ex;
        wb_PIF_ex    <= mem_PIF_ex;
        wb_PPI_ex    <= mem_PPI_ex;
        wb_PIL_ex    <= mem_PIL_ex;
        wb_PIS_ex    <= mem_PIS_ex;
        wb_PME_ex    <= mem_PME_ex;
        wb_exe_tlb_ex <= mem_exe_tlb_ex;

        wb_inst_tlbsrch <= mem_inst_tlbsrch;
        wb_inst_tlbwr <= mem_inst_tlbwr;
        wb_inst_tlbfill <= mem_inst_tlbfill;
        wb_inst_tlbrd <= mem_inst_tlbrd;
        wb_inst_invtlb <= mem_inst_invtlb;

        wb_refetch_flag <= mem_refetch_flag;

        //wb_tlbsrch_hit <= s1_found;
        //wb_tlbsrch_hit_idx <= s1_index;
    end
end

//ld在EXE阶段产生RAW时，由于指令数据等待的延迟，需要通过MEM阶段是否在等待data_ok来判断堵塞 
wire mem_ld_waiting;
assign mem_ld_waiting = mem_valid & mem_res_from_mem & ~data_sram_data_ok;

//control
reg mem_st_cancel;
reg mem_ld_cancel;
//pre_if_control
assign pre_if_valid = resetn;
assign to_if_valid = (pre_if_valid && pre_if_ready_go);
assign pre_if_ready_go = (inst_sram_addr_ok && inst_sram_req);
//if_control
assign if_ready_go = ((inst_sram_data_ok) || if_inst_valid) && !br_cancel && !if_ex_ertn_cancel;
assign if_allowin = !if_valid || if_ready_go && id_allowin;
assign if_to_id_valid = if_valid && if_ready_go;
//id_control
assign id_ready_go = (!(block_ld_eq_target && id_valid || csr_eq_target && id_valid || tlbsrch_blk && id_valid)) & exe_allowin;                
assign id_allowin = (!id_valid || id_ready_go && exe_allowin);
assign id_to_exe_valid = id_valid & id_ready_go;
//exe_control
assign exe_ready_go = (!exe_data_sram_en) ?
                     ((exe_op[0]& !exe_op[1] & exe_op[2] & ~ex_sig)? douu_valid :                   
                      (exe_op[0]& !exe_op[1] & !exe_op[2] & ~ex_sig)? dou_valid :
                       1'b1) : data_sram_req ? data_sram_addr_ok : 1'b1;
                
assign exe_allowin = !exe_valid || exe_ready_go && mem_allowin; 
assign exe_to_mem_valid = exe_valid && exe_ready_go;
//mem_control
assign mem_ready_go = (mem_is_ldst && ~mem_ex) ? data_sram_data_ok: 1'b1;
assign mem_allowin = !mem_valid || mem_ready_go && wb_allowin;
assign mem_to_wb_valid = mem_valid && mem_ready_go;
//wb_control
assign wb_ready_go = 1'b1;
assign allowout = 1'b1;
assign wb_allowin = !wb_valid || wb_ready_go && allowout;
assign wb_validout = wb_valid && wb_ready_go;


//inst_sram
assign inst_sram_req   = resetn && if_allowin && !br_stall;
assign inst_sram_we    = 4'b0;
assign inst_sram_addr  = f_paddr;//nextpc;
assign inst_sram_wdata = 32'b0;
assign inst_sram_wr    = 1'b0;//no write
assign inst_sram_size  = 2'b10;//always 4 bytes
assign inst_sram_wstrb = 4'b0;//no write 
//exe_rf_we
//data_SRAM
assign exe_en          = (~exe_ex & ~mem_ex & ~ex_sig) && exe_valid;
assign data_sram_en    = exe_en && exe_data_sram_en;
assign data_sram_addr  = s_paddr;
assign data_sram_wdata = st_data;// from rkd_value
assign data_sram_wstrb = exe_data_sram_we & st_strb;
assign data_sram_wr    = exe_mem_we && !st_cancel;

assign data_sram_req   = resetn && data_sram_en && mem_allowin && !ld_cancel && !st_cancel;
assign data_sram_size  = exe_inst_st_b || exe_inst_ld_bu || exe_inst_ld_b? 2'b00 ://use es
                         exe_inst_st_h || exe_inst_ld_hu || exe_inst_ld_h? 2'b01 : 2'b10;

assign ex_sig = wb_ex | mem_ex | exe_ex;                 

//exp18 added
//如果 TLBSRCH 指令在 EX 级的时，有一条修改 CSR.ASID 或CSR.TLBEHI 
//的CSR 指令或者TLBRD 指令恰好在 MEM 级，那么直接读 CSR 模块的 ASID
//TLBEHI 的就会出现问题
wire tlbsrch_blk;

assign tlbsrch_blk = exe_inst_tlbsrch & (mem_csr_num == `CSR_ASID || mem_csr_num == `CSR_TLBEHI || mem_inst_tlbrd);

assign csr_exe_eq_target = exe_eq_target & exe_inst_csr & ~ex_sig ;
assign csr_mem_eq_target = mem_eq_target & mem_inst_csr & ~ex_sig;
assign csr_wb_eq_target  =  wb_eq_target &  wb_inst_csr & ~ex_sig;
assign csr_eq_target = csr_exe_eq_target | csr_mem_eq_target | csr_wb_eq_target;
//exp15 changed
//ld在EXE阶段产生RAW时，由于指令数据等待的延迟，需要通过MEM阶段是否在等待data_ok来判断堵塞          
assign block_ld_eq_target = exe_eq_target & exe_res_from_mem & ~ex_sig | mem_ld_waiting;

assign br_taken_cancel = id_ready_go && id_valid && br_taken;
//pre-IF
//branch control
always @(posedge clk) begin
    if(!resetn) begin
        reg_br_taken <= 1'b0;
        reg_br_target <= 32'b0;
    end else if (pre_if_ready_go && if_allowin)begin
        reg_br_taken <= 1'b0;
        reg_br_target <= 32'b0;
    end else if (br_taken && !br_stall) begin
        reg_br_taken <= 1'b1;
        reg_br_target <= br_target;
    end
end
//int and exc.

reg         reg_refill_ex;
reg  [31:0] reg_tlb_rentry;

always @(posedge clk) begin
    if(!resetn) begin
        reg_wb_ex <= 1'b0;
        reg_ertn_flush <= 1'b0;
        reg_ex_entry <= 32'b0;
        reg_ertn_pc <= 32'b0;
        reg_wb_refetch <= 1'b0;
        reg_refetch_pc <= 32'b0;
        reg_refill_ex <= 1'b0;
        reg_tlb_rentry <= 32'b0;
    end
    else if (pre_if_ready_go && if_allowin)begin
        reg_wb_ex <= 1'b0;
        reg_ertn_flush <= 1'b0;
        reg_ex_entry <= 32'b0;
        reg_ertn_pc <= 32'b0;
        reg_wb_refetch <= 1'b0;
        reg_refetch_pc <= 32'b0;
        reg_refill_ex <= 1'b0;
        reg_tlb_rentry <= 32'b0;
    end  else if (wb_ex || ertn_flush || wb_refetch) begin
        if (wb_ex & wb_refill_ex)begin
            reg_refill_ex <= 1'b1;
            reg_tlb_rentry <= tlb_rentry;
        end
        if (wb_ex) begin
            reg_wb_ex <= 1'b1;
            reg_ex_entry <= ex_entry;
        end
        if (ertn_flush) begin
            reg_ertn_flush <= 1'b1;
            reg_ertn_pc <= ertn_pc;
        end
        if (wb_refetch) begin   //add in exp 18
            reg_wb_refetch <= 1'b1;
            reg_refetch_pc <= refetch_pc;
        end
    end
    
end

wire wb_refill;
assign wb_refill = wb_ex & wb_refill_ex; 


//nextPC
assign seq_pc   = if_pc + 3'h4;                 
assign nextpc   = 
                  wb_refetch      ? refetch_pc   : //added                           
                  reg_wb_refetch  ? reg_refetch_pc  : //added
                  ertn_flush      ? ertn_pc      :                            
                  reg_ertn_flush  ? reg_ertn_pc  :
                  wb_ex & wb_refill_ex    ? tlb_rentry:
                  reg_refill_ex   ? reg_tlb_rentry:
                  wb_ex           ? ex_entry     :
                  reg_wb_ex       ? reg_ex_entry :
                  reg_br_taken    ? reg_br_target:
                  (br_taken && !br_stall) ? br_target    : 
                  seq_pc;

//TLB所需虚地址信息
assign s0_asid = f_asid;
assign s0_vppn = f_vppn;
assign s0_va_bit12 = f_va_bit12;

//fetch
vaddr_trans v0(
    .vaddr(nextpc),
    .paddr(f_paddr),

    .asid(w_asid),
    .s_vppn(f_vppn),           
    .s_va_bit12(f_va_bit12),
    .s_asid(f_asid),

    .da(da),
    .pg(pg),

    .crmd_plv(crmd_plv),
    .dmw0_vseg(dmw0_vseg),
    .dmw0_pseg(dmw0_pseg),
    .dmw0_plv(dmw0_plv),
    .dmw1_vseg(dmw1_vseg),
    .dmw1_pseg(dmw1_pseg),
    .dmw1_plv(dmw1_plv),

    .s_found(s0_found),          
    .s_ppn(s0_ppn),            
    .s_ps(s0_ps),
    .s_plv(s0_plv),
    //.s_mat,
    .s_d(s0_d),
    .s_v(s0_v),

    .mem_type(2'b10),

    .mapping_ADEF(mapping_ADEF),
    .mapping_ADEM(),        

    .tlb_refill(f_refill),
    .tlb_PIL(),
    .tlb_PIS(),
    .tlb_PIF(f_PIF),
    .tlb_PME(),     
    .tlb_PPI(f_PPI)
);
   
                  
//ld or st cancel 
//used in exe_stage.
wire ld_cancel;
wire st_cancel;
assign ld_cancel = mem_valid && (mem_ex || mem_ertn_flush) ||
                   wb_valid  && (wb_ex  || wb_ertn_flush ) ||
                   (exe_inst_ld_h || exe_inst_ld_hu || exe_inst_ld_w) && ALE;
assign st_cancel = mem_valid && (mem_ex || mem_ertn_flush) || 
                   wb_valid  && (wb_ex  || wb_ertn_flush ) ||
                   (exe_inst_st_h || exe_inst_st_w) && ALE;


//IF_stage
//IF:when ready_go = 1 and allowin = 0,
//inst cannot go into ID_stage.
//signal "inst" is id_inst //
//when id_allowin gets to 1, buffer should be empty
//its previous value should be pass to inst
//also, when wb_ex or ertn_flush is 1'b1, cancel.
//debugging

//exp15 added
//to judge the br_cancel first or the ex_ertn_cancelfirst
//判断分支取消优先还是异常取消优先
//主要针对分支取消时发生了时钟中断的情况
reg br_cancel_first;
reg ex_ertn_cancel_first;

always @ (posedge clk) begin
    if(!resetn) begin
        br_cancel_first <= 1'b0;
    end 
    else if(br_cancel && !ex_ertn_cancel_first)begin
        br_cancel_first <= 1'b1;
    end
    else if(!br_cancel && br_cancel_first)begin
        br_cancel_first <= 1'b0;
    end

    if(!resetn) begin
        ex_ertn_cancel_first <= 1'b0;
    end 
    else if(if_ex_ertn_cancel && !br_cancel_first)begin
        ex_ertn_cancel_first <= 1'b1;
    end
    else if(!if_ex_ertn_cancel && ex_ertn_cancel_first)begin
        ex_ertn_cancel_first <= 1'b0;
    end
end
always @ (posedge clk) begin
    if(!resetn) begin
        if_inst_buff <= 32'b0;
        if_inst_valid <= 1'b0;
    end else if (id_allowin || wb_ex || ertn_flush || wb_refetch) begin
        if_inst_buff <= 32'b0;
        if_inst_valid <= 1'b0;
    end else if(inst_sram_data_ok && !id_allowin && !if_inst_valid && !if_ex_ertn_cancel && !br_cancel) begin
        if_inst_buff <= inst_sram_rdata;
        if_inst_valid <= 1'b1;
    end 
end
assign if_inst = if_inst_valid ? if_inst_buff : inst_sram_rdata;//inst selection
//the inst should be canceled if :
always @ (posedge clk) begin
    if(!resetn) begin
        if_ex_ertn_cancel <= 1'b0;
    end else if((if_valid && !if_ready_go /*|| to_if_valid*/) && (wb_ex || ertn_flush || wb_refetch)) begin
        if_ex_ertn_cancel <= 1'b1;
    end else if(inst_sram_data_ok && !br_cancel_first) begin
        if_ex_ertn_cancel <= 1'b0;
    end
end
always @ (posedge clk) begin
    if(!resetn) begin
        br_cancel <= 1'b0;
    end else if(!if_allowin && !if_ready_go && (br_taken && !br_stall)) begin
        br_cancel <= 1'b1;
    end else if(inst_sram_data_ok && !ex_ertn_cancel_first) begin
        br_cancel <= 1'b0;
    end
    
end


//ID_stage

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
decoder_5_32 u_dec4(.in(rk),.out(op_14_10_d));                              
decoder_5_32 u_dec5(.in(rj),.out(op_9_5_d));
decoder_5_32 u_dec6(.in(rd),.out(op_4_0_d));

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

assign inst_blt    = op_31_26_d[6'h18];
assign inst_bge    = op_31_26_d[6'h19];
assign inst_bltu   = op_31_26_d[6'h1a];
assign inst_bgeu   = op_31_26_d[6'h1b];
//load
assign inst_ld_w   = op_31_26_d[6'h0a] & op_25_22_d[4'h2];
assign inst_ld_h   = op_31_26_d[6'h0a] & op_25_22_d[4'h1];
assign inst_ld_b   = op_31_26_d[6'h0a] & op_25_22_d[4'h0];
//load(u)
assign inst_ld_hu  = op_31_26_d[6'h0a] & op_25_22_d[4'h9];
assign inst_ld_bu  = op_31_26_d[6'h0a] & op_25_22_d[4'h8];
//store
assign inst_st_w   = op_31_26_d[6'h0a] & op_25_22_d[4'h6];
assign inst_st_h   = op_31_26_d[6'h0a] & op_25_22_d[4'h5];
assign inst_st_b   = op_31_26_d[6'h0a] & op_25_22_d[4'h4];
//CSR
assign inst_csrrd  = op_31_26_d[6'h01] & !inst[25:24] & op_9_5_d[5'h00];
assign inst_csrwd  = op_31_26_d[6'h01] & !inst[25:24] & op_9_5_d[5'h01];
assign inst_csrxchg = op_31_26_d[6'h01] & !inst[25:24] & ~op_9_5_d[5'h00] & ~op_9_5_d[5'h01];
//SYS
assign inst_syscall = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h16];
//BRK
assign inst_break = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h14];
//ERTN
assign inst_ertn = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0]
                    & op_19_15_d[5'h10] & op_14_10_d[5'h0e] & op_9_5_d[5'h0]
                    & op_4_0_d [5'h0];
//rdcntvl.w,rdcntvh.w rdcntid
assign inst_rdcntvl_w = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h0] & op_19_15_d[5'h00] & op_14_10_d[5'h18] & op_9_5_d[5'h0];
assign inst_rdcntvh_w = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h0] & op_19_15_d[5'h00] & op_14_10_d[5'h19] & op_9_5_d[5'h0]; 
assign inst_rdcntid_w = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h0] & op_19_15_d[5'h00] & op_14_10_d[5'h18] & op_4_0_d[5'h0];
//tlb
assign inst_tlbsrch = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h10] & op_14_10_d[5'h0a];
assign inst_tlbrd   = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h10] & op_14_10_d[5'h0b];
assign inst_tlbwr   = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h10] & op_14_10_d[5'h0c];
assign inst_tlbfill = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h10] & op_14_10_d[5'h0d];
assign inst_invtlb  = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h13];

//加入优先级判断
assign INE= (inst_invtlb && id_invtlb_op > 5'd6) || ~(inst_add_w | inst_sub_w | inst_slt | inst_sltu | inst_nor | inst_and | inst_or | inst_xor | inst_slli_w | 
            inst_srli_w | inst_srai_w | inst_sll_w | inst_srl_w | inst_sra_w | inst_mul_w | inst_mulh_w | inst_mulh_wu | 
            inst_div_w | inst_div_wu | inst_mod_w | inst_mod_wu | inst_addi_w |inst_slti | inst_sltui | inst_andi | 
            inst_ori | inst_xori | inst_jirl | inst_b | inst_bl | inst_beq | inst_bne | inst_lu12i_w  | inst_pcaddu12i | 
            inst_blt | inst_bge | inst_bltu | inst_bgeu | inst_ld_w | inst_ld_h | inst_ld_b | inst_ld_hu | inst_ld_bu
            | inst_st_w | inst_st_h | inst_st_b | inst_csrrd | inst_csrwd  | inst_csrxchg | inst_syscall | inst_break |
            inst_ertn | inst_rdcntvl_w | inst_rdcntvh_w | inst_rdcntid_w | inst_tlbsrch | inst_tlbrd | inst_tlbwr | inst_tlbfill |
            inst_invtlb) & ~ if_to_id_ex;   
             
assign ADEF = ((nextpc[1:0]!=2'b00) & inst_sram_req) || mapping_ADEF;
assign ADEM = mapping_ADEM & exe_data_sram_en & ~id_to_exe_ex;
assign ALE  = ~id_to_exe_ex & ~ADEM & ((exe_inst_ld_h | exe_inst_ld_hu | exe_inst_st_h) & alu_result[0] 
            ||(exe_inst_ld_w | exe_inst_st_w) &(alu_result[1:0]!=2'b00)) ;

assign ertn_block = (!exe_ready_go)?1'b0:
                     wb_ertn_flush? 1'b0 :(exe_ertn_flush | mem_ertn_flush |inst_ertn);


assign inst_csr = inst_csrrd | inst_csrwd | inst_csrxchg | inst_rdcntid_w;               
assign csr_we = inst_csrwd | inst_csrxchg;
assign csr_num = inst_rdcntid_w? 14'h0040:inst[23:10];
assign csr_mask = inst_csrxchg ? rj_value : 32'hffffffff;
assign csr_wvalue = rkd_value;                          


assign alu_op[ 0] = inst_add_w | inst_addi_w | inst_ld_w | inst_st_w
                    | inst_jirl | inst_bl | inst_pcaddu12i 
                    | inst_ld_b | inst_ld_bu | inst_ld_h | inst_ld_hu
                    | inst_st_h | inst_st_b;
assign alu_op[ 1] = inst_sub_w;
assign alu_op[ 2] = inst_slt | inst_slti;// | inst_blt | inst_bge;//
assign alu_op[ 3] = inst_sltu | inst_sltui;// | inst_bltu | inst_bgeu;//
assign alu_op[ 4] = inst_and | inst_andi;
assign alu_op[ 5] = inst_nor;
assign alu_op[ 6] = inst_or | inst_ori;
assign alu_op[ 7] = inst_xor | inst_xori;
assign alu_op[ 8] = inst_slli_w | inst_sll_w;
assign alu_op[ 9] = inst_srli_w | inst_srl_w;
assign alu_op[10] = inst_srai_w | inst_sra_w;
assign alu_op[11] = inst_lu12i_w;
assign alu_op2[0] = inst_mul_w | inst_mulh_w | inst_mulh_wu | inst_mod_w | inst_mod_wu | inst_div_w | inst_div_wu;
assign alu_op2[1] = inst_mul_w | inst_mulh_w | inst_mulh_wu;  
assign alu_op2[2] = inst_mulh_wu | inst_div_wu | inst_mod_wu; 
assign alu_op2[3] = inst_mulh_w | inst_mulh_wu | inst_div_wu | inst_div_w; 

assign need_ui5   =  inst_slli_w | inst_srli_w | inst_srai_w;
assign need_ui12  =  inst_andi | inst_ori | inst_xori;

assign need_si12  =  inst_addi_w | inst_ld_w | inst_st_w | inst_slti | inst_sltui | inst_st_h | inst_st_b;
assign need_si16  =  inst_jirl | inst_beq | inst_bne | inst_blt | inst_bge | inst_bltu | inst_bgeu;
assign need_si20  =  inst_lu12i_w | inst_pcaddu12i;
assign need_si26  =  inst_b | inst_bl;
assign src2_is_4  =  inst_jirl | inst_bl;

assign rj_read_need = inst_add_w | inst_sub_w | inst_slt | inst_sltu | inst_or | inst_and | inst_xor | inst_nor |
                       inst_slli_w | inst_srli_w | inst_srai_w | inst_beq | inst_bne | inst_jirl | inst_ld_w | inst_st_w | inst_addi_w |
                       inst_ori | inst_xori | inst_andi | inst_sltui | inst_slti | inst_mul_w | inst_mulh_w | inst_mulh_wu | 
                       inst_div_w | inst_mod_w | inst_div_wu | inst_mod_wu | inst_sll_w | inst_srl_w | inst_sra_w 
                       | inst_blt | inst_bge | inst_bltu | inst_bgeu | inst_st_h | inst_st_b 
                       | inst_csrxchg;
assign rd_read_need = inst_beq | inst_bne | inst_st_w | inst_blt | inst_bge | inst_bltu | inst_bgeu | inst_st_h | inst_st_b |
                        inst_csrwd | inst_csrxchg;
assign rk_read_need = inst_add_w | inst_sub_w | inst_slt | inst_sltu | inst_or | inst_and | inst_xor | inst_nor | 
                      inst_jirl | inst_mul_w | inst_mulh_w | inst_mulh_wu | inst_div_w | inst_mod_w | inst_div_wu | inst_mod_wu
                      | inst_sll_w | inst_srl_w | inst_sra_w;
                      
assign exe_target ={5{exe_rf_we}} & exe_dest;
assign mem_target ={5{mem_rf_we}} & mem_dest;
assign wb_target ={5{wb_rf_we}} & wb_dest;
 
assign exe_eq_target = (!exe_valid)? 1'b0:                          
                      (exe_target==5'b00000)? 1'b0:                                                             
                      (rj_read_need & (rj==exe_target))| (rk_read_need & (rk==exe_target))| (rd_read_need & (rd==exe_target)) ;
           
assign mem_eq_target = (!mem_to_wb_valid)? 1'b0:                                 
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

assign src_reg_is_rd = inst_beq | inst_bne | inst_blt | inst_bge | inst_bltu | inst_bgeu | inst_st_w | inst_st_h | inst_st_b
                        | inst_csrwd | inst_csrxchg;

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
                       inst_bl     | //
                       inst_ld_b   |
                       inst_ld_h   | 
                       inst_ld_bu  |
                       inst_ld_hu  |
                       inst_st_h   |
                       inst_st_b;

assign res_from_mem  = inst_ld_w | inst_ld_b |inst_ld_h | inst_ld_bu |inst_ld_hu;
assign res_from_counter = inst_rdcntvl_w | inst_rdcntvh_w;
assign dst_is_r1     = inst_bl;
assign br_stall      = !id_ready_go & (inst_beq | inst_bne | inst_blt | inst_bge | inst_bltu | inst_bgeu | inst_b | inst_bl);// | inst_jirl);

assign gr_we         = ~inst_st_w & ~inst_beq & ~inst_bne & ~inst_b & ~inst_blt & ~inst_bge & ~inst_bltu & ~inst_bgeu & ~inst_st_h & ~inst_st_b
                     & ~inst_tlbsrch & ~inst_tlbwr & ~inst_tlbrd & ~inst_tlbfill & ~inst_invtlb;// added in exp 18
assign mem_we        = inst_st_w | inst_st_h | inst_st_b;
assign dest          = dst_is_r1 ? 5'd1 :
                       inst_rdcntid_w? rj: rd;

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

assign rj_value  =(exe_eq_target && rj_read_need && ~exe_res_from_mem && (rj==exe_target))? result          :
                  (mem_eq_target && rj_read_need &&                      (rj==mem_target))? final_result    :
                  (wb_eq_target && rj_read_need  &&                      (rj==wb_target) )? wb_final_result :
                                                                                            rf_rdata1;
assign rkd_value =(exe_eq_target && ((rk_read_need && (rk==exe_target)) || (rd_read_need && (rd==exe_target)) && ~exe_res_from_mem ))? result          :
                  (mem_eq_target && ((rk_read_need && (rk==mem_target)) || (rd_read_need && (rd==mem_target))                      ))? final_result    :
                  (wb_eq_target  && ((rk_read_need && (rk==wb_target )) || (rd_read_need && (rd==wb_target ))                      ))? wb_final_result :
                                                                                                                                       rf_rdata2;
//compare
assign rj_eq_rd   = (rj_value == rkd_value);
assign rj_lt_rd_u = (rj_value < rkd_value);
assign rj_ge_rd_u = !rj_lt_rd_u;
assign rj_lt_rd = 
    (!rj_value[31] & !rkd_value[31]) & (rj_value[30:0] < rkd_value[30:0]) |
    (rj_value[31] & !rkd_value[31]) |
    (rj_value[31] & rkd_value[31]) & (rj_value[30:0] < rkd_value[30:0]);
assign rj_ge_rd = !rj_lt_rd;
assign br_taken =  (inst_beq  &&  rj_eq_rd
                   || inst_bne  && !rj_eq_rd
                   || inst_bltu && rj_lt_rd_u
                   || inst_bgeu && rj_ge_rd_u
                   || inst_blt && rj_lt_rd
                   || inst_bge && rj_ge_rd
                   || inst_jirl
                   || inst_bl
                   || inst_b) && id_valid;            

assign br_target = (inst_beq || inst_bne || inst_bl || inst_b || inst_bge || inst_bgeu || inst_blt || inst_bltu) ? (id_pc + br_offs) :
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
//EXE_stage


always @(posedge clk) begin
    if(!resetn) begin
        exe_reg_wb_ex <= 1'b0;
        exe_reg_ertn_flush <= 1'b0;
    end else if (id_to_exe_valid && exe_allowin)begin
        exe_reg_wb_ex <= 1'b0;
        exe_reg_ertn_flush <= 1'b0;
    end else if (wb_ex || ertn_flush || wb_refetch) begin
        if (wb_ex) begin
            exe_reg_wb_ex <= 1'b1;
        end
        if (ertn_flush) begin
            exe_reg_ertn_flush <= 1'b1;
        end
    end
end

div div (                                   
  .aclk(clk),                               // input wire aclk
  .aresetn(~ex_sig),                        //reset            
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
  .aclk(clk),                                  // input wire aclk
  .aresetn(~ex_sig),                            //reset          
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
               (exe_counter[0] & exe_counter[1]) ? counter[63:32]:
               (exe_counter[0] & exe_counter[1]) ? counter[31:0]:            
                                        alu_result;
//exe to tlb
assign s1_asid = invtlb_valid ? exe_rj_value[9:0] : w_asid;
assign s1_vppn = tlbsrch_valid ? w_vppn : 
                (invtlb_valid ? exe_rkd_value[31:13] 
                                        : s_vppn);
assign s1_va_bit12 = invtlb_valid ? exe_rkd_value[12] : s_va_bit12;

assign mem_type = {1'b0, exe_mem_we};    //00:load 01:store

wire tlb_ex_level = exe_data_sram_en & ~id_to_exe_ex & ~ADEM & ~ALE;

assign exe_refill = tlb_ex_level  & s_refill;
assign exe_PIL = tlb_ex_level & s_PIL;
assign exe_PIS = tlb_ex_level & s_PIS;
assign exe_PPI = tlb_ex_level & s_PPI;
assign exe_PME = tlb_ex_level & s_PME;

vaddr_trans v1(
    .vaddr(alu_result),
    .paddr(s_paddr),

    .asid(w_asid),
    .s_vppn(s_vppn),           
    .s_va_bit12(s_va_bit12),
    .s_asid(s_asid),

    .da(da),
    .pg(pg),

    .crmd_plv(crmd_plv),
    .dmw0_vseg(dmw0_vseg),
    .dmw0_pseg(dmw0_pseg),
    .dmw0_plv(dmw0_plv),
    .dmw1_vseg(dmw1_vseg),
    .dmw1_pseg(dmw1_pseg),
    .dmw1_plv(dmw1_plv),

    .s_found(s1_found),          
    .s_ppn(s1_ppn),            
    .s_ps(s1_ps),
    .s_plv(s1_plv),
    //.s_mat,
    .s_d(s1_d),
    .s_v(s1_v),

    .mem_type(mem_type),

    .mapping_ADEF(),
    .mapping_ADEM(mapping_ADEM),        

    .tlb_refill(s_refill),
    .tlb_PIL(s_PIL),
    .tlb_PIS(s_PIS),
    .tlb_PIF(),
    .tlb_PME(s_PME),     
    .tlb_PPI(s_PPI)
);

//store
assign st_data = exe_inst_st_b ? {4{exe_rkd_value[7 :0]}} :
                 exe_inst_st_h ? {2{exe_rkd_value[15:0]}} :
                                    exe_rkd_value;


//assign data_sram_wdata = st_data;
assign st_b_strb = 4'b0001 << (alu_result[1:0]);
assign st_h_strb = {{2{alu_result[1]}},{2{!alu_result[1]}}};
assign st_w_strb = 4'b1111;
assign st_strb = 
    ({4{exe_inst_st_b}} & st_b_strb) |
    ({4{exe_inst_st_h}} & st_h_strb) |
    ({4{exe_inst_st_w}} & st_w_strb);
//load
//assign mem_result   = data_sram_rdata; //need to be changed
assign final_result = mem_res_from_mem && !mem_ld_cancel ? mem_result : mem_alu_result;                
assign load_word = data_sram_rdata;

assign last     =  mem_alu_result[1:0];
assign bz = (!last[1] & !last[0]);
assign lz = (last[1] & !last[0]);
assign fz = (!last[1] & last[0]);
assign nz = (last[1] & last[0]);
assign byte = nz ? load_word[31:24] :
              lz ? load_word[23:16] :
              fz ? load_word[15: 8] :
                   load_word[ 7: 0];
assign half_word = last[1]?load_word[31:16] :load_word[15:0];

assign extended_half = 
    mem_inst_ld_h ? {{16{half_word[15]}},half_word} : // sign-extend
    {{16{1'b0}},half_word} ;    //zero-extend
assign extended_byte =
    mem_inst_ld_b ? {{24{byte[7]}},byte} : 
    {{24{1'b0}},byte};
assign mem_result = 
    ({32{mem_inst_ld_w}} & load_word) |
    ({32{mem_inst_ld_b | mem_inst_ld_bu}} & extended_byte) |
    ({32{mem_inst_ld_h | mem_inst_ld_hu}} & extended_half); 

wire csr_wen = wb_valid & wb_csr_we;            
assign ertn_flush = wb_valid & wb_ertn_flush;   
assign csr_wr_en = wb_valid & wb_inst_csr;

//加入了异常优先级的判断
wire wb_exe_ex;

assign wb_exe_ex = wb_valid & wb_exe_tlb_ex;
assign exe_tlb_ex = exe_valid & (exe_PIL | exe_PIS | exe_PPI | exe_PME | exe_refill);

//IF > ID
assign sys_ex = inst_syscall & ~if_to_id_ex; 
assign brk_ex = inst_break   & ~if_to_id_ex;

assign pre_if_ex = pre_if_valid & (ADEF | f_PIF | f_PPI | f_refill);

assign id_ex   = id_valid & (if_to_id_ex | INE | sys_ex | brk_ex | has_int); 

assign exe_ex = exe_valid & (exe_sys_ex | exe_brk_ex | exe_int_ex | exe_ine_ex | exe_adef_ex | ALE | ADEM |
                exe_PIF_ex | exe_PIL | exe_PIS | exe_PPI_ex | exe_PPI | exe_PME | exe_refill | exe_refill_ex);      
assign mem_ex = mem_valid & (mem_sys_ex | mem_brk_ex | mem_int_ex | mem_ine_ex | mem_adef_ex | mem_ale_ex | 
                mem_PIF_ex | mem_PIL_ex | mem_PIS_ex | mem_PPI_ex | mem_PME_ex | mem_refill_ex | mem_adem_ex);         
assign wb_ex = wb_valid & (wb_sys_ex | wb_brk_ex | wb_ine_ex | wb_int_ex | wb_adef_ex | wb_ale_ex | 
                wb_PIF_ex | wb_PIL_ex | wb_PIS_ex | wb_PPI_ex | wb_PME_ex | wb_refill_ex | wb_adem_ex);    

assign rf_we    = wb_rf_we && wb_valid && ~wb_ex;                   
assign rf_waddr = wb_dest;
assign rf_wdata = csr_wr_en ? csr_rvalue : wb_final_result;         
//Ecode Esubcode
assign wb_ecode = wb_int_ex     ? 6'h00:
                  wb_ale_ex     ? 6'h09:
                  (wb_adef_ex | wb_adem_ex)    ? 6'h08:
                  wb_sys_ex     ? 6'h0b:
                  wb_brk_ex     ? 6'h0c:
                  wb_ine_ex     ? 6'h0d:
                  wb_PIL_ex     ? 6'h01:
                  wb_PIS_ex     ? 6'h02:
                  wb_PIF_ex     ? 6'h03:
                  wb_PME_ex     ? 6'h04:
                  wb_PPI_ex     ? 6'h07:
                  wb_refill_ex  ? 6'h3f:
                                6'h00;
 //exp12 only for SYS
assign wb_esubcode = wb_adef_ex ? 9'h00 :
                     wb_adem_ex ? 9'h01 :
                                  9'h00;                  //exp13 has no esubcode

//add in exp18
wire [4:0] id_invtlb_op;
assign id_invtlb_op = rd;
//TLB指令引发的重取指机制 判断在id级
assign id_refetch_flag = inst_tlbfill || inst_tlbwr || inst_tlbrd || inst_invtlb;
 //|| (csr_we && (csr_num == `CSR_CRMD || csr_num == `CSR_ASID));
//重取标志和标记异常的指令处理方式与异常等相同 这里用wb_refetch来表示 
assign wb_refetch = wb_refetch_flag && wb_valid;
assign refetch_pc = inst_invtlb && id_invtlb_op > 5'd6 ? ex_entry : wb_pc + 4;
//
//CSR中TLB相关的控制信号
//
//指令类型
assign tlbsrch_valid = exe_inst_tlbsrch && exe_valid;
assign tlbrd_valid = wb_inst_tlbrd && wb_valid;
assign tlbwr_valid = wb_inst_tlbwr && wb_valid;
assign tlbfill_valid = wb_inst_tlbfill && wb_valid;
//assign invtlb_valid = wb_inst_invtlb;
assign invtlb_valid = exe_inst_invtlb && exe_valid;
wire [3:0] tlbidx_idx;
assign r_index = tlbidx_idx;
assign w_index = tlbidx_idx;// ? tlbidx_idx : counter;//tlbidx_idx;//
assign we = tlbwr_valid || tlbfill_valid;

assign tlbsrch_hit = s1_found;// wb_tlbsrch_hit;
assign tlbsrch_hit_idx = s1_index;// wb_tlbsrch_hit_idx;

csr csr1(.clk(clk),
        .reset(~resetn),                    
        .csr_re(1'b1),                      
        .csr_num(wb_csr_num),               
        .csr_we(csr_wen),                   
        .csr_wmask(wb_csr_wmask),           
        .csr_wvalue(wb_csr_wvalue),         
        .wb_ex(wb_ex),                      
        .ertn_flush(ertn_flush),            
        .wb_ecode(wb_ecode),                
        .wb_esubcode(wb_esubcode),
        .wb_exe_ex(wb_exe_ex),          
        .wb_pc(wb_pc),                      
        .wb_vaddr(wb_vaddr),                
        .csr_rvalue(csr_rvalue),            
        .ex_entry(ex_entry),                
        .ertn_pc(ertn_pc),                  
        .has_int(has_int),
        .da(da),
        .pg(pg),
        .tlb_rentry(tlb_rentry),
        .crmd_plv(crmd_plv),
        .dmw0_pseg(dmw0_pseg),
        .dmw0_vseg(dmw0_vseg),
        .dmw0_plv(dmw0_plv),
        .dmw1_pseg(dmw1_pseg),
        .dmw1_vseg(dmw1_vseg),
        .dmw1_plv(dmw1_plv),
        //add in exp18       
        .tlbsrch_valid(tlbsrch_valid),
        .tlbrd_valid(tlbrd_valid),
        .tlbwr_valid(tlbwr_valid),
        .tlbfill_valid(tlbfill_valid),
        .tlbsrch_hit(tlbsrch_hit),
        .tlbsrch_hit_idx(tlbsrch_hit_idx),  
        .tlbidx_idx(tlbidx_idx),  
        //write
        .tlb_w_e(w_e),
        .tlb_w_vppn(w_vppn),
        .tlb_w_ps(w_ps),
        .tlb_w_asid(w_asid),
        .tlb_w_g(w_g),
        .tlb_w_ppn0(w_ppn0),
        .tlb_w_plv0(w_plv0),
        .tlb_w_mat0(w_mat0),
        .tlb_w_d0(w_d0),
        .tlb_w_v0(w_v0),    
        .tlb_w_ppn1(w_ppn1),
        .tlb_w_plv1(w_plv1),
        .tlb_w_mat1(w_mat1),
        .tlb_w_d1(w_d1),
        .tlb_w_v1(w_v1),  
        //read
        .tlb_r_e(r_e),
        .tlb_r_vppn(r_vppn),
        .tlb_r_ps(r_ps),
        .tlb_r_asid(r_asid),
        .tlb_r_g(r_g),
        .tlb_r_ppn0(r_ppn0),
        .tlb_r_plv0(r_plv0),
        .tlb_r_mat0(r_mat0),
        .tlb_r_d0(r_d0),
        .tlb_r_v0(r_v0),    
        .tlb_r_ppn1(r_ppn1),
        .tlb_r_plv1(r_plv1),
        .tlb_r_mat1(r_mat1),
        .tlb_r_d1(r_d1),
        .tlb_r_v1(r_v1)
);
//实例化TLB模块
tlb #(.TLBNUM(16)) tlb0(
    .clk        (clk),
    // search port 0 (for fetch)·
    .s0_vppn    (s0_vppn),
    .s0_va_bit12(s0_va_bit12),
    .s0_asid    (s0_asid),
    .s0_found   (s0_found),
    .s0_index   (s0_index),
    .s0_ppn     (s0_ppn),
    .s0_ps      (s0_ps),
    .s0_plv     (s0_plv),
    .s0_mat     (s0_mat),
    .s0_d       (s0_d),
    .s0_v       (s0_v),
    // search port 1 (for load/store)
    .s1_vppn    (s1_vppn),
    .s1_va_bit12(s1_va_bit12),
    .s1_asid    (s1_asid),
    .s1_found   (s1_found),
    .s1_index   (s1_index),
    .s1_ppn     (s1_ppn),
    .s1_ps      (s1_ps),
    .s1_plv     (s1_plv),
    .s1_mat     (s1_mat),
    .s1_d       (s1_d),
    .s1_v       (s1_v),
    // invtlb opcode   //use for INVTLB
    .invtlb_op  (invtlb_op),
    .invtlb_valid(invtlb_valid),
    // write port //use for TLBWR and TLBFILL
    .we         (we),
    .w_index    (w_index),
    .w_e        (w_e),
    .w_vppn     (w_vppn),
    .w_ps       (w_ps),
    .w_asid     (w_asid),
    .w_g        (w_g),
    .w_ppn0     (w_ppn0),
    .w_plv0     (w_plv0),
    .w_mat0     (w_mat0),
    .w_d0       (w_d0),
    .w_v0       (w_v0),
    .w_ppn1     (w_ppn1),
    .w_plv1     (w_plv1),
    .w_mat1     (w_mat1),
    .w_d1       (w_d1),
    .w_v1       (w_v1),
     // read port /used for TLBRD
    .r_index    (r_index),
    .r_e        (r_e),
    .r_vppn     (r_vppn),
    .r_ps       (r_ps),
    .r_asid     (r_asid),
    .r_g        (r_g),
    .r_ppn0     (r_ppn0),
    .r_plv0     (r_plv0),
    .r_mat0     (r_mat0),
    .r_d0       (r_d0),
    .r_v0       (r_v0),
    .r_ppn1     (r_ppn1),
    .r_plv1     (r_plv1),
    .r_mat1     (r_mat1),
    .r_d1       (r_d1),
    .r_v1       (r_v1)
);
//debug info generate
assign debug_wb_pc       = wb_pc;
assign debug_wb_rf_we   = {4{rf_we}};                   
assign debug_wb_rf_wnum  = wb_dest;
assign debug_wb_rf_wdata = rf_wdata;

endmodule