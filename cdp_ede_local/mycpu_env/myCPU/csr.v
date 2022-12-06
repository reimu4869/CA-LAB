`include "csr_head.v"
//根据下一阶段实验可能需要添加信号

module csr(
    input wire          clk,
    input wire          reset,

    input wire          csr_re,
    input wire [13:0]   csr_num,

    input wire          csr_we,
    input wire [31:0]   csr_wmask,
    input wire [31:0]   csr_wvalue,

    input wire          wb_ex,
    input wire          ertn_flush,

    input wire          wb_exe_ex,      //  用于判断TLB异常是否来源于exe级
    input wire [ 5:0]   wb_ecode,
    input wire [ 8:0]   wb_esubcode,

    input wire [31:0]   wb_pc,
    input wire [31:0]   wb_vaddr,

    output wire [31:0]  csr_rvalue,
    output wire [31:0]  ex_entry,
    output wire [31:0]  ertn_pc,
    output wire         has_int,
    //exp19 add 用于虚实地址转换
    output wire [31:0]  tlb_rentry,

    output wire         da,
    output wire         pg,

    output wire [ 1:0]  crmd_plv,

    output wire [ 2:0]  dmw0_pseg,
    output wire [ 2:0]  dmw0_vseg,
    output wire [ 3:0]  dmw0_plv,
    output wire [ 2:0]  dmw1_pseg,
    output wire [ 2:0]  dmw1_vseg,
    output wire [ 3:0]  dmw1_plv, 
    //added in exp18: tlb
    //insts
    input wire         tlbsrch_valid,
    input wire         tlbrd_valid,
    input wire         tlbwr_valid,
    input wire         tlbfill_valid,
    //srch hit?
    input wire         tlbsrch_hit,
    input wire  [3:0 ] tlbsrch_hit_idx,

    output reg  [3:0 ] tlbidx_idx,
    //from tlb module
    //wirte
    output wire         tlb_w_e,  
    output wire [ 18:0] tlb_w_vppn,
    output wire  [ 5:0] tlb_w_ps,
    output wire  [ 9:0] tlb_w_asid,
    output wire         tlb_w_g,
    output wire [ 19:0] tlb_w_ppn0,
    output wire  [ 1:0] tlb_w_plv0,
    output wire  [ 1:0] tlb_w_mat0,
    output wire         tlb_w_d0,
    output wire         tlb_w_v0,
    output wire [ 19:0] tlb_w_ppn1,
    output wire  [ 1:0] tlb_w_plv1,
    output wire  [ 1:0] tlb_w_mat1,
    output wire         tlb_w_d1,
    output wire         tlb_w_v1,
    //read
    input wire         tlb_r_e,
    input wire [ 18:0] tlb_r_vppn,
    input wire  [ 5:0] tlb_r_ps,
    input wire  [ 9:0] tlb_r_asid,
    input wire         tlb_r_g,
    input wire [ 19:0] tlb_r_ppn0,
    input wire  [ 1:0] tlb_r_plv0,
    input wire  [ 1:0] tlb_r_mat0,
    input wire         tlb_r_d0,
    input wire         tlb_r_v0,
    input wire [ 19:0] tlb_r_ppn1,
    input wire  [ 1:0] tlb_r_plv1,
    input wire  [ 1:0] tlb_r_mat1,
    input wire         tlb_r_d1,
    input wire         tlb_r_v1
);
//crmd
reg  [ 1:0] csr_crmd_plv;
reg         csr_crmd_ie;
/*--- exp19 added ---*/
reg         csr_crmd_da;
reg         csr_crmd_pg;
reg  [ 1:0] csr_crmd_datf;
reg  [ 1:0] csr_crmd_datm;
//prmd
reg  [ 1:0] csr_prmd_pplv;
reg         csr_prmd_pie;
//
reg  [13:0] csr_ecfg_lie;
//estat
reg  [12:0] csr_estat_is;
reg  [ 5:0] csr_estat_ecode;
reg  [ 8:0] csr_estat_esubcode;

//era
reg  [31:0] csr_era_pc;

reg  [31:0] csr_badv_vaddr;
wire        wb_ex_addr_err;

reg  [25:0] csr_eentry_va;
//save
reg  [31:0] csr_save0_data;
reg  [31:0] csr_save1_data;
reg  [31:0] csr_save2_data;
reg  [31:0] csr_save3_data;
//tid
reg  [31:0] csr_tid_tid;
//tcfg
reg         csr_tcfg_en;
reg         csr_tcfg_periodic;
reg  [29:0] csr_tcfg_initval;
wire [31:0] tcfg_next_value;
wire [31:0] csr_tval;
reg  [31:0] timer_cnt;
//ticlr
wire        csr_ticlr_clr;

//TLB寄存器
//命名来至指令集手册7.5节
//TLB索引（TLBIDX）
wire [31:0] tlbidx;
//reg [3 :0] tlbidx_idx;//n:0 //as output
reg [5 :0] tlbidx_ps;//29:24
reg        tlbidx_ne;//31:31
//TLB表项高位（TLBEHI）
//exp19 add 'tlb_ex'
wire        tlb_ex;
wire [31:0] tlbehi;
reg [18:0] tlbehi_vppn;
//TLB表项低位(TLBELO0,TLBELO1)
wire [31:0]tlbelo0      , tlbelo1;
reg        tlbelo0_v    , tlbelo1_v;//0:0, 有效位
reg        tlbelo0_d    , tlbelo1_d;//1:1, 脏位
reg [1 :0] tlbelo0_plv  , tlbelo1_plv;//3:2
reg [1 :0] tlbelo0_mat  , tlbelo1_mat;//5:4
reg        tlbelo0_g    , tlbelo1_g;//6:6
reg [23:0] tlbelo0_ppn  , tlbelo1_ppn;//31:6 高4位为0
//地址空间标识符（ASID）
wire [31:0] asid;
reg [9 :0] asid_asid; //9:0
wire [7 :0] asid_asidbits;//23:16
//TLB重填例外入口地址（TLBRENTRY）
wire [31:0] tlbrentry;
reg [25:0] tlbrentry_pa;
//DMW0 DMW1 
reg          csr_dmw0_plv0   , csr_dmw1_plv0;
reg          csr_dmw0_plv3   , csr_dmw1_plv3;
reg  [1:0]   csr_dmw0_mat    , csr_dmw1_mat;
reg  [2:0]   csr_dmw0_pseg   , csr_dmw1_pseg;
reg  [2:0]   csr_dmw0_vseg   , csr_dmw1_vseg;


//CRMD
always @(posedge clk) begin
    if (reset) begin 
        csr_crmd_plv <= 2'b0;
        csr_crmd_ie <= 1'b0;
    end
    else if (wb_ex) begin
        csr_crmd_plv <= 2'b0;
        csr_crmd_ie <= 1'b0;
    end
    else if (ertn_flush)begin
        csr_crmd_plv <= csr_prmd_pplv;
        csr_crmd_ie <= csr_prmd_pie;
    end
    else if (csr_we && csr_num==`CSR_CRMD) begin
        csr_crmd_plv <= csr_wmask[`CSR_CRMD_PLV] & csr_wvalue[`CSR_CRMD_PLV]
                        | ~csr_wmask[`CSR_CRMD_PLV] & csr_crmd_plv;
        csr_crmd_ie <= csr_wmask[`CSR_CRMD_IE] & csr_wvalue[`CSR_CRMD_IE]
                            | ~csr_wmask[`CSR_CRMD_IE] & csr_crmd_ie;
    end     
end

//CRMD DA PG
always @(posedge clk) begin
    if (reset) begin 
        csr_crmd_da <= 1'b1;
        csr_crmd_pg <= 1'b0;
    end
    else if(wb_ex && (wb_ecode == 6'h3f))begin
        csr_crmd_da <= 1'b1;
        csr_crmd_pg <= 1'b0;
    end
    else if(ertn_flush && (csr_estat_ecode == 6'h3f))begin
        csr_crmd_da <= 1'b0;
        csr_crmd_pg <= 1'b1;
    end   
    else if (csr_we && csr_num==`CSR_CRMD) begin
        csr_crmd_da <= csr_wmask[`CSR_CRMD_DA] & csr_wvalue[`CSR_CRMD_DA]
                        | ~csr_wmask[`CSR_CRMD_DA] & csr_crmd_da;
        csr_crmd_pg <= csr_wmask[`CSR_CRMD_PG] & csr_wvalue[`CSR_CRMD_PG]
                        | ~csr_wmask[`CSR_CRMD_PG] & csr_crmd_pg;
    end     
end

//CRMD DATF DATM
always @(posedge clk) begin
    if (reset) begin 
       csr_crmd_datf <= 2'b0;
       csr_crmd_datm <= 2'b0;
    end
    else if (csr_we && csr_num==`CSR_CRMD) begin
        csr_crmd_datf <= csr_wmask[`CSR_CRMD_DATF] & csr_wvalue[`CSR_CRMD_DATF]
                        | ~csr_wmask[`CSR_CRMD_DATF] & csr_crmd_datf;
        csr_crmd_datm <= csr_wmask[`CSR_CRMD_DATM] & csr_wvalue[`CSR_CRMD_DATM]
                            | ~csr_wmask[`CSR_CRMD_DATM] & csr_crmd_datm;
    end     
end

//PRMD
always @(posedge clk) begin
    if (wb_ex) begin
        csr_prmd_pplv <= csr_crmd_plv;
        csr_prmd_pie <= csr_crmd_ie;
    end
    else if (csr_we && csr_num==`CSR_PRMD) begin
        csr_prmd_pplv <= csr_wmask[`CSR_PRMD_PPLV]&csr_wvalue[`CSR_PRMD_PPLV]
                        | ~csr_wmask[`CSR_PRMD_PPLV]&csr_prmd_pplv;
        csr_prmd_pie <= csr_wmask[`CSR_PRMD_PIE]&csr_wvalue[`CSR_PRMD_PIE]
                        | ~csr_wmask[`CSR_PRMD_PIE]&csr_prmd_pie;
    end
end

//ECFG
always @(posedge clk) begin
    if (reset)
        csr_ecfg_lie <= 13'b0;
    else if (csr_we && csr_num==`CSR_ECFG)
        csr_ecfg_lie <= csr_wmask[`CSR_ECFG_LIE]&csr_wvalue[`CSR_ECFG_LIE]
                        | ~csr_wmask[`CSR_ECFG_LIE]&csr_ecfg_lie;
end

//ESTAT IS
always @(posedge clk) begin
    if (reset)
        csr_estat_is[1:0] <= 2'b0;
    else if (csr_we && csr_num==`CSR_ESTAT)
        csr_estat_is[1:0] <= csr_wmask[`CSR_ESTAT_IS10] & csr_wvalue[`CSR_ESTAT_IS10]
                            | ~csr_wmask[`CSR_ESTAT_IS10] & csr_estat_is[1:0];
    csr_estat_is[9:2] <= 8'b0 /*hw_int_in[7:0]*/;                                           //exp12未实现中断，从而全部置0
    csr_estat_is[10] <= 1'b0;
    if (timer_cnt[31:0]==32'b0)
        csr_estat_is[11] <= 1'b1;
    else if (csr_we && csr_num==`CSR_TICLR && csr_wmask[`CSR_TICLR_CLR]
                                && csr_wvalue[`CSR_TICLR_CLR])
        csr_estat_is[11] <= 1'b0;
    csr_estat_is[12] <= 1'b0 /*ipi_int_in*/;                                                 //exp12未实现中断，从而全部置0
end

//ESTAT Ecode Esubcode
always @(posedge clk) begin
    if (wb_ex) begin
        csr_estat_ecode <= wb_ecode;
        csr_estat_esubcode <= wb_esubcode;
    end
end

//ERA
always @(posedge clk) begin
    if (wb_ex)
        csr_era_pc <= wb_pc;
    else if (csr_we && csr_num==`CSR_ERA)
        csr_era_pc <= csr_wmask[`CSR_ERA_PC]&csr_wvalue[`CSR_ERA_PC]
                        | ~csr_wmask[`CSR_ERA_PC]&csr_era_pc;
end

//BADV
assign wb_ex_addr_err = wb_ecode==`ECODE_ADE || wb_ecode==`ECODE_ALE || wb_ecode == `ECODE_TLBR ||
                        wb_ecode == `ECODE_PIF || wb_ecode == `ECODE_PIL || wb_ecode == `ECODE_PIS ||
                        wb_ecode == `ECODE_PPI || wb_ecode == `ECODE_PME;
always @(posedge clk) begin
    if (wb_ex && wb_ex_addr_err)
        csr_badv_vaddr <= (wb_ecode==`ECODE_ADE &&
                        wb_esubcode==`ESUBCODE_ADEF || tlb_ex && ~wb_exe_ex) ? wb_pc : wb_vaddr;
end

//EENTRY
always @(posedge clk) begin
    if (csr_we && csr_num==`CSR_EENTRY)
        csr_eentry_va <= csr_wmask[`CSR_EENTRY_VA]&csr_wvalue[`CSR_EENTRY_VA]
                        | ~csr_wmask[`CSR_EENTRY_VA]&csr_eentry_va;
end

//SAVE0~3
always @(posedge clk) begin
    if (csr_we && csr_num==`CSR_SAVE0)
        csr_save0_data <= csr_wmask[`CSR_SAVE_DATA]&csr_wvalue[`CSR_SAVE_DATA]
                            | ~csr_wmask[`CSR_SAVE_DATA]&csr_save0_data;
    if (csr_we && csr_num==`CSR_SAVE1)
        csr_save1_data <= csr_wmask[`CSR_SAVE_DATA]&csr_wvalue[`CSR_SAVE_DATA]
                            | ~csr_wmask[`CSR_SAVE_DATA]&csr_save1_data;
    if (csr_we && csr_num==`CSR_SAVE2)
        csr_save2_data <= csr_wmask[`CSR_SAVE_DATA]&csr_wvalue[`CSR_SAVE_DATA]
                            | ~csr_wmask[`CSR_SAVE_DATA]&csr_save2_data;
    if (csr_we && csr_num==`CSR_SAVE3)
        csr_save3_data <= csr_wmask[`CSR_SAVE_DATA]&csr_wvalue[`CSR_SAVE_DATA]
                            | ~csr_wmask[`CSR_SAVE_DATA]&csr_save3_data;
end

//TID
always @(posedge clk) begin
    if (reset)
        csr_tid_tid <= 31'b0 /*coreid_in*/;                                             //exp12暂时不实现，从而置0
    else if (csr_we && csr_num==`CSR_TID)
        csr_tid_tid <= csr_wmask[`CSR_TID_TID]&csr_wvalue[`CSR_TID_TID]
                        | ~csr_wmask[`CSR_TID_TID]&csr_tid_tid;
end

//TCFG
always @(posedge clk) begin
    if (reset)
        csr_tcfg_en <= 1'b0;
    else if (csr_we && csr_num==`CSR_TCFG)
        csr_tcfg_en <= csr_wmask[`CSR_TCFG_EN] & csr_wvalue[`CSR_TCFG_EN]
                        | ~csr_wmask[`CSR_TCFG_EN] & csr_tcfg_en;
    if (csr_we && csr_num==`CSR_TCFG) begin
        csr_tcfg_periodic <= csr_wmask[`CSR_TCFG_PERIOD]&csr_wvalue[`CSR_TCFG_PERIOD]
                            | ~csr_wmask[`CSR_TCFG_PERIOD]&csr_tcfg_periodic;
        csr_tcfg_initval <= csr_wmask[`CSR_TCFG_INITVAL]&csr_wvalue[`CSR_TCFG_INITVAL]
                            | ~csr_wmask[`CSR_TCFG_INITVAL]&csr_tcfg_initval;
end
end

//TVAL
assign tcfg_next_value = csr_wmask[31:0]&csr_wvalue[31:0]
                        | ~csr_wmask[31:0]&{csr_tcfg_initval,
                            csr_tcfg_periodic, csr_tcfg_en};

always @(posedge clk) begin
    if (reset)
        timer_cnt <= 32'hffffffff;
    else if (csr_we && csr_num==`CSR_TCFG && tcfg_next_value[`CSR_TCFG_EN])
        timer_cnt <= {tcfg_next_value[`CSR_TCFG_INITVAL], 2'b0};
    else if (csr_tcfg_en && timer_cnt!=32'hffffffff) begin
        if (timer_cnt[31:0]==32'b0 && csr_tcfg_periodic)
            timer_cnt <= {csr_tcfg_initval, 2'b0};
        else
            timer_cnt <= timer_cnt - 1'b1;
    end
end
assign csr_tval = timer_cnt[31:0];

//TICLR 
assign csr_ticlr_clr = 1'b0;

wire [31:0] csr_crmd_rvalue = {23'b0,csr_crmd_datm,csr_crmd_datf,csr_crmd_pg,csr_crmd_da,csr_crmd_ie, csr_crmd_plv};
wire [31:0] csr_prmd_rvalue = {29'b0, csr_prmd_pie, csr_prmd_pplv};
wire [31:0] csr_ecfg_rvalue = {19'b0, csr_ecfg_lie};
wire [31:0] csr_estat_rvalue = {1'b0,csr_estat_esubcode,csr_estat_ecode,
                                3'b0, csr_estat_is};
wire [31:0] csr_era_rvalue = csr_era_pc;
wire [31:0] csr_badv_rvalue = csr_badv_vaddr;
wire [31:0] csr_eentry_rvalue = {csr_eentry_va,6'b0};
wire [31:0] csr_save0_rvalue = csr_save0_data;
wire [31:0] csr_save1_rvalue = csr_save1_data;
wire [31:0] csr_save2_rvalue = csr_save2_data;
wire [31:0] csr_save3_rvalue = csr_save3_data;
wire [31:0] csr_tid_rvalue = csr_tid_tid;
wire [31:0] csr_tcfg_rvalue = {csr_tcfg_initval,csr_tcfg_periodic,csr_tcfg_en};
wire [31:0] csr_tval_rvalue = csr_tval;
wire [31:0] csr_ticlr_rvalue = {30'b0, csr_ticlr_clr};

assign ex_entry = csr_eentry_rvalue;
assign ertn_pc = csr_era_pc;
assign has_int = ((csr_estat_is[11:0] & csr_ecfg_lie[11:0]) != 12'b0)
                && (csr_crmd_ie == 1'b1);
            
/*  ---  TLB  ---  */
//TLBIDX
always @ (posedge clk)begin
    //tlbidx_idx
    if (reset) begin
        tlbidx_idx <= 4'b0;
    end else if (tlbsrch_valid && tlbsrch_hit) begin
        tlbidx_idx <= tlbsrch_hit_idx;
    end else if (csr_we && csr_num == `CSR_TLBIDX) begin
        tlbidx_idx <= csr_wmask[`CSR_TLBIDX_IDX] & csr_wvalue[`CSR_TLBIDX_IDX]
                            | ~csr_wmask[`CSR_TLBIDX_IDX] & tlbidx_idx;
    end
    //tlbidx_ps
    if (reset || (tlbrd_valid && !tlb_r_e)) begin
        tlbidx_ps <= 6'b0;
    end else if (tlbrd_valid) begin
        tlbidx_ps <= tlb_r_e ? tlb_r_ps : 6'b0 ;
    end else if (csr_we && csr_num == `CSR_TLBIDX) begin
        tlbidx_ps <= csr_wmask[`CSR_TLBIDX_PS] & csr_wvalue[`CSR_TLBIDX_PS]
                            | ~csr_wmask[`CSR_TLBIDX_PS] & tlbidx_ps;
    end
    //tlbidx_ne
    if (reset) begin
        tlbidx_ne <= 1'b1;
    end else if (tlbrd_valid) begin
        tlbidx_ne <= !tlb_r_e;
    end else if (tlbsrch_valid) begin
        tlbidx_ne <= !tlbsrch_hit;
    end else if (csr_we && csr_num == `CSR_TLBIDX) begin
        tlbidx_ne <= csr_wmask[`CSR_TLBIDX_NE] & csr_wvalue[`CSR_TLBIDX_NE]
                            | ~csr_wmask[`CSR_TLBIDX_NE] & tlbidx_ne;
    end
end
assign tlbidx = {tlbidx_ne, 1'b0, tlbidx_ps, 20'b0, tlbidx_idx};
//TLBEHI

assign tlb_ex = wb_ecode == `ECODE_TLBR || wb_ecode == `ECODE_PIF || wb_ecode == `ECODE_PIL || 
                wb_ecode == `ECODE_PIS || wb_ecode == `ECODE_PPI || wb_ecode == `ECODE_PME;

always @ (posedge clk)begin
    if (reset) begin
        tlbehi_vppn <= 19'b0;
    end
    else if(wb_ex & tlb_ex)begin
        tlbehi_vppn <= (wb_exe_ex ? wb_vaddr[31:13] : wb_pc[31:13]);
    end 
    else if (tlbrd_valid && !tlb_r_e) begin
        tlbehi_vppn <= 19'b0;
    end
    else if (tlbrd_valid && tlb_r_e) begin
        tlbehi_vppn <= tlb_r_vppn;
    end else if (csr_we && csr_num == `CSR_TLBEHI) begin
        tlbehi_vppn <= csr_wmask[`CSR_TLBEHI_VPPN] & csr_wvalue[`CSR_TLBEHI_VPPN]
                            | ~csr_wmask[`CSR_TLBEHI_VPPN] & tlbehi_vppn;
    end
end
assign tlbehi = {tlbehi_vppn, 13'b0};
// TLBELO
always @ (posedge clk) begin
    if (reset || (tlbrd_valid && !tlb_r_e)) begin
        tlbelo0_v   <= 1'b0;
        tlbelo0_d   <= 1'b0;
        tlbelo0_plv <= 2'b0;
        tlbelo0_mat <= 2'b0;
        tlbelo0_g   <= 1'b0;
        tlbelo0_ppn <= 24'b0;

        tlbelo1_v   <= 1'b0;
        tlbelo1_d   <= 1'b0;
        tlbelo1_plv <= 2'b0;
        tlbelo1_mat <= 2'b0;
        tlbelo1_g   <= 1'b0;
        tlbelo1_ppn <= 24'b0;
    end else if (tlbrd_valid && tlb_r_e) begin
        tlbelo0_v   <= tlb_r_v0;
        tlbelo0_d   <= tlb_r_d0;
        tlbelo0_plv <= tlb_r_plv0;
        tlbelo0_mat <= tlb_r_mat0;
        tlbelo0_g   <= tlb_r_g;
        tlbelo0_ppn <= {4'b0, tlb_r_ppn0};

        tlbelo1_v   <= tlb_r_v1;
        tlbelo1_d   <= tlb_r_d1;
        tlbelo1_plv <= tlb_r_plv1;
        tlbelo1_mat <= tlb_r_mat1;
        tlbelo1_g   <= tlb_r_g;
        tlbelo1_ppn <= {4'b0, tlb_r_ppn1};
    end else if (csr_we) begin
        if (csr_num == `CSR_TLBELO0) begin
            tlbelo0_v   <= csr_wmask[`CSR_TLBELO_V]   & csr_wvalue[`CSR_TLBELO_V]   |
                                ~csr_wmask[`CSR_TLBELO_V]   & tlbelo0_v;
            tlbelo0_d   <= csr_wmask[`CSR_TLBELO_D]   & csr_wvalue[`CSR_TLBELO_D]   |
                                ~csr_wmask[`CSR_TLBELO_D]   & tlbelo0_d;
            tlbelo0_plv <= csr_wmask[`CSR_TLBELO_PLV] & csr_wvalue[`CSR_TLBELO_PLV] |
                                ~csr_wmask[`CSR_TLBELO_PLV] & tlbelo0_plv;
            tlbelo0_mat <= csr_wmask[`CSR_TLBELO_MAT] & csr_wvalue[`CSR_TLBELO_MAT] |
                                ~csr_wmask[`CSR_TLBELO_MAT] & tlbelo0_mat;
            tlbelo0_g   <= csr_wmask[`CSR_TLBELO_G]   & csr_wvalue[`CSR_TLBELO_G]   |
                                ~csr_wmask[`CSR_TLBELO_G]   & tlbelo0_g;
            tlbelo0_ppn <= csr_wmask[`CSR_TLBELO_PPN] & csr_wvalue[`CSR_TLBELO_PPN] |
                                ~csr_wmask[`CSR_TLBELO_PPN] & tlbelo0_ppn;
        end else if (csr_num == `CSR_TLBELO1) begin
            tlbelo1_v   <= csr_wmask[`CSR_TLBELO_V]   & csr_wvalue[`CSR_TLBELO_V]   |
                              ~csr_wmask[`CSR_TLBELO_V]   & tlbelo1_v;
            tlbelo1_d   <= csr_wmask[`CSR_TLBELO_D]   & csr_wvalue[`CSR_TLBELO_D]   |
                              ~csr_wmask[`CSR_TLBELO_D]   & tlbelo1_d;
            tlbelo1_plv <= csr_wmask[`CSR_TLBELO_PLV] & csr_wvalue[`CSR_TLBELO_PLV] |
                              ~csr_wmask[`CSR_TLBELO_PLV] & tlbelo1_plv;
            tlbelo1_mat <= csr_wmask[`CSR_TLBELO_MAT] & csr_wvalue[`CSR_TLBELO_MAT] |
                              ~csr_wmask[`CSR_TLBELO_MAT] & tlbelo1_mat;
            tlbelo1_g   <= csr_wmask[`CSR_TLBELO_G]   & csr_wvalue[`CSR_TLBELO_G]   |
                              ~csr_wmask[`CSR_TLBELO_G]   & tlbelo1_g;
            tlbelo1_ppn <= csr_wmask[`CSR_TLBELO_PPN] & csr_wvalue[`CSR_TLBELO_PPN] |
                              ~csr_wmask[`CSR_TLBELO_PPN] & tlbelo1_ppn;
        end
    end
end
assign tlbelo0 = {tlbelo0_ppn, 1'b0, tlbelo0_g, tlbelo0_mat, tlbelo0_plv, tlbelo0_d, tlbelo0_v};
assign tlbelo1 = {tlbelo1_ppn, 1'b0, tlbelo1_g, tlbelo1_mat, tlbelo1_plv, tlbelo1_d, tlbelo1_v};
//ASID
always @ (posedge clk) begin
    if (reset) begin
        asid_asid <= 10'b0;
    end else if (tlbrd_valid) begin
        asid_asid <= tlb_r_e ? tlb_r_asid : 10'b0;
    end else if (csr_we && csr_num == `CSR_ASID) begin
        asid_asid <= csr_wmask[`CSR_ASID_ASID] & csr_wvalue[`CSR_ASID_ASID]
                            | ~csr_wmask[`CSR_ASID_ASID] & asid_asid;
    end
end
assign asid_asidbits = 8'd10;
assign asid = {8'b0, asid_asidbits, 6'b0, asid_asid};
//TLBRENTRY
always @ (posedge clk) begin
    if (reset) begin
        tlbrentry_pa <= 26'b0;
    end else if (csr_we && csr_num == `CSR_TLBRENTRY) begin
        tlbrentry_pa <= csr_wmask[`CSR_TLBRENTRY_PA] & csr_wvalue[`CSR_TLBRENTRY_PA]
                            | ~csr_wmask[`CSR_TLBRENTRY_PA] & tlbrentry_pa;
    end
end

//DMW
always @(posedge clk) begin
    if (reset) begin 
       csr_dmw0_plv0 <= 1'b0;
       csr_dmw0_plv3 <= 1'b0;
    end
    else if (csr_we && csr_num==`CSR_DMW0) begin
        csr_dmw0_plv0 <= csr_wmask[`CSR_DMW_PLV0] & csr_wvalue[`CSR_DMW_PLV0]
                        | ~csr_wmask[`CSR_DMW_PLV0] & csr_dmw0_plv0;
        csr_dmw0_plv3 <= csr_wmask[`CSR_DMW_PLV3] & csr_wvalue[`CSR_DMW_PLV3]
                            | ~csr_wmask[`CSR_DMW_PLV3] & csr_dmw0_plv3;
    end

    if (reset) begin 
       csr_dmw1_plv0 <= 1'b0;
       csr_dmw1_plv3 <= 1'b0;
    end
    else if (csr_we && csr_num==`CSR_DMW1) begin
        csr_dmw1_plv0 <= csr_wmask[`CSR_DMW_PLV0] & csr_wvalue[`CSR_DMW_PLV0]
                        | ~csr_wmask[`CSR_DMW_PLV0] & csr_dmw1_plv0;
        csr_dmw1_plv3 <= csr_wmask[`CSR_DMW_PLV3] & csr_wvalue[`CSR_DMW_PLV3]
                            | ~csr_wmask[`CSR_DMW_PLV3] & csr_dmw1_plv3;
    end     
end

always @(posedge clk) begin
    if (reset) begin 
       csr_dmw0_mat <= 2'b0;
       csr_dmw0_pseg <= 3'b0;
       csr_dmw0_vseg <= 3'b0;
    end
    else if (csr_we && csr_num==`CSR_DMW0) begin
        csr_dmw0_mat <= csr_wmask[`CSR_DMW_MAT] & csr_wvalue[`CSR_DMW_MAT]
                        | ~csr_wmask[`CSR_DMW_MAT] & csr_dmw0_mat;
        csr_dmw0_pseg <= csr_wmask[`CSR_DMW_PSEG] & csr_wvalue[`CSR_DMW_PSEG]
                            | ~csr_wmask[`CSR_DMW_PSEG] & csr_dmw0_pseg;
        csr_dmw0_vseg <= csr_wmask[`CSR_DMW_VSEG] & csr_wvalue[`CSR_DMW_VSEG]
                            | ~csr_wmask[`CSR_DMW_VSEG] & csr_dmw0_vseg;
    end

    if (reset) begin 
       csr_dmw1_mat <= 2'b0;
       csr_dmw1_pseg <= 3'b0;
       csr_dmw1_vseg <= 3'b0;
    end
    else if (csr_we && csr_num==`CSR_DMW1) begin
        csr_dmw1_mat <= csr_wmask[`CSR_DMW_MAT] & csr_wvalue[`CSR_DMW_MAT]
                        | ~csr_wmask[`CSR_DMW_MAT] & csr_dmw1_mat;
        csr_dmw1_pseg <= csr_wmask[`CSR_DMW_PSEG] & csr_wvalue[`CSR_DMW_PSEG]
                            | ~csr_wmask[`CSR_DMW_PSEG] & csr_dmw1_pseg;
        csr_dmw1_vseg <= csr_wmask[`CSR_DMW_VSEG] & csr_wvalue[`CSR_DMW_VSEG]
                            | ~csr_wmask[`CSR_DMW_VSEG] & csr_dmw1_vseg;
    end     
end


assign tlbrentry = {tlbrentry_pa, 6'b0};

assign tlb_w_e    = ~tlbidx_ne;
assign tlb_w_ps   = tlbidx_ps;
assign tlb_w_vppn = tlbehi_vppn;
assign tlb_w_asid = asid_asid;
assign tlb_w_g    = tlbelo0_g & tlbelo1_g;
assign tlb_w_ppn0 = tlbelo0_ppn[19:0];
assign tlb_w_plv0 = tlbelo0_plv;
assign tlb_w_mat0 = tlbelo0_mat;
assign tlb_w_d0   = tlbelo0_d;
assign tlb_w_v0   = tlbelo0_v;
assign tlb_w_ppn1 = tlbelo1_ppn[19:0];
assign tlb_w_plv1 = tlbelo1_plv;
assign tlb_w_mat1 = tlbelo1_mat;
assign tlb_w_d1   = tlbelo1_d;
assign tlb_w_v1   = tlbelo1_v;

wire [31:0] csr_dwm0_value = {csr_dmw0_vseg, 1'b0, csr_dmw0_pseg,19'b0,csr_dmw0_mat,csr_dmw0_plv3,2'b0,csr_dmw0_plv0};
wire [31:0] csr_dwm1_value = {csr_dmw1_vseg, 1'b0, csr_dmw1_pseg,19'b0,csr_dmw1_mat,csr_dmw1_plv3,2'b0,csr_dmw1_plv0};

assign tlb_rentry = tlbrentry;
assign da = csr_crmd_da;
assign pg = csr_crmd_pg;

assign crmd_plv = csr_crmd_plv;
assign dmw0_plv = {csr_dmw0_plv3,2'b0,csr_dmw0_plv0};
assign dmw0_pseg = csr_dmw0_pseg;
assign dmw0_vseg = csr_dmw0_vseg;
assign dmw1_plv = {csr_dmw1_plv3,2'b0,csr_dmw1_plv0};
assign dmw1_pseg = csr_dmw1_pseg;
assign dmw1_vseg = csr_dmw1_vseg;

assign csr_rvalue = {32{csr_num == `CSR_CRMD}} & csr_crmd_rvalue
                    | {32{csr_num == `CSR_PRMD}} & csr_prmd_rvalue
                    | {32{csr_num == `CSR_ECFG}} & csr_ecfg_rvalue
                    | {32{csr_num == `CSR_ESTAT}} & csr_estat_rvalue
                    | {32{csr_num == `CSR_ERA}} & csr_era_rvalue
                    | {32{csr_num == `CSR_BADV}} & csr_badv_rvalue
                    | {32{csr_num == `CSR_EENTRY}} & csr_eentry_rvalue
                    | {32{csr_num == `CSR_SAVE0}} & csr_save0_rvalue
                    | {32{csr_num == `CSR_SAVE1}} & csr_save1_rvalue
                    | {32{csr_num == `CSR_SAVE2}} & csr_save2_rvalue
                    | {32{csr_num == `CSR_SAVE3}} & csr_save3_rvalue
                    | {32{csr_num == `CSR_TID}} & csr_tid_rvalue
                    | {32{csr_num == `CSR_TCFG}} & csr_tcfg_rvalue
                    | {32{csr_num == `CSR_TVAL}} & csr_tval_rvalue
                    | {32{csr_num == `CSR_TICLR}} & csr_ticlr_rvalue
                    | {32{csr_num == `CSR_TLBIDX}} & tlbidx
                    | {32{csr_num == `CSR_TLBEHI}} & tlbehi
                    | {32{csr_num == `CSR_TLBELO0}} & tlbelo0
                    | {32{csr_num == `CSR_TLBELO1}} & tlbelo1
                    | {32{csr_num == `CSR_ASID}} & asid
                    | {32{csr_num == `CSR_TLBRENTRY}} & tlbrentry
                    | {32{csr_num == `CSR_DMW0}} & csr_dwm0_value
                    | {32{csr_num == `CSR_DMW1}} & csr_dwm1_value;

endmodule