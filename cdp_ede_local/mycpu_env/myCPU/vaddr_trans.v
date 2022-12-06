module vaddr_trans (
    input   wire    [31:0]  vaddr,
    output  wire    [31:0]  paddr,

    input  wire     [ 9:0]  asid,
    output wire     [18:0]  s_vppn,           
    output wire             s_va_bit12,
    output wire     [ 9:0]  s_asid,

    input   wire            da,
    input   wire            pg,

    input   wire    [ 1:0]  crmd_plv,
    input   wire    [ 2:0]  dmw0_vseg,
    input   wire    [ 2:0]  dmw0_pseg,
    input   wire    [ 3:0]  dmw0_plv,
    input   wire    [ 2:0]  dmw1_vseg,
    input   wire    [ 2:0]  dmw1_pseg,
    input   wire    [ 3:0]  dmw1_plv,

    input   wire            s_found,          
    input   wire    [19:0]  s_ppn,            
    input   wire    [ 5:0]  s_ps,
    input   wire    [ 1:0]  s_plv,
    //input wire    [ 1:0]  s_mat,
    input   wire            s_d,
    input   wire            s_v,

    input   wire    [ 1:0]  mem_type,

    output  wire            mapping_ADEF,
    output  wire            mapping_ADEM,           

    output  wire            tlb_refill,
    output  wire            tlb_PIL,
    output  wire            tlb_PIS,
    output  wire            tlb_PIF,
    output  wire            tlb_PME,     
    output  wire            tlb_PPI
);
//addr trans mode 地址翻译模式
wire    mode_direct;
wire    mode_mapping;

//direct mapping  直接映射翻译相关信号
wire    m_d_fonud;
wire    m_d0_fonud;
wire    m_d1_fonud;

wire    m_d0_plv;
wire    m_d1_plv;


//paddr from three mode 三种翻译地址
wire    [31:0] d_paddr;
wire    [31:0] m_d_paddr;
wire    [31:0] m_d0_paddr;
wire    [31:0] m_d1_paddr;
wire    [31:0] m_p_paddr;


wire   plv_wrong;

//tlb 异常判断
wire    refill;
wire    PIL ;
wire    PIS ;
wire    PIF ;
wire    PPI ;
wire    PME ;

wire    tlb_ex;

//地址翻译模式
assign  mode_direct = da & ~pg;
assign  mode_mapping = pg & ~da;

//直接映射特权等级判断
assign  m_d0_plv = crmd_plv ? dmw0_plv[crmd_plv] : (dmw0_plv[crmd_plv] | dmw0_plv[3]);
assign  m_d1_plv = crmd_plv ? dmw1_plv[crmd_plv] : (dmw1_plv[crmd_plv] | dmw1_plv[3]);
//直接映射匹配判断
assign  m_d0_fonud = (dmw0_vseg == vaddr[31:29]) && m_d0_plv;
assign  m_d1_fonud = (dmw1_vseg == vaddr[31:29]) && m_d1_plv;
assign  m_d_fonud = m_d0_fonud | m_d1_fonud;

//三类地址计算
assign  d_paddr = vaddr;
assign  m_d0_paddr = {dmw0_pseg,vaddr[28:0]};
assign  m_d1_paddr = {dmw1_pseg,vaddr[28:0]};
assign  m_d_paddr = m_d0_fonud ? m_d0_paddr :
                    m_d1_fonud ? m_d1_paddr :
                    32'b0;
assign  m_p_paddr = (s_ps == 6'b010110) ? {s_ppn,vaddr[11:0]} : {s_ppn[19:9],vaddr[20:0]};

//tlb异常判断
assign plv_wrong = crmd_plv > s_plv;

assign refill = ~s_found;
assign PIL    = (mem_type == 2'b00) && (s_found & ~s_v);
assign PIS    = (mem_type == 2'b01) && (s_found & ~s_v);
assign PIF    = (mem_type == 2'b10) && (s_found & ~s_v);
assign PPI    = (s_found & s_v) && plv_wrong;
assign PME    = (s_found & s_v) && ~plv_wrong && (mem_type == 2'b01) && ~s_d;

assign tlb_refill = mode_mapping && ~m_d_fonud && refill;
assign tlb_PIL = mode_mapping && ~m_d_fonud && PIL;
assign tlb_PIS = mode_mapping && ~m_d_fonud && PIS;
assign tlb_PIF = mode_mapping && ~m_d_fonud && PIF;
assign tlb_PPI = mode_mapping && ~m_d_fonud && PPI;
assign tlb_PME = mode_mapping && ~m_d_fonud && PME;

assign tlb_ex = tlb_refill | tlb_PPI | tlb_PME | tlb_PIS | tlb_PIL | tlb_PIF;

//映射模式下的ADE异常判断
assign mapping_ADEF = mode_mapping && ~m_d_fonud && (crmd_plv == 2'b11) && vaddr[31];
assign mapping_ADEM = mode_mapping && ~m_d_fonud && (crmd_plv == 2'b11) && vaddr[31];

//向TLB中传入的虚地址信息
assign s_vppn = vaddr[31:13];
assign s_va_bit12 = vaddr[12];
assign s_asid = asid;

//实地址选择
assign paddr =  mode_direct                ?  d_paddr   :
                mode_mapping && m_d_fonud  ?  m_d_paddr :
                mode_mapping && ~tlb_ex    ?  m_p_paddr :
                32'b0;


endmodule 