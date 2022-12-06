`define CSR_CRMD    14'h0000
`define CSR_PRMD    14'h0001
`define CSR_ECFG    14'h0004
`define CSR_ESTAT   14'h0005
`define CSR_ERA     14'h0006
`define CSR_BADV    14'h0007
`define CSR_EENTRY  14'h000c
`define CSR_SAVE0   14'h0030
`define CSR_SAVE1   14'h0031
`define CSR_SAVE2   14'h0032
`define CSR_SAVE3   14'h0033
`define CSR_TID     14'h0040
`define CSR_TCFG    14'h0041
`define CSR_TVAL    14'h0042
`define CSR_TICLR   14'h0044
//exp18
`define CSR_TLBIDX    14'h0010
`define CSR_TLBEHI    14'h0011
`define CSR_TLBELO0   14'h0012
`define CSR_TLBELO1   14'h0013
`define CSR_ASID      14'h0018
`define CSR_TLBRENTRY 14'h0088
//exp19
`define CSR_DMW0      14'h0180
`define CSR_DMW1      14'h0181 

//部分宏定义大概可以合并

`define CSR_CRMD_PLV        1:0
`define CSR_CRMD_IE         2
//exp19
`define CSR_CRMD_DA         3
`define CSR_CRMD_PG         4
`define CSR_CRMD_DATF       6:5
`define CSR_CRMD_DATM       8:7

`define CSR_PRMD_PPLV       1:0
`define CSR_PRMD_PIE        2

`define CSR_ECFG_LIE        13:0

`define CSR_ESTAT_IS10      1:0

//exp19 add
`define ECODE_PIL           6'h01
`define ECODE_PIS           6'h02
`define ECODE_PIF           6'h03
`define ECODE_PME           6'h04
`define ECODE_ADE           6'h08
`define ECODE_PPI           6'h07
`define ECODE_ALE           6'h09
`define ECODE_TLBR          6'h3f
`define ESUBCODE_ADEF       9'h000
`define ESUBCODE_ADEM       9'h001

`define CSR_ERA_PC          31:0

`define CSR_EENTRY_VA       31:6

`define CSR_SAVE_DATA       31:0

`define CSR_TID_TID         31:0

`define CSR_TICLR_CLR       0

`define CSR_TCFG_EN         0
`define CSR_TCFG_PERIOD     1
`define CSR_TCFG_INITVAL    29:0

//added in exp18
//TLBIDX
`define CSR_TLBIDX_IDX      3:0
`define CSR_TLBIDX_PS       29:24
`define CSR_TLBIDX_NE       31
//TLBEHI
`define CSR_TLBEHI_VPPN     31:13
//TLBELO
`define CSR_TLBELO_V        0
`define CSR_TLBELO_D        1
`define CSR_TLBELO_PLV      3:2
`define CSR_TLBELO_MAT      5:4
`define CSR_TLBELO_G        6
`define CSR_TLBELO_PPN      31:8
//ASID
`define CSR_ASID_ASID       9:0
//TLBRENTRY
`define CSR_TLBRENTRY_PA    31:6
//DMW
`define CSR_DMW_PLV0        0
`define CSR_DMW_PLV3        3
`define CSR_DMW_MAT         5:4
`define CSR_DMW_PSEG        27:25
`define CSR_DMW_VSEG        31:29