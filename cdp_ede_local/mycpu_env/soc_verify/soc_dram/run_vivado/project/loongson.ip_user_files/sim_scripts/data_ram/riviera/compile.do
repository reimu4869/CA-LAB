vlib work
vlib riviera

vlib riviera/xpm
vlib riviera/dist_mem_gen_v8_0_13
vlib riviera/xil_defaultlib

vmap xpm riviera/xpm
vmap dist_mem_gen_v8_0_13 riviera/dist_mem_gen_v8_0_13
vmap xil_defaultlib riviera/xil_defaultlib

vlog -work xpm  -sv2k12 \
"E:/tool/vivado/Vivado/2019.2/data/ip/xpm/xpm_cdc/hdl/xpm_cdc.sv" \

vcom -work xpm -93 \
"E:/tool/vivado/Vivado/2019.2/data/ip/xpm/xpm_VCOMP.vhd" \

vlog -work dist_mem_gen_v8_0_13  -v2k5 \
"../../../ipstatic/simulation/dist_mem_gen_v8_0.v" \

vlog -work xil_defaultlib  -v2k5 \
"../../../../../../rtl/xilinx_ip/data_ram/sim/data_ram.v" \


vlog -work xil_defaultlib \
"glbl.v"
