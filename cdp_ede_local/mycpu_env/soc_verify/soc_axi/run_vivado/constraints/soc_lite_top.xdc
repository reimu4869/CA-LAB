#set_property SEVERITY {Warning} [get_drc_checks RTSTAT-2]
#时钟信号连接
set_property PACKAGE_PIN AC19 [get_ports clk]
set_property CLOCK_DEDICATED_ROUTE BACKBONE [get_nets clk]
create_clock -period 10.000 -name clk -waveform {0.000 5.000} [get_ports clk]

#reset
set_property PACKAGE_PIN Y3 [get_ports resetn]


#LED
set_property PACKAGE_PIN K23 [get_ports {led[0]}]
set_property PACKAGE_PIN J21 [get_ports {led[1]}]
set_property PACKAGE_PIN H23 [get_ports {led[2]}]
set_property PACKAGE_PIN J19 [get_ports {led[3]}]
set_property PACKAGE_PIN G9 [get_ports {led[4]}]
set_property PACKAGE_PIN J26 [get_ports {led[5]}]
set_property PACKAGE_PIN J23 [get_ports {led[6]}]
set_property PACKAGE_PIN J8 [get_ports {led[7]}]
set_property PACKAGE_PIN H8 [get_ports {led[8]}]
set_property PACKAGE_PIN G8 [get_ports {led[9]}]
set_property PACKAGE_PIN F7 [get_ports {led[10]}]
set_property PACKAGE_PIN A4 [get_ports {led[11]}]
set_property PACKAGE_PIN A5 [get_ports {led[12]}]
set_property PACKAGE_PIN A3 [get_ports {led[13]}]
set_property PACKAGE_PIN D5 [get_ports {led[14]}]
set_property PACKAGE_PIN H7 [get_ports {led[15]}]

#led_rg 0/1
set_property PACKAGE_PIN G7 [get_ports {led_rg0[0]}]
set_property PACKAGE_PIN F8 [get_ports {led_rg0[1]}]
set_property PACKAGE_PIN B5 [get_ports {led_rg1[0]}]
set_property PACKAGE_PIN D6 [get_ports {led_rg1[1]}]

#NUM
set_property PACKAGE_PIN D3  [get_ports {num_csn[7]}]
set_property PACKAGE_PIN D25 [get_ports {num_csn[6]}]
set_property PACKAGE_PIN D26 [get_ports {num_csn[5]}]
set_property PACKAGE_PIN E25 [get_ports {num_csn[4]}]
set_property PACKAGE_PIN E26 [get_ports {num_csn[3]}]
set_property PACKAGE_PIN G25 [get_ports {num_csn[2]}]
set_property PACKAGE_PIN G26 [get_ports {num_csn[1]}]
set_property PACKAGE_PIN H26 [get_ports {num_csn[0]}]

set_property PACKAGE_PIN C3 [get_ports {num_a_g[0]}]
set_property PACKAGE_PIN E6 [get_ports {num_a_g[1]}]
set_property PACKAGE_PIN B2 [get_ports {num_a_g[2]}]
set_property PACKAGE_PIN B4 [get_ports {num_a_g[3]}]
set_property PACKAGE_PIN E5 [get_ports {num_a_g[4]}]
set_property PACKAGE_PIN D4 [get_ports {num_a_g[5]}]
set_property PACKAGE_PIN A2 [get_ports {num_a_g[6]}]
#set_property PACKAGE_PIN C4 :DP

#num_data
set_property PACKAGE_PIN J16  [get_ports {num_data[0]}]
set_property PACKAGE_PIN M17  [get_ports {num_data[1]}]
set_property PACKAGE_PIN F15  [get_ports {num_data[2]}]
set_property PACKAGE_PIN M22  [get_ports {num_data[3]}]
set_property PACKAGE_PIN G17  [get_ports {num_data[4]}]
set_property PACKAGE_PIN K18  [get_ports {num_data[5]}]
set_property PACKAGE_PIN N22  [get_ports {num_data[6]}]
set_property PACKAGE_PIN B21 [get_ports {num_data[7]}]
set_property PACKAGE_PIN G15 [get_ports {num_data[8]}]
set_property PACKAGE_PIN E21 [get_ports {num_data[9]}]
set_property PACKAGE_PIN G16 [get_ports {num_data[10]}]
set_property PACKAGE_PIN D21 [get_ports {num_data[11]}]
set_property PACKAGE_PIN K17 [get_ports {num_data[12]}]
set_property PACKAGE_PIN E22 [get_ports {num_data[13]}]
set_property PACKAGE_PIN H19 [get_ports {num_data[14]}]
set_property PACKAGE_PIN H18 [get_ports {num_data[15]}]
set_property PACKAGE_PIN J20 [get_ports {num_data[16]}]
set_property PACKAGE_PIN G21 [get_ports {num_data[17]}]
set_property PACKAGE_PIN M21 [get_ports {num_data[18]}]
set_property PACKAGE_PIN G22 [get_ports {num_data[19]}]
set_property PACKAGE_PIN H17 [get_ports {num_data[20]}]
set_property PACKAGE_PIN G20 [get_ports {num_data[21]}]
set_property PACKAGE_PIN L19 [get_ports {num_data[22]}]
set_property PACKAGE_PIN M20 [get_ports {num_data[23]}]
set_property PACKAGE_PIN J15  [get_ports {num_data[24]}]
set_property PACKAGE_PIN H22  [get_ports {num_data[25]}]
set_property PACKAGE_PIN H15  [get_ports {num_data[26]}]
set_property PACKAGE_PIN L20  [get_ports {num_data[27]}]
set_property PACKAGE_PIN H14  [get_ports {num_data[28]}]
set_property PACKAGE_PIN K21  [get_ports {num_data[29]}]
set_property PACKAGE_PIN J14  [get_ports {num_data[30]}]
set_property PACKAGE_PIN K22  [get_ports {num_data[31]}]

#switch
set_property PACKAGE_PIN AC21 [get_ports {switch[7]}]
set_property PACKAGE_PIN AD24 [get_ports {switch[6]}]
set_property PACKAGE_PIN AC22 [get_ports {switch[5]}]
set_property PACKAGE_PIN AC23 [get_ports {switch[4]}]
set_property PACKAGE_PIN AB6  [get_ports {switch[3]}]
set_property PACKAGE_PIN W6   [get_ports {switch[2]}]
set_property PACKAGE_PIN AA7  [get_ports {switch[1]}]
set_property PACKAGE_PIN Y6   [get_ports {switch[0]}]

#btn_key
set_property PACKAGE_PIN V8  [get_ports {btn_key_col[0]}]
set_property PACKAGE_PIN V9  [get_ports {btn_key_col[1]}]
set_property PACKAGE_PIN Y8  [get_ports {btn_key_col[2]}]
set_property PACKAGE_PIN V7  [get_ports {btn_key_col[3]}]
set_property PACKAGE_PIN U7  [get_ports {btn_key_row[0]}]
set_property PACKAGE_PIN W8  [get_ports {btn_key_row[1]}]
set_property PACKAGE_PIN Y7  [get_ports {btn_key_row[2]}]
set_property PACKAGE_PIN AA8 [get_ports {btn_key_row[3]}]

#btn_step
set_property PACKAGE_PIN Y5 [get_ports {btn_step[0]}]
set_property PACKAGE_PIN V6 [get_ports {btn_step[1]}]

set_property IOSTANDARD LVCMOS33 [get_ports clk]
set_property IOSTANDARD LVCMOS33 [get_ports resetn]
set_property IOSTANDARD LVCMOS33 [get_ports {led[*]}]
set_property IOSTANDARD LVCMOS33 [get_ports {led_rg0[*]}]
set_property IOSTANDARD LVCMOS33 [get_ports {led_rg1[*]}]
set_property IOSTANDARD LVCMOS33 [get_ports {num_a_g[*]}]
set_property IOSTANDARD LVCMOS33 [get_ports {num_csn[*]}]
set_property IOSTANDARD LVCMOS33 [get_ports {switch[*]}]
set_property IOSTANDARD LVCMOS33 [get_ports {btn_key_col[*]}]
set_property IOSTANDARD LVCMOS33 [get_ports {btn_key_row[*]}]
set_property IOSTANDARD LVCMOS33 [get_ports {btn_step[*]}]
set_property IOSTANDARD LVCMOS33 [get_ports {num_data[*]}]


set_false_path -from [get_clocks -of_objects [get_pins pll.clk_pll/inst/plle2_adv_inst/CLKOUT1]] -to [get_clocks -of_objects [get_pins pll.clk_pll/inst/plle2_adv_inst/CLKOUT0]]
set_false_path -from [get_clocks -of_objects [get_pins pll.clk_pll/inst/plle2_adv_inst/CLKOUT0]] -to [get_clocks -of_objects [get_pins pll.clk_pll/inst/plle2_adv_inst/CLKOUT1]]
