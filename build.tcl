# generic parameters
	set PARALLEL_IO		2
	set MAX_WINDOW_ROWS	5
	set MAX_WINDOW_COLS	5
	set MAX_IMAGE_ROWS	100
	set MAX_IMAGE_COLS	100
	set DATA_WIDTH		8	
	set BLOCK_EXTRA_INPUT	true

# set the project name
set project_name window_buffer

# set the top-level of our HDL source:
set top_level_filename window_buffer
set top_level_entityname DEFAULT

# create and open the project and set project-level properties
   project new "build/${project_name}.ise"
   project set family "Virtex7"
   project set device "xc7vx485t"
   project set package "ffg1157"
   project set speed "-2"
   project set top_level_module_type "HDL"
   project set synthesis_tool "XST (VHDL/Verilog)"
   project set simulator "ISim (VHDL/Verilog)"
   
   # set the generic values
#   project set "Generics, Parameters" "arch=$arch width=$width parallel_inputs=$parallel_inputs add_core_name=\"${add_core_name}\" use_bram=$use_bram FCBT_max_inputs=$FCBT_max_inputs FCBT_obuf_size=$FCBT_obuf_size" -process "Synthesize - XST" 

   project set "Generics, Parameters" "PARALLEL_IO=$PARALLEL_IO MAX_WINDOW_COLS=$MAX_WINDOW_COLS MAX_WINDOW_ROWS=$MAX_WINDOW_ROWS MAX_IMAGE_COLS=$MAX_IMAGE_COLS MAX_IMAGE_ROWS=$MAX_IMAGE_ROWS DATA_WIDTH=$DATA_WIDTH BLOCK_EXTRA_INPUT=$BLOCK_EXTRA_INPUT"  -process "Synthesize - XST" 

# set top_level entity
   project set top "${top_level_entityname}" "{top_level_filename}"
   
# add all the source HDLs
foreach filename [glob -directory ../src *] {
  xfile add $filename
}

# run the entire xst-to-trce flow
process run "Implement Design"

project close
