project_euler_5:

# call graph
 main: 260
   tasm_arithmetic_u32_safepow: 103
     tasm_arithmetic_u32_safepow_while_acc: 95
       tasm_arithmetic_u32_safepow_mul_acc_with_bpow2: 8
   tasm_arithmetic_u32_safepow: 76
     tasm_arithmetic_u32_safepow_while_acc: 68
       tasm_arithmetic_u32_safepow_mul_acc_with_bpow2: 8
   tasm_arithmetic_u32_safemul: 8
   tasm_arithmetic_u32_safemul: 8
   tasm_arithmetic_u32_safemul: 8
   tasm_arithmetic_u32_safemul: 8
   tasm_arithmetic_u32_safemul: 8
   tasm_arithmetic_u32_safemul: 8
   tasm_arithmetic_u32_safemul: 8
   tasm_io_write_to_stdout___u32: 2
 total: 262

# aggregated
 main: 260
   tasm_arithmetic_u32_safepow: 179
     tasm_arithmetic_u32_safepow_while_acc: 163
       tasm_arithmetic_u32_safepow_mul_acc_with_bpow2: 16
   tasm_arithmetic_u32_safemul: 56
   tasm_io_write_to_stdout___u32: 2
 total: 262
