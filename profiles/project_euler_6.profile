project_euler_6:

# call graph
 main: 259
   tasm_arithmetic_u32_safepow: 78
     tasm_arithmetic_u32_safepow_while_acc: 68
       tasm_arithmetic_u32_safepow_mul_acc_with_bpow2: 8
   tasm_arithmetic_u32_safeadd: 8
   tasm_arithmetic_u32_safepow: 78
     tasm_arithmetic_u32_safepow_while_acc: 68
       tasm_arithmetic_u32_safepow_mul_acc_with_bpow2: 8
   tasm_arithmetic_u32_safemul: 8
   tasm_arithmetic_u32_safeadd: 8
   tasm_arithmetic_u32_safemul: 8
   tasm_arithmetic_u32_safemul: 8
   tasm_arithmetic_u32_safeadd: 8
   tasm_arithmetic_u32_safemul: 8
   tasm_arithmetic_u32_safesub: 11
   tasm_io_write_to_stdout___u32: 2
 total: 265

# aggregated
 main: 259
   tasm_arithmetic_u32_safepow: 156
     tasm_arithmetic_u32_safepow_while_acc: 136
       tasm_arithmetic_u32_safepow_mul_acc_with_bpow2: 16
   tasm_arithmetic_u32_safeadd: 24
   tasm_arithmetic_u32_safemul: 32
   tasm_arithmetic_u32_safesub: 11
   tasm_io_write_to_stdout___u32: 2
 total: 265