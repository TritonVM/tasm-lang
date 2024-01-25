project_euler_6:

# call graph
 main: 255
   tasm_arithmetic_u32_safepow: 76
     tasm_arithmetic_u32_safepow_while_acc: 68
       tasm_arithmetic_u32_safepow_mul_acc_with_bpow2: 8
   tasm_arithmetic_u32_safeadd: 8
   tasm_arithmetic_u32_safepow: 76
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
 total: 257

# aggregated
 main: 255 (1)
   tasm_arithmetic_u32_safepow: 152 (0.59607846)
     tasm_arithmetic_u32_safepow_while_acc: 136 (0.53333336)
       tasm_arithmetic_u32_safepow_mul_acc_with_bpow2: 16 (0.0627451)
   tasm_arithmetic_u32_safeadd: 24 (0.09411765)
   tasm_arithmetic_u32_safemul: 32 (0.1254902)
   tasm_arithmetic_u32_safesub: 11 (0.043137256)
   tasm_io_write_to_stdout___u32: 2 (0.007843138)
 total: 257 (1.0078431)
