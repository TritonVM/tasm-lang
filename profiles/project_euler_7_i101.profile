project_euler_7_i101:

# aggregated unsorted
 main:, 64890, 1;
   tasm_arithmetic_u32_leadingzeros:, 12, 0.00018492834;
     tasm_arithmetic_u32_leadingzeros_non_zero:, 4, 0.00006164278;
   tasm_arithmetic_u32_safesub:, 22, 0.0003390353;
   tasm_arithmetic_u32_safemul:, 8, 0.00012328556;
   tasm_list_new___bool:, 27, 0.00041608876;
     tasm_memory_dyn_malloc:, 20, 0.0003082139;
       tasm_memory_dyn_malloc_initialize:, 3, 0.000046232086;
   tasm_list_push___bool:, 36, 0.000554785;
   _binop_Lt__LboolR_bool_18_while_loop:, 25376, 0.39106178;
     tasm_list_push___bool:, 10872, 0.16754508;
     tasm_arithmetic_u32_safeadd:, 7016, 0.10812144;
   _binop_Lt__LboolR_bool_29_while_loop:, 39373, 0.6067653;
     _primes_cast_u32__LboolR_bool_33_then:, 29637, 0.45672676;
       tasm_arithmetic_u32_safeadd:, 800, 0.012328556;
       tasm_arithmetic_u32_safemul:, 800, 0.012328556;
       _binop_Lt__LboolR_bool_43_while_loop:, 26437, 0.40741256;
         tasm_arithmetic_u32_safeadd:, 6616, 0.10195716;
     _primes_cast_u32__LboolR_bool_33_else:, 173, 0.0026660503;
   tasm_io_write_to_stdout___u32:, 4, 0.00006164278;
 total:, 64892, 1.0000309;

# aggregated + sorted
       tasm_memory_dyn_malloc_initialize:, 3, 0.000046232086;
     tasm_arithmetic_u32_leadingzeros_non_zero:, 4, 0.00006164278;
   tasm_io_write_to_stdout___u32:, 4, 0.00006164278;
   tasm_arithmetic_u32_safemul:, 8, 0.00012328556;
   tasm_arithmetic_u32_leadingzeros:, 12, 0.00018492834;
     tasm_memory_dyn_malloc:, 20, 0.0003082139;
   tasm_arithmetic_u32_safesub:, 22, 0.0003390353;
   tasm_list_new___bool:, 27, 0.00041608876;
   tasm_list_push___bool:, 36, 0.000554785;
     _primes_cast_u32__LboolR_bool_33_else:, 173, 0.0026660503;
       tasm_arithmetic_u32_safeadd:, 800, 0.012328556;
       tasm_arithmetic_u32_safemul:, 800, 0.012328556;
         tasm_arithmetic_u32_safeadd:, 6616, 0.10195716;
     tasm_arithmetic_u32_safeadd:, 7016, 0.10812144;
     tasm_list_push___bool:, 10872, 0.16754508;
   _binop_Lt__LboolR_bool_18_while_loop:, 25376, 0.39106178;
       _binop_Lt__LboolR_bool_43_while_loop:, 26437, 0.40741256;
     _primes_cast_u32__LboolR_bool_33_then:, 29637, 0.45672676;
   _binop_Lt__LboolR_bool_29_while_loop:, 39373, 0.6067653;
 main:, 64890, 1;
 total:, 64892, 1.0000309;
