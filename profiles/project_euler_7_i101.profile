project_euler_7_i101:
| Subroutine                                           |            Processor |             Op Stack |                  RAM |                 Hash |                  U32 |
|:-----------------------------------------------------|---------------------:|---------------------:|---------------------:|---------------------:|---------------------:|
| main                                                 |       64890 (100.0%) |       51594 (100.0%) |        3527 (100.0%) |           0 (  0.0%) |       15220 (100.0%) |
| ··tasmlib_arithmetic_u32_leadingzeros                |          12 (  0.0%) |           8 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |           8 (  0.1%) |
| ····tasmlib_arithmetic_u32_leadingzeros_non_zero     |           4 (  0.0%) |           2 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |           8 (  0.1%) |
| ··tasmlib_arithmetic_u32_safesub                     |          22 (  0.0%) |          18 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |           8 (  0.1%) |
| ··tasmlib_arithmetic_u32_safemul                     |           8 (  0.0%) |           7 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |          11 (  0.1%) |
| ··tasmlib_list_new___bool                            |          27 (  0.0%) |          21 (  0.0%) |           3 (  0.1%) |           0 (  0.0%) |           0 (  0.0%) |
| ····tasmlib_memory_dyn_malloc                        |          20 (  0.0%) |          17 (  0.0%) |           2 (  0.1%) |           0 (  0.0%) |           0 (  0.0%) |
| ······tasmlib_memory_dyn_malloc_initialize           |           3 (  0.0%) |           2 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··tasmlib_list_push___bool                           |          36 (  0.1%) |          28 (  0.1%) |           8 (  0.2%) |           0 (  0.0%) |           0 (  0.0%) |
| ··_binop_Lt__LboolR_bool_18_while_loop               |       25376 ( 39.1%) |       19334 ( 37.5%) |        2416 ( 68.5%) |           0 (  0.0%) |       12284 ( 80.7%) |
| ····tasmlib_list_push___bool                         |       10872 ( 16.8%) |        8456 ( 16.4%) |        2416 ( 68.5%) |           0 (  0.0%) |           0 (  0.0%) |
| ····tasmlib_arithmetic_u32_safeadd                   |        7016 ( 10.8%) |        6139 ( 11.9%) |           0 (  0.0%) |           0 (  0.0%) |        5629 ( 37.0%) |
| ··_binop_Lt__LboolR_bool_29_while_loop               |       39373 ( 60.7%) |       32152 ( 62.3%) |        1100 ( 31.2%) |           0 (  0.0%) |        2909 ( 19.1%) |
| ····_primes_cast_u32__LboolR_bool_33_then            |       29637 ( 45.7%) |       24502 ( 47.5%) |         827 ( 23.4%) |           0 (  0.0%) |        2101 ( 13.8%) |
| ······tasmlib_arithmetic_u32_safeadd                 |         800 (  1.2%) |         700 (  1.4%) |           0 (  0.0%) |           0 (  0.0%) |           3 (  0.0%) |
| ······tasmlib_arithmetic_u32_safemul                 |         800 (  1.2%) |         700 (  1.4%) |           0 (  0.0%) |           0 (  0.0%) |         433 (  2.8%) |
| ······_binop_Lt__LboolR_bool_43_while_loop           |       26437 ( 40.7%) |       22102 ( 42.8%) |         827 ( 23.4%) |           0 (  0.0%) |        1665 ( 10.9%) |
| ········tasmlib_arithmetic_u32_safeadd               |        6616 ( 10.2%) |        5789 ( 11.2%) |           0 (  0.0%) |           0 (  0.0%) |         616 (  4.0%) |
| ····_primes_cast_u32__LboolR_bool_33_else            |         173 (  0.3%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··tasmlib_io_write_to_stdout___u32                   |           4 (  0.0%) |           2 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| Total                                                |       64892 (100.0%) |       51594 (100.0%) |        3527 (100.0%) |         216 (100.0%) |       15220 (100.0%) |
