project_euler_4_i10_to_50:
| Subroutine                                            |            Processor |             Op Stack |                  RAM |                 Hash |                  U32 |
|:------------------------------------------------------|---------------------:|---------------------:|---------------------:|---------------------:|---------------------:|
| main                                                  |      304383 (100.0%) |      229374 (100.0%) |       12093 (100.0%) |           0 (  0.0%) |       24612 (100.0%) |
| ··tasmlib_list_new                                    |          31 (  0.0%) |          23 (  0.0%) |           3 (  0.0%) |           0 (  0.0%) |          32 (  0.1%) |
| ····tasmlib_memory_dyn_malloc                         |          25 (  0.0%) |          21 (  0.0%) |           2 (  0.0%) |           0 (  0.0%) |          32 (  0.1%) |
| ······tasmlib_memory_dyn_malloc_initialize            |           4 (  0.0%) |           2 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··_binop_Neq__LboolR_bool_49_while_loop               |      304341 (100.0%) |      229340 (100.0%) |       12090 (100.0%) |           0 (  0.0%) |       24580 ( 99.9%) |
| ····_binop_Neq__LboolR_bool_55_while_loop             |      303371 ( 99.7%) |      228532 ( 99.6%) |       12090 (100.0%) |           0 (  0.0%) |       24580 ( 99.9%) |
| ······tasmlib_arithmetic_u32_safe_add                 |       15120 (  5.0%) |       11760 (  5.1%) |           0 (  0.0%) |           0 (  0.0%) |         254 (  1.0%) |
| ······tasmlib_arithmetic_u32_safe_mul                 |        7380 (  2.4%) |        5740 (  2.5%) |           0 (  0.0%) |           0 (  0.0%) |        6875 ( 27.9%) |
| ······find_decimal_digits                             |      130912 ( 43.0%) |       90008 ( 39.2%) |        9058 ( 74.9%) |           0 (  0.0%) |        8285 ( 33.7%) |
| ········_binop_Neq__LboolR_bool_7_while_loop          |      123532 ( 40.6%) |       83448 ( 36.4%) |        8238 ( 68.1%) |           0 (  0.0%) |        8285 ( 33.7%) |
| ··········tasmlib_list_push___u32                     |       52174 ( 17.1%) |       32952 ( 14.4%) |        8238 ( 68.1%) |           0 (  0.0%) |           6 (  0.0%) |
| ······list_is_palindrome                              |      123209 ( 40.5%) |       98468 ( 42.9%) |        3032 ( 25.1%) |           0 (  0.0%) |         141 (  0.6%) |
| ········_binop_Neq__LboolR_bool_26_while_loop         |      115009 ( 37.8%) |       91908 ( 40.1%) |        2212 ( 18.3%) |           0 (  0.0%) |         141 (  0.6%) |
| ··········tasmlib_arithmetic_u32_safe_sub             |       26544 (  8.7%) |       19908 (  8.7%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··········_binop_Neq__LboolR_bool_37_then             |        7133 (  2.3%) |        4076 (  1.8%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··········tasmlib_arithmetic_u32_safe_add             |        9954 (  3.3%) |        7742 (  3.4%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ······_binop_And__LboolR_bool_68_else                 |        1612 (  0.5%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ····tasmlib_arithmetic_u32_safe_add                   |         360 (  0.1%) |         280 (  0.1%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··········_binop_Neq__LboolR_bool_37_else             |         174 (  0.1%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ······_binop_And__LboolR_bool_68_then                 |          98 (  0.0%) |          56 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··tasmlib_io_write_to_stdout___u32                    |           3 (  0.0%) |           1 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| Total                                                 |      304384 (100.0%) |      229374 (100.0%) |       12093 (100.0%) |         270 (100.0%) |       24612 (100.0%) |
