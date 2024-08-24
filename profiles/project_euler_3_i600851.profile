project_euler_3_i600851:
| Subroutine                                              |            Processor |             Op Stack |                  RAM |                 Hash |                  U32 |
|:--------------------------------------------------------|---------------------:|---------------------:|---------------------:|---------------------:|---------------------:|
| main                                                    |       48053 (100.0%) |       35116 (100.0%) |        1728 (100.0%) |           0 (  0.0%) |       15918 (100.0%) |
| ··_unaryop_not__LboolR_bool_7_while_loop                |       48041 (100.0%) |       35104 (100.0%) |        1728 (100.0%) |           0 (  0.0%) |       15918 (100.0%) |
| ····tasmlib_arithmetic_u64_safe_mul                     |        5616 ( 11.7%) |        3744 ( 10.7%) |           0 (  0.0%) |           0 (  0.0%) |        1880 ( 11.8%) |
| ····tasmlib_arithmetic_u64_lt                           |        1872 (  3.9%) |        1008 (  2.9%) |           0 (  0.0%) |           0 (  0.0%) |        2445 ( 15.4%) |
| ····tasmlib_arithmetic_u64_div_mod                      |       32318 ( 67.3%) |       24596 ( 70.0%) |        1716 ( 99.3%) |           0 (  0.0%) |       10625 ( 66.7%) |
| ······tasmlib_arithmetic_u64_shift_right                |        7722 ( 16.1%) |        6006 ( 17.1%) |           0 (  0.0%) |           0 (  0.0%) |          56 (  0.4%) |
| ······tasmlib_arithmetic_u64_and                        |        1716 (  3.6%) |         572 (  1.6%) |           0 (  0.0%) |           0 (  0.0%) |        4753 ( 29.9%) |
| ······tasmlib_arithmetic_u64_lt                         |        1859 (  3.9%) |        1001 (  2.9%) |           0 (  0.0%) |           0 (  0.0%) |        2344 ( 14.7%) |
| ······_binop_Gt_bool_bool_26_else                       |       12870 ( 26.8%) |        9438 ( 26.9%) |         572 ( 33.1%) |           0 (  0.0%) |        3472 ( 21.8%) |
| ········_binop_Or_bool_bool_44_then                     |        7722 ( 16.1%) |        4862 ( 13.8%) |         286 ( 16.6%) |           0 (  0.0%) |        3472 ( 21.8%) |
| ··········_binop_Eq_bool_bool_47_else                   |        5148 ( 10.7%) |        2860 (  8.1%) |           0 (  0.0%) |           0 (  0.0%) |        3472 ( 21.8%) |
| ············_binop_Eq_bool_bool_53_then                 |        3861 (  8.0%) |        2002 (  5.7%) |           0 (  0.0%) |           0 (  0.0%) |        3472 ( 21.8%) |
| ····_binop_Eq__LboolR_bool_12_else                      |        3550 (  7.4%) |        1988 (  5.7%) |           0 (  0.0%) |           0 (  0.0%) |         968 (  6.1%) |
| ······tasmlib_arithmetic_u64_add                        |        2130 (  4.4%) |        1136 (  3.2%) |           0 (  0.0%) |           0 (  0.0%) |         968 (  6.1%) |
| ····_binop_Eq__LboolR_bool_12_then                      |         239 (  0.5%) |         182 (  0.5%) |          12 (  0.7%) |           0 (  0.0%) |           0 (  0.0%) |
| ······tasmlib_arithmetic_u64_div_mod                    |         226 (  0.5%) |         172 (  0.5%) |          12 (  0.7%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_shift_right              |          54 (  0.1%) |          42 (  0.1%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_and                      |          12 (  0.0%) |           4 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_lt                       |          13 (  0.0%) |           7 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········_binop_Gt_bool_bool_26_else                     |          90 (  0.2%) |          66 (  0.2%) |           4 (  0.2%) |           0 (  0.0%) |           0 (  0.0%) |
| ··········_binop_Or_bool_bool_44_then                   |          54 (  0.1%) |          34 (  0.1%) |           2 (  0.1%) |           0 (  0.0%) |           0 (  0.0%) |
| ············_binop_Eq_bool_bool_47_else                 |          36 (  0.1%) |          20 (  0.1%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··············_binop_Eq_bool_bool_53_then               |          27 (  0.1%) |          14 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··tasmlib_io_write_to_stdout___u64                      |           3 (  0.0%) |           2 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| Total                                                   |       48054 (100.0%) |       35116 (100.0%) |        1728 (100.0%) |         654 (100.0%) |       15918 (100.0%) |
