project_euler_1:
| Subroutine                                 |            Processor |             Op Stack |                  RAM |                 Hash |                  U32 |
|:-------------------------------------------|---------------------:|---------------------:|---------------------:|---------------------:|---------------------:|
| main                                       |       54959 (100.0%) |       39570 (100.0%) |           0 (  NaN%) |           0 (  0.0%) |       48359 (100.0%) |
| ··_binop_Lt__LboolR_bool_4_while_loop      |       54950 (100.0%) |       39564 (100.0%) |           0 (  NaN%) |           0 (  0.0%) |       48359 (100.0%) |
| ····_binop_Or__LboolR_bool_15_else         |         533 (  1.0%) |           0 (  0.0%) |           0 (  NaN%) |           0 (  0.0%) |           0 (  0.0%) |
| ····tasmlib_arithmetic_u32_safeadd         |        7992 ( 14.5%) |        6993 ( 17.7%) |           0 (  NaN%) |           0 (  0.0%) |        9719 ( 20.1%) |
| ····_binop_Or__LboolR_bool_15_then         |        7456 ( 13.6%) |        5592 ( 14.1%) |           0 (  NaN%) |           0 (  0.0%) |        7678 ( 15.9%) |
| ······tasmlib_arithmetic_u32_safeadd       |        3728 (  6.8%) |        3262 (  8.2%) |           0 (  NaN%) |           0 (  0.0%) |        7678 ( 15.9%) |
| ··tasmlib_io_write_to_stdout___u32         |           2 (  0.0%) |           1 (  0.0%) |           0 (  NaN%) |           0 (  0.0%) |           0 (  0.0%) |
| Total                                      |       54961 (100.0%) |       39570 (100.0%) |           0 (  NaN%) |          72 (100.0%) |       48359 (100.0%) |
