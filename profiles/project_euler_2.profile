project_euler_2:
| Subroutine                                  |            Processor |              OpStack |                  Ram |                 Hash |                  U32 |
|:--------------------------------------------|---------------------:|---------------------:|---------------------:|---------------------:|---------------------:|
| main                                        |        1496 ( 99.9%) |        1108 (100.0%) |           0 (  NaN%) |           0 (  0.0%) |        1732 (100.0%) |
| ··_unaryop_not__LboolR_bool_6_while_loop    |        1486 ( 99.2%) |        1100 ( 99.3%) |           0 (  NaN%) |           0 (  0.0%) |        1732 (100.0%) |
| ····_binop_Eq__LboolR_bool_11_else          |          21 (  1.4%) |           0 (  0.0%) |           0 (  NaN%) |           0 (  0.0%) |           0 (  0.0%) |
| ····tasmlib_arithmetic_u32_safe_add         |         256 ( 17.1%) |         224 ( 20.2%) |           0 (  NaN%) |           0 (  0.0%) |         423 ( 24.4%) |
| ····_binop_Eq__LboolR_bool_11_then          |         176 ( 11.7%) |         132 ( 11.9%) |           0 (  NaN%) |           0 (  0.0%) |         142 (  8.2%) |
| ······tasmlib_arithmetic_u32_safe_add       |          88 (  5.9%) |          77 (  6.9%) |           0 (  NaN%) |           0 (  0.0%) |         142 (  8.2%) |
| ··tasmlib_io_write_to_stdout___u32          |           2 (  0.1%) |           1 (  0.1%) |           0 (  NaN%) |           0 (  0.0%) |           0 (  0.0%) |
| Total                                       |        1498 (100.0%) |        1108 (100.0%) |           0 (  NaN%) |          66 (100.0%) |        1732 (100.0%) |

| Table     | Height | Dominates |
|:----------|-------:|----------:|
| Program   |    110 |        no |
| Processor |   1498 |        no |
| OpStack   |   1108 |        no |
| Ram       |      0 |        no |
| JumpStack |   1498 |        no |
| Hash      |     66 |        no |
| Cascade   |    726 |        no |
| Lookup    |    256 |        no |
| U32       |   1732 |       yes |

Padded height: 2^11
