project_euler_5:
| Subroutine                                                     |            Processor |              OpStack |                  Ram |                 Hash |                  U32 |
|:---------------------------------------------------------------|---------------------:|---------------------:|---------------------:|---------------------:|---------------------:|
| main                                                           |         260 ( 99.2%) |         192 (100.0%) |           0 (  NaN%) |           0 (  0.0%) |         183 (100.0%) |
| ··tasmlib_arithmetic_u32_safe_pow                              |         179 ( 68.3%) |         130 ( 67.7%) |           0 (  NaN%) |           0 (  0.0%) |          57 ( 31.1%) |
| ····tasmlib_arithmetic_u32_safe_pow_while_acc                  |         163 ( 62.2%) |         120 ( 62.5%) |           0 (  NaN%) |           0 (  0.0%) |          57 ( 31.1%) |
| ······tasmlib_arithmetic_u32_safe_pow_mul_acc_with_bpow2       |          16 (  6.1%) |          12 (  6.2%) |           0 (  NaN%) |           0 (  0.0%) |           0 (  0.0%) |
| ··tasmlib_arithmetic_u32_safe_mul                              |          56 ( 21.4%) |          49 ( 25.5%) |           0 (  NaN%) |           0 (  0.0%) |         126 ( 68.9%) |
| ··tasmlib_io_write_to_stdout___u32                             |           2 (  0.8%) |           1 (  0.5%) |           0 (  NaN%) |           0 (  0.0%) |           0 (  0.0%) |
| Total                                                          |         262 (100.0%) |         192 (100.0%) |           0 (  NaN%) |          84 (100.0%) |         183 (100.0%) |

| Table     | Height | Dominates |
|:----------|-------:|----------:|
| Program   |    140 |        no |
| Processor |    262 |        no |
| OpStack   |    192 |        no |
| Ram       |      0 |        no |
| JumpStack |    262 |        no |
| Hash      |     84 |        no |
| Cascade   |    926 |       yes |
| Lookup    |    256 |        no |
| U32       |    183 |        no |

Padded height: 2^10
