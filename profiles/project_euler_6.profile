project_euler_6:
| Subroutine                                                    |            Processor |             Op Stack |                  RAM |                 Hash |                  U32 |
|:--------------------------------------------------------------|---------------------:|---------------------:|---------------------:|---------------------:|---------------------:|
| main                                                          |         255 ( 99.2%) |         186 (100.0%) |           0 (  NaN%) |           0 (  0.0%) |         277 (100.0%) |
| ··tasmlib_arithmetic_u32_safepow                              |         152 ( 59.1%) |         110 ( 59.1%) |           0 (  NaN%) |           0 (  0.0%) |         102 ( 36.8%) |
| ····tasmlib_arithmetic_u32_safepow_while_acc                  |         136 ( 52.9%) |         100 ( 53.8%) |           0 (  NaN%) |           0 (  0.0%) |         102 ( 36.8%) |
| ······tasmlib_arithmetic_u32_safepow_mul_acc_with_bpow2       |          16 (  6.2%) |          12 (  6.5%) |           0 (  NaN%) |           0 (  0.0%) |           0 (  0.0%) |
| ··tasmlib_arithmetic_u32_safeadd                              |          24 (  9.3%) |          21 ( 11.3%) |           0 (  NaN%) |           0 (  0.0%) |          17 (  6.1%) |
| ··tasmlib_arithmetic_u32_safemul                              |          32 ( 12.5%) |          28 ( 15.1%) |           0 (  NaN%) |           0 (  0.0%) |          74 ( 26.7%) |
| ··tasmlib_arithmetic_u32_safesub                              |          11 (  4.3%) |           9 (  4.8%) |           0 (  NaN%) |           0 (  0.0%) |          26 (  9.4%) |
| ··tasmlib_io_write_to_stdout___u32                            |           2 (  0.8%) |           1 (  0.5%) |           0 (  NaN%) |           0 (  0.0%) |           0 (  0.0%) |
| Total                                                         |         257 (100.0%) |         186 (100.0%) |           0 (  NaN%) |         114 (100.0%) |         277 (100.0%) |
