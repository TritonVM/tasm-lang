recufier_merkle_root:
| Subroutine                                                                    |            Processor |             Op Stack |                  RAM |                 Hash |                  U32 |
|:------------------------------------------------------------------------------|---------------------:|---------------------:|---------------------:|---------------------:|---------------------:|
| main                                                                          |        2266 ( 99.9%) |        1768 (100.0%) |          81 (100.0%) |          90 ( 40.5%) |          91 (100.0%) |
| ··merkle_root                                                                 |        2247 ( 99.1%) |        1744 ( 98.6%) |          80 ( 98.8%) |          90 ( 40.5%) |          91 (100.0%) |
| ····tasmlib_arithmetic_u32_safeadd                                            |           8 (  0.4%) |           7 (  0.4%) |           0 (  0.0%) |           0 (  0.0%) |           2 (  2.2%) |
| ····_fn_call__LDigestR_Digest_30_else                                         |        2219 ( 97.8%) |        1727 ( 97.7%) |          80 ( 98.8%) |          90 ( 40.5%) |          89 ( 97.8%) |
| ······tasmlib_arithmetic_u32_safesub                                          |          22 (  1.0%) |          18 (  1.0%) |           0 (  0.0%) |           0 (  0.0%) |          11 ( 12.1%) |
| ······merkle_root                                                             |        2142 ( 94.4%) |        1664 ( 94.1%) |          80 ( 98.8%) |          84 ( 37.8%) |          69 ( 75.8%) |
| ········tasmlib_arithmetic_u32_safeadd                                        |          16 (  0.7%) |          14 (  0.8%) |           0 (  0.0%) |           0 (  0.0%) |           5 (  5.5%) |
| ········_fn_call__LDigestR_Digest_30_else                                     |        2086 ( 92.0%) |        1630 ( 92.2%) |          80 ( 98.8%) |          84 ( 37.8%) |          64 ( 70.3%) |
| ··········tasmlib_arithmetic_u32_safesub                                      |          44 (  1.9%) |          36 (  2.0%) |           0 (  0.0%) |           0 (  0.0%) |           9 (  9.9%) |
| ··········merkle_root                                                         |        1932 ( 85.2%) |        1504 ( 85.1%) |          80 ( 98.8%) |          72 ( 32.4%) |          50 ( 54.9%) |
| ············tasmlib_arithmetic_u32_safeadd                                    |          32 (  1.4%) |          28 (  1.6%) |           0 (  0.0%) |           0 (  0.0%) |           9 (  9.9%) |
| ············_fn_call__LDigestR_Digest_30_else                                 |        1820 ( 80.2%) |        1436 ( 81.2%) |          80 ( 98.8%) |          72 ( 32.4%) |          41 ( 45.1%) |
| ··············tasmlib_arithmetic_u32_safesub                                  |          88 (  3.9%) |          72 (  4.1%) |           0 (  0.0%) |           0 (  0.0%) |          17 ( 18.7%) |
| ··············merkle_root                                                     |        1512 ( 66.7%) |        1184 ( 67.0%) |          80 ( 98.8%) |          48 ( 21.6%) |          20 ( 22.0%) |
| ················tasmlib_arithmetic_u32_safeadd                                |          64 (  2.8%) |          56 (  3.2%) |           0 (  0.0%) |           0 (  0.0%) |          17 ( 18.7%) |
| ················_fn_call__LDigestR_Digest_30_else                             |        1288 ( 56.8%) |        1048 ( 59.3%) |          80 ( 98.8%) |          48 ( 21.6%) |           3 (  3.3%) |
| ··················tasmlib_arithmetic_u32_safesub                              |         176 (  7.8%) |         144 (  8.1%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··················merkle_root                                                 |         672 ( 29.6%) |         544 ( 30.8%) |          80 ( 98.8%) |           0 (  0.0%) |           0 (  0.0%) |
| ····················tasmlib_arithmetic_u32_safeadd                            |         128 (  5.6%) |         112 (  6.3%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ····················_leafs_start__LDigestR_Digest_10_then                     |         224 (  9.9%) |         272 ( 15.4%) |          80 ( 98.8%) |           0 (  0.0%) |           0 (  0.0%) |
| ··················tasmlib_arithmetic_u32_safeadd                              |          64 (  2.8%) |          56 (  3.2%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··················hash_pair                                                   |          16 (  0.7%) |          40 (  2.3%) |           0 (  0.0%) |          48 ( 21.6%) |           0 (  0.0%) |
| ··············tasmlib_arithmetic_u32_safeadd                                  |          32 (  1.4%) |          28 (  1.6%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··············hash_pair                                                       |           8 (  0.4%) |          20 (  1.1%) |           0 (  0.0%) |          24 ( 10.8%) |           0 (  0.0%) |
| ··········tasmlib_arithmetic_u32_safeadd                                      |          16 (  0.7%) |          14 (  0.8%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··········hash_pair                                                           |           4 (  0.2%) |          10 (  0.6%) |           0 (  0.0%) |          12 (  5.4%) |           0 (  0.0%) |
| ······tasmlib_arithmetic_u32_safeadd                                          |           8 (  0.4%) |           7 (  0.4%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ······hash_pair                                                               |           2 (  0.1%) |           5 (  0.3%) |           0 (  0.0%) |           6 (  2.7%) |           0 (  0.0%) |
| ··tasmlib_io_write_to_stdout___digest                                         |           2 (  0.1%) |           5 (  0.3%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| Total                                                                         |        2268 (100.0%) |        1768 (100.0%) |          81 (100.0%) |         222 (100.0%) |          91 (100.0%) |
