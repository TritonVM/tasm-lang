dazefield_element_mul:
| Subroutine                                                                 |            Processor |              OpStack |                  Ram |                 Hash |                  U32 |
|:---------------------------------------------------------------------------|---------------------:|---------------------:|---------------------:|---------------------:|---------------------:|
| main                                                                       |        1913 ( 99.9%) |        1386 (100.0%) |          20 (100.0%) |           0 (  0.0%) |         559 (100.0%) |
| ··tasmlib_io_read_stdin___bfe                                              |           4 (  0.2%) |           2 (  0.1%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··new                                                                      |         870 ( 45.4%) |         648 ( 46.8%) |           8 ( 40.0%) |           0 (  0.0%) |         238 ( 42.6%) |
| ····tasmlib_arithmetic_u128_safe_mul                                       |         220 ( 11.5%) |         184 ( 13.3%) |           0 (  0.0%) |           0 (  0.0%) |         103 ( 18.4%) |
| ····montyred                                                               |         921 ( 48.1%) |         666 ( 48.1%) |          12 ( 60.0%) |           0 (  0.0%) |         316 ( 56.5%) |
| ······tasmlib_arithmetic_u128_shift_right                                  |         135 (  7.0%) |          87 (  6.3%) |           0 (  0.0%) |           0 (  0.0%) |          91 ( 16.3%) |
| ········tasmlib_arithmetic_u128_shift_right_shift_amount_gt_32             |          30 (  1.6%) |          18 (  1.3%) |           0 (  0.0%) |           0 (  0.0%) |           7 (  1.3%) |
| ······tasmlib_arithmetic_u64_shift_left                                    |          66 (  3.4%) |          51 (  3.7%) |           0 (  0.0%) |           0 (  0.0%) |          44 (  7.9%) |
| ······tasmlib_arithmetic_u64_overflowing_add                               |          27 (  1.4%) |          15 (  1.1%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ······tasmlib_arithmetic_u64_shift_right                                   |          72 (  3.8%) |          57 (  4.1%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ······tasmlib_arithmetic_u64_wrapping_sub                                  |         171 (  8.9%) |         108 (  7.8%) |           0 (  0.0%) |           0 (  0.0%) |         113 ( 20.2%) |
| ······tasmlib_arithmetic_u64_overflowing_sub                               |          60 (  3.1%) |          39 (  2.8%) |           0 (  0.0%) |           0 (  0.0%) |           2 (  0.4%) |
| ······tasmlib_arithmetic_u64_add                                           |          36 (  1.9%) |          24 (  1.7%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ······tasmlib_arithmetic_u64_safe_mul                                      |         114 (  6.0%) |          78 (  5.6%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··method_DazeFieldElement_mul                                              |         351 ( 18.3%) |         250 ( 18.0%) |           4 ( 20.0%) |           0 (  0.0%) |         288 ( 51.5%) |
| ····tasmlib_arithmetic_u64_mul_two_u64s_to_u128_u64                        |          32 (  1.7%) |          20 (  1.4%) |           0 (  0.0%) |           0 (  0.0%) |         107 ( 19.1%) |
| ··method_DazeFieldElement_valued                                           |         656 ( 34.3%) |         464 ( 33.5%) |           8 ( 40.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ····method_DazeFieldElement_canonical_representation                       |         640 ( 33.4%) |         456 ( 32.9%) |           8 ( 40.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ······montyred                                                             |         614 ( 32.1%) |         444 ( 32.0%) |           8 ( 40.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u128_shift_right                                |          90 (  4.7%) |          58 (  4.2%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··········tasmlib_arithmetic_u128_shift_right_shift_amount_gt_32           |          20 (  1.0%) |          12 (  0.9%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_shift_left                                  |          44 (  2.3%) |          34 (  2.5%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_overflowing_add                             |          18 (  0.9%) |          10 (  0.7%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_shift_right                                 |          48 (  2.5%) |          38 (  2.7%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_wrapping_sub                                |         114 (  6.0%) |          72 (  5.2%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_overflowing_sub                             |          40 (  2.1%) |          26 (  1.9%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_add                                         |          24 (  1.3%) |          16 (  1.2%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_safe_mul                                    |          76 (  4.0%) |          52 (  3.8%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··bfe_new_from_u64                                                         |           5 (  0.3%) |           3 (  0.2%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··tasmlib_io_write_to_stdout___bfe                                         |           2 (  0.1%) |           1 (  0.1%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··tasmlib_io_write_to_stdout___u64                                         |           2 (  0.1%) |           2 (  0.1%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| Total                                                                      |        1915 (100.0%) |        1386 (100.0%) |          20 (100.0%) |         504 (100.0%) |         559 (100.0%) |

| Table     | Height | Dominates |
|:----------|-------:|----------:|
| Program   |    840 |        no |
| Processor |   1915 |        no |
| OpStack   |   1386 |        no |
| Ram       |     20 |        no |
| JumpStack |   1915 |        no |
| Hash      |    504 |        no |
| Cascade   |   5225 |       yes |
| Lookup    |    256 |        no |
| U32       |    559 |        no |

Padded height: 2^13
