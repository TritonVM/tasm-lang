dazefield_element_mul:
| Subroutine                                                                 |            Processor |             Op Stack |                  RAM |                 Hash |                  U32 |
|:---------------------------------------------------------------------------|---------------------:|---------------------:|---------------------:|---------------------:|---------------------:|
| main                                                                       |        1914 ( 99.9%) |        1386 (100.0%) |          20 (100.0%) |           0 (  0.0%) |         559 (100.0%) |
| ··tasmlib_io_read_stdin___bfe                                              |           6 (  0.3%) |           2 (  0.1%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··new                                                                      |         872 ( 45.5%) |         648 ( 46.8%) |           8 ( 40.0%) |           0 (  0.0%) |         238 ( 42.6%) |
| ····tasmlib_arithmetic_u128_safe_mul                                       |         222 ( 11.6%) |         184 ( 13.3%) |           0 (  0.0%) |           0 (  0.0%) |         103 ( 18.4%) |
| ····montyred                                                               |         924 ( 48.3%) |         666 ( 48.1%) |          12 ( 60.0%) |           0 (  0.0%) |         316 ( 56.5%) |
| ······tasmlib_arithmetic_u128_shift_right                                  |         138 (  7.2%) |          87 (  6.3%) |           0 (  0.0%) |           0 (  0.0%) |          91 ( 16.3%) |
| ········tasmlib_arithmetic_u128_shift_right_shift_amount_gt_32             |          33 (  1.7%) |          18 (  1.3%) |           0 (  0.0%) |           0 (  0.0%) |           7 (  1.3%) |
| ······tasmlib_arithmetic_u64_shift_left                                    |          69 (  3.6%) |          51 (  3.7%) |           0 (  0.0%) |           0 (  0.0%) |          44 (  7.9%) |
| ······tasmlib_arithmetic_u64_overflowing_add                               |          30 (  1.6%) |          15 (  1.1%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ······tasmlib_arithmetic_u64_shift_right                                   |          75 (  3.9%) |          57 (  4.1%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ······tasmlib_arithmetic_u64_wrapping_sub                                  |         180 (  9.4%) |         108 (  7.8%) |           0 (  0.0%) |           0 (  0.0%) |         113 ( 20.2%) |
| ······tasmlib_arithmetic_u64_overflowing_sub                               |          63 (  3.3%) |          39 (  2.8%) |           0 (  0.0%) |           0 (  0.0%) |           2 (  0.4%) |
| ······tasmlib_arithmetic_u64_add                                           |          39 (  2.0%) |          24 (  1.7%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ······tasmlib_arithmetic_u64_safe_mul                                      |         117 (  6.1%) |          78 (  5.6%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··method_DazeFieldElement_mul                                              |         352 ( 18.4%) |         250 ( 18.0%) |           4 ( 20.0%) |           0 (  0.0%) |         288 ( 51.5%) |
| ····tasmlib_arithmetic_u64_mul_two_u64s_to_u128_u64                        |          33 (  1.7%) |          20 (  1.4%) |           0 (  0.0%) |           0 (  0.0%) |         107 ( 19.1%) |
| ··method_DazeFieldElement_valued                                           |         658 ( 34.4%) |         464 ( 33.5%) |           8 ( 40.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ····method_DazeFieldElement_canonical_representation                       |         642 ( 33.5%) |         456 ( 32.9%) |           8 ( 40.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ······montyred                                                             |         616 ( 32.2%) |         444 ( 32.0%) |           8 ( 40.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u128_shift_right                                |          92 (  4.8%) |          58 (  4.2%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··········tasmlib_arithmetic_u128_shift_right_shift_amount_gt_32           |          22 (  1.1%) |          12 (  0.9%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_shift_left                                  |          46 (  2.4%) |          34 (  2.5%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_overflowing_add                             |          20 (  1.0%) |          10 (  0.7%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_shift_right                                 |          50 (  2.6%) |          38 (  2.7%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_wrapping_sub                                |         120 (  6.3%) |          72 (  5.2%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_overflowing_sub                             |          42 (  2.2%) |          26 (  1.9%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_add                                         |          26 (  1.4%) |          16 (  1.2%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ········tasmlib_arithmetic_u64_safe_mul                                    |          78 (  4.1%) |          52 (  3.8%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··bfe_new_from_u64                                                         |           6 (  0.3%) |           3 (  0.2%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··tasmlib_io_write_to_stdout___bfe                                         |           3 (  0.2%) |           1 (  0.1%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| ··tasmlib_io_write_to_stdout___u64                                         |           3 (  0.2%) |           2 (  0.1%) |           0 (  0.0%) |           0 (  0.0%) |           0 (  0.0%) |
| Total                                                                      |        1915 (100.0%) |        1386 (100.0%) |          20 (100.0%) |         504 (100.0%) |         559 (100.0%) |
