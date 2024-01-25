dazefield_element_mul:

# call graph
 main: 2464
   tasm_io_read_stdin___bfe: 2
   new: 566
     tasm_arithmetic_u128_safe_mul: 145
     montyred: 403
       tasm_arithmetic_u128_shift_right: 116
         tasm_arithmetic_u128_shift_right_handle_hi_shift: 66
           tasm_arithmetic_u128_shift_right: 49
       tasm_arithmetic_u64_shift_left: 23
       tasm_arithmetic_u64_overflowing_add: 11
       tasm_arithmetic_u64_shift_right: 26
       tasm_arithmetic_u64_wrapping_sub: 25
       tasm_arithmetic_u64_wrapping_sub: 25
       tasm_arithmetic_u64_overflowing_sub: 28
       tasm_arithmetic_u64_add: 14
       tasm_arithmetic_u64_safe_mul: 38
       tasm_arithmetic_u64_wrapping_sub: 25
   tasm_io_read_stdin___bfe: 2
   new: 566
     tasm_arithmetic_u128_safe_mul: 145
     montyred: 403
       tasm_arithmetic_u128_shift_right: 116
         tasm_arithmetic_u128_shift_right_handle_hi_shift: 66
           tasm_arithmetic_u128_shift_right: 49
       tasm_arithmetic_u64_shift_left: 23
       tasm_arithmetic_u64_overflowing_add: 11
       tasm_arithmetic_u64_shift_right: 26
       tasm_arithmetic_u64_wrapping_sub: 25
       tasm_arithmetic_u64_wrapping_sub: 25
       tasm_arithmetic_u64_overflowing_sub: 28
       tasm_arithmetic_u64_add: 14
       tasm_arithmetic_u64_safe_mul: 38
       tasm_arithmetic_u64_wrapping_sub: 25
   method_DazeFieldElement_mul: 448
     tasm_arithmetic_u64_mul_two_u64s_to_u128_u64: 33
     montyred: 403
       tasm_arithmetic_u128_shift_right: 116
         tasm_arithmetic_u128_shift_right_handle_hi_shift: 66
           tasm_arithmetic_u128_shift_right: 49
       tasm_arithmetic_u64_shift_left: 23
       tasm_arithmetic_u64_overflowing_add: 11
       tasm_arithmetic_u64_shift_right: 26
       tasm_arithmetic_u64_wrapping_sub: 25
       tasm_arithmetic_u64_wrapping_sub: 25
       tasm_arithmetic_u64_overflowing_sub: 28
       tasm_arithmetic_u64_add: 14
       tasm_arithmetic_u64_safe_mul: 38
       tasm_arithmetic_u64_wrapping_sub: 25
   method_DazeFieldElement_valued: 424
     method_DazeFieldElement_canonical_representation: 416
       montyred: 403
         tasm_arithmetic_u128_shift_right: 116
           tasm_arithmetic_u128_shift_right_handle_hi_shift: 66
             tasm_arithmetic_u128_shift_right: 49
         tasm_arithmetic_u64_shift_left: 23
         tasm_arithmetic_u64_overflowing_add: 11
         tasm_arithmetic_u64_shift_right: 26
         tasm_arithmetic_u64_wrapping_sub: 25
         tasm_arithmetic_u64_wrapping_sub: 25
         tasm_arithmetic_u64_overflowing_sub: 28
         tasm_arithmetic_u64_add: 14
         tasm_arithmetic_u64_safe_mul: 38
         tasm_arithmetic_u64_wrapping_sub: 25
   bfe_new_from_u64: 5
   tasm_io_write_to_stdout___bfe: 2
   method_DazeFieldElement_valued: 424
     method_DazeFieldElement_canonical_representation: 416
       montyred: 403
         tasm_arithmetic_u128_shift_right: 116
           tasm_arithmetic_u128_shift_right_handle_hi_shift: 66
             tasm_arithmetic_u128_shift_right: 49
         tasm_arithmetic_u64_shift_left: 23
         tasm_arithmetic_u64_overflowing_add: 11
         tasm_arithmetic_u64_shift_right: 26
         tasm_arithmetic_u64_wrapping_sub: 25
         tasm_arithmetic_u64_wrapping_sub: 25
         tasm_arithmetic_u64_overflowing_sub: 28
         tasm_arithmetic_u64_add: 14
         tasm_arithmetic_u64_safe_mul: 38
         tasm_arithmetic_u64_wrapping_sub: 25
   tasm_io_write_to_stdout___u64: 2
 total: 2466

# aggregated
 main: 2464 (1)
   tasm_io_read_stdin___bfe: 4 (0.0016233766)
   new: 1132 (0.45941558)
     tasm_arithmetic_u128_safe_mul: 290 (0.1176948)
     montyred: 1209 (0.49066558)
       tasm_arithmetic_u128_shift_right: 348 (0.14123377)
         tasm_arithmetic_u128_shift_right_handle_hi_shift: 198 (0.08035714)
           tasm_arithmetic_u128_shift_right: 147 (0.05965909)
       tasm_arithmetic_u64_shift_left: 69 (0.028003247)
       tasm_arithmetic_u64_overflowing_add: 33 (0.013392857)
       tasm_arithmetic_u64_shift_right: 78 (0.031655844)
       tasm_arithmetic_u64_wrapping_sub: 225 (0.091314934)
       tasm_arithmetic_u64_overflowing_sub: 84 (0.03409091)
       tasm_arithmetic_u64_add: 42 (0.017045455)
       tasm_arithmetic_u64_safe_mul: 114 (0.046266235)
   method_DazeFieldElement_mul: 448 (0.18181819)
     tasm_arithmetic_u64_mul_two_u64s_to_u128_u64: 33 (0.013392857)
   method_DazeFieldElement_valued: 848 (0.34415585)
     method_DazeFieldElement_canonical_representation: 832 (0.33766234)
       montyred: 806 (0.32711038)
         tasm_arithmetic_u128_shift_right: 232 (0.09415584)
           tasm_arithmetic_u128_shift_right_handle_hi_shift: 132 (0.05357143)
             tasm_arithmetic_u128_shift_right: 98 (0.039772727)
         tasm_arithmetic_u64_shift_left: 46 (0.01866883)
         tasm_arithmetic_u64_overflowing_add: 22 (0.008928572)
         tasm_arithmetic_u64_shift_right: 52 (0.021103896)
         tasm_arithmetic_u64_wrapping_sub: 150 (0.060876623)
         tasm_arithmetic_u64_overflowing_sub: 56 (0.022727273)
         tasm_arithmetic_u64_add: 28 (0.011363637)
         tasm_arithmetic_u64_safe_mul: 76 (0.030844156)
   bfe_new_from_u64: 5 (0.0020292208)
   tasm_io_write_to_stdout___bfe: 2 (0.0008116883)
   tasm_io_write_to_stdout___u64: 2 (0.0008116883)
 total: 2466 (1.0008117)
