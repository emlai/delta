
define i32 @main(i32 %a, i32 %b, i32 %c, i32 %d) {
  %1 = and i32 %a, %b
  %2 = or i32 %a, %b
  %3 = xor i32 %a, %b
  %4 = shl i32 %a, %b
  %5 = ashr i32 %a, %b
  %6 = lshr i32 %c, %d
  %7 = xor i32 %a, -1
  ret i32 0
}
