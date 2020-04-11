
define i32 @main() {
  %i = alloca i32
  store i32 0, i32* %i
  %i.load = load i32, i32* %i
  %1 = add i32 %i.load, 1
  store i32 %1, i32* %i
  %i.load1 = load i32, i32* %i
  %2 = sub i32 %i.load1, 1
  store i32 %2, i32* %i
  %i.load2 = load i32, i32* %i
  %3 = mul i32 %i.load2, 1
  store i32 %3, i32* %i
  %i.load3 = load i32, i32* %i
  %4 = sdiv i32 %i.load3, 1
  store i32 %4, i32* %i
  %i.load4 = load i32, i32* %i
  %5 = and i32 %i.load4, 1
  store i32 %5, i32* %i
  %i.load5 = load i32, i32* %i
  %6 = or i32 %i.load5, 1
  store i32 %6, i32* %i
  %i.load6 = load i32, i32* %i
  %7 = xor i32 %i.load6, 1
  store i32 %7, i32* %i
  %i.load7 = load i32, i32* %i
  %8 = shl i32 %i.load7, 1
  store i32 %8, i32* %i
  %i.load8 = load i32, i32* %i
  %9 = ashr i32 %i.load8, 1
  store i32 %9, i32* %i
  ret i32 0
}
