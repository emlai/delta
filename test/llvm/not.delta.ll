
define i32 @main() {
  %b = alloca i1
  store i1 false, i1* %b
  %b.load = load i1, i1* %b
  %1 = xor i1 %b.load, true
  store i1 %1, i1* %b
  ret i32 0
}
