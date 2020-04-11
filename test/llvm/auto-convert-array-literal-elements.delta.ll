
define i32 @main() {
  %foo = alloca [1 x float]
  %bar = alloca [2 x i16]
  store [1 x float] [float 1.000000e+00], [1 x float]* %foo
  store [2 x i16] [i16 50, i16 100], [2 x i16]* %bar
  ret i32 0
}
