
define i32 @main() {
  %f = alloca float
  %g = alloca float
  store float 4.200000e+01, float* %f
  store float -4.200000e+01, float* %g
  ret i32 0
}
