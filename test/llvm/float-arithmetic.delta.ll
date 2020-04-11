
define i32 @main() {
  %a = alloca float
  %b = alloca float
  store float 1.000000e+00, float* %a
  %a.load = load float, float* %a
  %1 = fsub float -0.000000e+00, %a.load
  %2 = fmul float 2.000000e+00, %1
  store float %2, float* %b
  %b.load = load float, float* %b
  %3 = fadd float %b.load, 1.000000e+00
  store float %3, float* %b
  %b.load1 = load float, float* %b
  %4 = fadd float %b.load1, -1.000000e+00
  store float %4, float* %b
  ret i32 0
}
