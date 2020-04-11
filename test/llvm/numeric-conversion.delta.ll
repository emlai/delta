
define i32 @main() {
  %i = alloca i16
  %f = alloca float
  %u = alloca i64
  %s = alloca i32
  store i16 42, i16* %i
  %i.load = load i16, i16* %i
  %1 = uitofp i16 %i.load to float
  store float %1, float* %f
  %f.load = load float, float* %f
  %2 = fptosi float %f.load to i64
  store i64 %2, i64* %u
  store i32 1, i32* %s
  ret i32 0
}
