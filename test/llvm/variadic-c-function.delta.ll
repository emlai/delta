
@0 = private unnamed_addr constant [6 x i8] c"%d %f\00", align 1

define i32 @main() {
  %f = alloca float
  store float 0x4010CCCCC0000000, float* %f
  %f.load = load float, float* %f
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @0, i32 0, i32 0), i32 -3, float %f.load)
  ret i32 0
}

declare i32 @printf(i8*, ...)
