
define i32 @main() {
  %i = alloca i32
  %j = alloca i32
  %k = alloca i32
  store i32 42, i32* %i
  store i32 1, i32* %j
  store i32 105, i32* %k
  ret i32 0
}
