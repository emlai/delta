define void @main() {
  %i = alloca i32
  store i32 42, i32* %i
  %j = alloca i32
  store i32 1, i32* %j
  %k = alloca i32
  store i32 105, i32* %k
  ret void
}
