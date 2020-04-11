
define i32 @main() {
  %d = alloca i32
  store i32 42, i32* %d
  ret i32 0
}
