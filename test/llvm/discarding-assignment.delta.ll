
declare i32 @foo()

define i32 @main() {
  %1 = call i32 @foo()
  ret i32 0
}
