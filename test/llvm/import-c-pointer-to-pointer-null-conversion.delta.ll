
define i32 @main() {
  call void @foo(i8** null)
  ret i32 0
}

declare void @foo(i8**)
