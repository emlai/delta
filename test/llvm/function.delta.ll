
define i32 @main() {
  call void @bar()
  call void @_EN4main3fooE()
  ret i32 0
}

declare void @bar()

define void @_EN4main3fooE() {
  ret void
}
