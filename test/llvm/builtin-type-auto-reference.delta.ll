
define void @_EN4main3fooEP3int(i32* %ref_i) {
  ret void
}

define i32 @main() {
  %bar = alloca i32
  store i32 42, i32* %bar
  call void @_EN4main3fooEP3int(i32* %bar)
  ret i32 0
}
