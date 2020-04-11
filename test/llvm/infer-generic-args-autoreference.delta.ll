
define i32 @main() {
  %i = alloca i32
  store i32 42, i32* %i
  call void @_EN4main1fI3intEEP3int(i32* %i)
  ret i32 0
}

define void @_EN4main1fI3intEEP3int(i32* %p) {
  ret void
}
