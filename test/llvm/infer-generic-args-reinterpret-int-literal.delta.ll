
define i32 @main() {
  %u = alloca i32
  %u.load = load i32, i32* %u
  call void @_EN4main1fI4uintEE4uint4uint(i32 0, i32 %u.load)
  ret i32 0
}

define void @_EN4main1fI4uintEE4uint4uint(i32 %a, i32 %b) {
  ret void
}
