
%A = type { i32 }

define i32 @main() {
  %a = alloca %A
  call void @_EN4main1A4initE(%A* %a)
  ret i32 0
}

define void @_EN4main1A4initE(%A* %this) {
  ret void
}
