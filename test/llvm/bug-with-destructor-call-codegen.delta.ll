
define i32 @main() {
  %a = alloca {}
  %1 = alloca {}
  call void @_EN4main1X4initE({}* %a)
  call void @_EN4main1YI4boolE4initE({}* %1)
  %2 = call i32 @_EN4main1YI4boolE1fE({}* %1)
  call void @_EN4main1X6deinitE({}* %a)
  ret i32 0
}

define void @_EN4main1X4initE({}* %this) {
  ret void
}

define void @_EN4main1YI4boolE4initE({}* %this) {
  ret void
}

define i32 @_EN4main1YI4boolE1fE({}* %this) {
  ret i32 1
}

define void @_EN4main1X6deinitE({}* %this) {
  ret void
}
