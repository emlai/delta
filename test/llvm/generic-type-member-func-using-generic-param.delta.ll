
define i32 @main() {
  %a = alloca {}
  call void @_EN4main1AI3intE4initE({}* %a)
  call void @_EN4main1AI3intE1aE3int({}* %a, i32 5)
  ret i32 0
}

define void @_EN4main1AI3intE4initE({}* %this) {
  ret void
}

define void @_EN4main1AI3intE1aE3int({}* %this, i32 %n) {
  ret void
}
