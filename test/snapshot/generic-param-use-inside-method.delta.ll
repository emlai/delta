
define i32 @main() {
  %s = alloca {}, align 8
  call void @_EN4main1SI3intE4initE({}* %s)
  call void @_EN4main1SI3intE1fE({}* %s)
  ret i32 0
}

define void @_EN4main1SI3intE4initE({}* %this) {
  ret void
}

define void @_EN4main1SI3intE1fE({}* %this) {
  %t = alloca i32, align 4
  call void @_EN4main1SI3intE1gE({}* %this)
  ret void
}

define void @_EN4main1SI3intE1gE({}* %this) {
  %t2 = alloca i32, align 4
  ret void
}
