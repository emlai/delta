
define i32 @main() {
  %s = alloca {}
  %1 = alloca i32
  %2 = alloca i32
  call void @_EN4main1SI3intE4initE({}* %s)
  store i32 0, i32* %1
  call void @_EN4main1SI3intE1fEP3int({}* %s, i32* %1)
  store i32 0, i32* %2
  call void @_EN4main1SI3intE1fEP3int({}* %s, i32* %2)
  ret i32 0
}

define void @_EN4main1SI3intE4initE({}* %this) {
  ret void
}

define void @_EN4main1SI3intE1fEP3int({}* %this, i32* %t) {
  ret void
}
