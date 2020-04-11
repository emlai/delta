
define {} @_EN4main1fE() {
  %x = alloca {}
  call void @_EN4main1X4initE({}* %x)
  %x.load = load {}, {}* %x
  ret {} %x.load
}

define void @_EN4main1X4initE({}* %this) {
  ret void
}

define void @_EN4main1X6deinitE({}* %this) {
  ret void
}

define i32 @main() {
  %a = alloca {}
  %1 = call {} @_EN4main1fE()
  store {} %1, {}* %a
  call void @_EN4main1X6deinitE({}* %a)
  ret i32 0
}
