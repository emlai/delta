
define i32 @main() {
  %x = alloca {}
  %1 = alloca {}
  call void @_EN4main1X4initE({}* %x)
  call void @_EN4main1X6deinitE({}* %x)
  call void @_EN4main1X4initE({}* %1)
  %.load = load {}, {}* %1
  store {} %.load, {}* %x
  call void @_EN4main1X6deinitE({}* %x)
  ret i32 0
}

define void @_EN4main1X4initE({}* %this) {
  ret void
}

define void @_EN4main1X6deinitE({}* %this) {
  ret void
}
