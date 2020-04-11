
define i32 @main() {
  %1 = alloca {}
  %2 = alloca {}
  call void @_EN4main1SI3intE4initE({}* %1)
  call void @_EN4main1SI3intE1fE({}* %1)
  call void @_EN4main1SI5floatE4initE({}* %2)
  call void @_EN4main1SI5floatE1fE({}* %2)
  ret i32 0
}

define void @_EN4main1SI3intE4initE({}* %this) {
  ret void
}

define void @_EN4main1SI3intE1fE({}* %this) {
  call void @_EN4main1SI3intE1gE({}* %this)
  ret void
}

define void @_EN4main1SI5floatE4initE({}* %this) {
  ret void
}

define void @_EN4main1SI5floatE1fE({}* %this) {
  call void @_EN4main1SI5floatE1gE({}* %this)
  ret void
}

define void @_EN4main1SI3intE1gE({}* %this) {
  ret void
}

define void @_EN4main1SI5floatE1gE({}* %this) {
  ret void
}
