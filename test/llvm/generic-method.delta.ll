
define i32 @_EN4main1gE() {
  ret i32 0
}

define i1 @_EN4main1hE() {
  ret i1 false
}

define i32 @main() {
  %x = alloca {}
  call void @_EN4main1X4initE({}* %x)
  %1 = call i32 @_EN4main1X1fI3intEEF_3int({}* %x, i32 ()* @_EN4main1gE)
  %2 = call i1 @_EN4main1X1fI4boolEEF_4bool({}* %x, i1 ()* @_EN4main1hE)
  ret i32 0
}

define void @_EN4main1X4initE({}* %this) {
  ret void
}

define i32 @_EN4main1X1fI3intEEF_3int({}* %this, i32 ()* %t) {
  %1 = call i32 %t()
  ret i32 %1
}

define i1 @_EN4main1X1fI4boolEEF_4bool({}* %this, i1 ()* %t) {
  %1 = call i1 %t()
  ret i1 %1
}
