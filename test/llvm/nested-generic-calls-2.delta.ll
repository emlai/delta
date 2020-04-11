
%"M<S>" = type { %S }
%S = type { i32 }

define i32 @main() {
  %m = alloca %"M<S>"
  call void @_EN4main1MI1SE4initE(%"M<S>"* %m)
  call void @_EN4main1MI1SE1fE(%"M<S>"* %m)
  ret i32 0
}

define void @_EN4main1MI1SE4initE(%"M<S>"* %this) {
  ret void
}

define void @_EN4main1MI1SE1fE(%"M<S>"* %this) {
  %a = alloca {}
  %1 = alloca %S
  call void @_EN4main1AI1SE4initE({}* %a)
  %2 = call %S @_EN4main1AI1SE1aE({}* %a)
  store %S %2, %S* %1
  call void @_EN4main1S1iE(%S* %1)
  ret void
}

define void @_EN4main1AI1SE4initE({}* %this) {
  ret void
}

define %S @_EN4main1AI1SE1aE({}* %this) {
  %1 = alloca %S
  call void @_EN4main1S4initE(%S* %1)
  %.load = load %S, %S* %1
  ret %S %.load
}

define void @_EN4main1S1iE(%S* %this) {
  ret void
}

define void @_EN4main1S4initE(%S* %this) {
  ret void
}
