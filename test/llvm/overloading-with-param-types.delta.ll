
%S = type { i32 }
%T = type { i32 }

define void @_EN4main1fE1S(%S %a) {
  ret void
}

define void @_EN4main1fE1T(%T %a) {
  ret void
}

define void @_EN4mainplE1S1S(%S %a, %S %b) {
  ret void
}

define void @_EN4mainplE1T1T(%T %a, %T %b) {
  ret void
}

define i32 @main() {
  %s = alloca %S
  %t = alloca %T
  call void @_EN4main1S4initE(%S* %s)
  call void @_EN4main1T4initE(%T* %t)
  %s.load = load %S, %S* %s
  call void @_EN4main1fE1S(%S %s.load)
  %t.load = load %T, %T* %t
  call void @_EN4main1fE1T(%T %t.load)
  %s.load1 = load %S, %S* %s
  %s.load2 = load %S, %S* %s
  call void @_EN4mainplE1S1S(%S %s.load1, %S %s.load2)
  %t.load3 = load %T, %T* %t
  %t.load4 = load %T, %T* %t
  call void @_EN4mainplE1T1T(%T %t.load3, %T %t.load4)
  ret i32 0
}

define void @_EN4main1S4initE(%S* %this) {
  %i = getelementptr inbounds %S, %S* %this, i32 0, i32 0
  store i32 0, i32* %i
  ret void
}

define void @_EN4main1T4initE(%T* %this) {
  %i = getelementptr inbounds %T, %T* %this, i32 0, i32 0
  store i32 0, i32* %i
  ret void
}
