
define i32 @main({} %x) {
  %a = alloca {}
  %1 = alloca {}
  store {} %x, {}* %1
  %2 = call {} @_EN4main1XI3intE1fI4boolEE4bool({}* %1, i1 false)
  store {} %2, {}* %a
  call void @_EN4main1XI4boolE1gE({}* %a)
  ret i32 0
}

define {} @_EN4main1XI3intE1fI4boolEE4bool({}* %this, i1 %u) {
  %x = alloca {}
  %x.load = load {}, {}* %x
  ret {} %x.load
}

define void @_EN4main1XI4boolE1gE({}* %this) {
  ret void
}
