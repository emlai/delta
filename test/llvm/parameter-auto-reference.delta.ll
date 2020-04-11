
define void @_EN4main1fE1S({} %s) {
  %1 = alloca {}
  store {} %s, {}* %1
  call void @_EN4main1gEP1S({}* %1)
  ret void
}

define void @_EN4main1gEP1S({}* %s) {
  ret void
}
