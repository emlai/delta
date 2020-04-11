
define void @_EN4main1fE1YP1Y({} %a, {}* %b) {
  call void @_EN4main1Y6deinitE({}* %b)
  store {} %a, {}* %b
  ret void
}

define void @_EN4main1Y6deinitE({}* %this) {
  ret void
}

define void @_EN4main1gE1Y({} %a) {
  %b = alloca {}
  store {} %a, {}* %b
  call void @_EN4main1Y6deinitE({}* %b)
  ret void
}
