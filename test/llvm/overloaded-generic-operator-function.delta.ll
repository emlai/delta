
define void @_EN4maineqE1S1S({} %a, {} %b) {
  ret void
}

define void @_EN4main1fE1S({} %s) {
  call void @_EN4maineqE1S1S({} %s, {} %s)
  ret void
}
