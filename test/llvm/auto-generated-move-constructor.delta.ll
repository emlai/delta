
%X = type { i32 }

define void @_EN4main3fooEP1X1X(%X* %p, %X %x) {
  store %X %x, %X* %p
  ret void
}

define void @_EN4main1X6deinitE(%X* %this) {
  ret void
}
