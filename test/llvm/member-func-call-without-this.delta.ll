
define i32 @main() {
  %s = alloca {}
  %t = alloca {}
  call void @_EN4main1S4initE({}* %s)
  call void @_EN4main1S3fooE({}* %s)
  call void @_EN4main3FooI3intE4initE({}* %t)
  call void @_EN4main3FooI3intE3bazE({}* %t)
  call void @_EN4main3FooI3intE6deinitE({}* %t)
  ret i32 0
}

define void @_EN4main1S4initE({}* %this) {
  ret void
}

define void @_EN4main1S3fooE({}* %this) {
  call void @_EN4main1S3barE({}* %this)
  ret void
}

define void @_EN4main3FooI3intE4initE({}* %this) {
  call void @_EN4main3FooI3intE3bazE({}* %this)
  ret void
}

define void @_EN4main3FooI3intE3bazE({}* %this) {
  call void @_EN4main3FooI3intE3quxE({}* %this)
  ret void
}

define void @_EN4main3FooI3intE6deinitE({}* %this) {
  call void @_EN4main3FooI3intE3bazE({}* %this)
  ret void
}

define void @_EN4main1S3barE({}* %this) {
  ret void
}

define void @_EN4main3FooI3intE3quxE({}* %this) {
  ret void
}
