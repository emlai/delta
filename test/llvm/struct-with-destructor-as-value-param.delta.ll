
%"A<int>" = type { i32 }

define void @_EN4main3fooE1AI3intE(%"A<int>" %a) {
  %1 = alloca %"A<int>"
  store %"A<int>" %a, %"A<int>"* %1
  call void @_EN4main1AI3intE6deinitE(%"A<int>"* %1)
  ret void
}

define void @_EN4main1AI3intE6deinitE(%"A<int>"* %this) {
  ret void
}
