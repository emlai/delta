
%"F<int, bool>" = type { i32, i1 }

define i32 @main() {
  %f = alloca %"F<int, bool>"
  call void @_EN4main1FI3int4boolE4initE(%"F<int, bool>"* %f)
  call void @_EN4main1FI3int4boolE3fooE(%"F<int, bool>"* %f)
  ret i32 0
}

define void @_EN4main1FI3int4boolE4initE(%"F<int, bool>"* %this) {
  ret void
}

define void @_EN4main1FI3int4boolE3fooE(%"F<int, bool>"* %this) {
  ret void
}
