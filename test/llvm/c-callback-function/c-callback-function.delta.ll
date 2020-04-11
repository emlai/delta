
define i32 @_EN4main1fE3int(i32 %a) {
  ret i32 %a
}

define i32 @main() {
  call void @foo(i32 (i32)* @_EN4main1fE3int)
  call void @bar(i32 (i32)* @_EN4main1fE3int)
  call void @baz(i32 (i32)* @_EN4main1fE3int)
  ret i32 0
}

declare void @foo(i32 (i32)*)

declare void @bar(i32 (i32)*)

declare void @baz(i32 (i32)*)
