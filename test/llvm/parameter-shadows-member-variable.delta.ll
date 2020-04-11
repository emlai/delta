
%S = type { i32 }

define i32 @main() {
  %1 = alloca %S
  call void @_EN4main1S4initE(%S* %1)
  call void @_EN4main1S3fooE3int(%S* %1, i32 30)
  ret i32 0
}

define void @_EN4main1S4initE(%S* %this) {
  ret void
}

define void @_EN4main1S3fooE3int(%S* %this, i32 %bar) {
  %1 = add i32 %bar, 42
  ret void
}
