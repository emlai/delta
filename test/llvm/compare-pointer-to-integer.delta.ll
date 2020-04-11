
define void @_EN4main1fEP3int3int(i32* %foo, i32 %bar) {
  %foo.load = load i32, i32* %foo
  %1 = icmp slt i32 %foo.load, %bar
  ret void
}
