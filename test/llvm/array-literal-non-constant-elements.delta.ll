
define [2 x i32] @_EN4main1fE3int3int(i32 %foo, i32 %bar) {
  %1 = add i32 %foo, %bar
  %2 = insertvalue [2 x i32] undef, i32 %1, 0
  %3 = sub i32 %foo, %bar
  %4 = insertvalue [2 x i32] %2, i32 %3, 1
  ret [2 x i32] %4
}

define [2 x i32] @_EN4main1gE3int3int(i32 %foo, i32 %bar) {
  %c = alloca [2 x i32]
  %1 = add i32 %foo, %bar
  %2 = insertvalue [2 x i32] undef, i32 %1, 0
  %3 = sub i32 %foo, %bar
  %4 = insertvalue [2 x i32] %2, i32 %3, 1
  store [2 x i32] %4, [2 x i32]* %c
  %c.load = load [2 x i32], [2 x i32]* %c
  ret [2 x i32] %c.load
}
