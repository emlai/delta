
%"Range<int>" = type { i32, i32 }

define void @_EN4main1fE5RangeI3intE(%"Range<int>" %r) {
  ret void
}

define i32 @main() {
  %foo = alloca i32
  %bar = alloca i32
  %1 = alloca %"Range<int>"
  store i32 0, i32* %foo
  store i32 0, i32* %bar
  %foo.load = load i32, i32* %foo
  %bar.load = load i32, i32* %bar
  call void @_EN3std5RangeI3intE4initE3int3int(%"Range<int>"* %1, i32 %foo.load, i32 %bar.load)
  %.load = load %"Range<int>", %"Range<int>"* %1
  call void @_EN4main1fE5RangeI3intE(%"Range<int>" %.load)
  ret i32 0
}

declare void @_EN3std5RangeI3intE4initE3int3int(%"Range<int>"*, i32, i32)
