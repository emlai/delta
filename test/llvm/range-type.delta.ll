
%"Range<int>" = type { i32, i32 }
%"RangeIterator<int>" = type { i32, i32 }

define i32 @_EN4main3fooE5RangeI3intE(%"Range<int>" %r) {
  %sum = alloca i32
  %__iterator = alloca %"RangeIterator<int>"
  %1 = alloca %"Range<int>"
  %i = alloca i32
  store i32 0, i32* %sum
  store %"Range<int>" %r, %"Range<int>"* %1
  %2 = call %"RangeIterator<int>" @_EN3std5RangeI3intE8iteratorE(%"Range<int>"* %1)
  store %"RangeIterator<int>" %2, %"RangeIterator<int>"* %__iterator
  br label %loop.condition

loop.condition:                                   ; preds = %loop.increment, %0
  %3 = call i1 @_EN3std13RangeIteratorI3intE8hasValueE(%"RangeIterator<int>"* %__iterator)
  br i1 %3, label %loop.body, label %loop.end

loop.body:                                        ; preds = %loop.condition
  %4 = call i32 @_EN3std13RangeIteratorI3intE5valueE(%"RangeIterator<int>"* %__iterator)
  store i32 %4, i32* %i
  %sum.load = load i32, i32* %sum
  %i.load = load i32, i32* %i
  %5 = add i32 %sum.load, %i.load
  store i32 %5, i32* %sum
  br label %loop.increment

loop.increment:                                   ; preds = %loop.body
  call void @_EN3std13RangeIteratorI3intE9incrementE(%"RangeIterator<int>"* %__iterator)
  br label %loop.condition

loop.end:                                         ; preds = %loop.condition
  %sum.load1 = load i32, i32* %sum
  ret i32 %sum.load1
}

declare %"RangeIterator<int>" @_EN3std5RangeI3intE8iteratorE(%"Range<int>"*)

declare i1 @_EN3std13RangeIteratorI3intE8hasValueE(%"RangeIterator<int>"*)

declare i32 @_EN3std13RangeIteratorI3intE5valueE(%"RangeIterator<int>"*)

declare void @_EN3std13RangeIteratorI3intE9incrementE(%"RangeIterator<int>"*)

define i32 @main() {
  %1 = alloca %"Range<int>"
  call void @_EN3std5RangeI3intE4initE3int3int(%"Range<int>"* %1, i32 0, i32 5)
  %.load = load %"Range<int>", %"Range<int>"* %1
  %2 = call i32 @_EN4main3fooE5RangeI3intE(%"Range<int>" %.load)
  ret i32 %2
}

declare void @_EN3std5RangeI3intE4initE3int3int(%"Range<int>"*, i32, i32)
