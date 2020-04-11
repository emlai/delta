
%"ClosedRangeIterator<int>" = type { i32, i32 }
%"ClosedRange<int>" = type { i32, i32 }

define i32 @main() {
  %sum = alloca i32
  %__iterator = alloca %"ClosedRangeIterator<int>"
  %1 = alloca %"ClosedRange<int>"
  %i = alloca i32
  store i32 0, i32* %sum
  call void @_EN3std11ClosedRangeI3intE4initE3int3int(%"ClosedRange<int>"* %1, i32 68, i32 75)
  %2 = call %"ClosedRangeIterator<int>" @_EN3std11ClosedRangeI3intE8iteratorE(%"ClosedRange<int>"* %1)
  store %"ClosedRangeIterator<int>" %2, %"ClosedRangeIterator<int>"* %__iterator
  br label %loop.condition

loop.condition:                                   ; preds = %loop.increment, %0
  %3 = call i1 @_EN3std19ClosedRangeIteratorI3intE8hasValueE(%"ClosedRangeIterator<int>"* %__iterator)
  br i1 %3, label %loop.body, label %loop.end

loop.body:                                        ; preds = %loop.condition
  %4 = call i32 @_EN3std19ClosedRangeIteratorI3intE5valueE(%"ClosedRangeIterator<int>"* %__iterator)
  store i32 %4, i32* %i
  %sum.load = load i32, i32* %sum
  %i.load = load i32, i32* %i
  %5 = add i32 %sum.load, %i.load
  store i32 %5, i32* %sum
  br label %loop.increment

loop.increment:                                   ; preds = %loop.body
  call void @_EN3std19ClosedRangeIteratorI3intE9incrementE(%"ClosedRangeIterator<int>"* %__iterator)
  br label %loop.condition

loop.end:                                         ; preds = %loop.condition
  ret i32 0
}

define void @_EN3std11ClosedRangeI3intE4initE3int3int(%"ClosedRange<int>"* %this, i32 %start, i32 %end) {
  %start1 = getelementptr inbounds %"ClosedRange<int>", %"ClosedRange<int>"* %this, i32 0, i32 0
  store i32 %start, i32* %start1
  %end2 = getelementptr inbounds %"ClosedRange<int>", %"ClosedRange<int>"* %this, i32 0, i32 1
  store i32 %end, i32* %end2
  ret void
}

define %"ClosedRangeIterator<int>" @_EN3std11ClosedRangeI3intE8iteratorE(%"ClosedRange<int>"* %this) {
  %1 = alloca %"ClosedRangeIterator<int>"
  %this.load = load %"ClosedRange<int>", %"ClosedRange<int>"* %this
  call void @_EN3std19ClosedRangeIteratorI3intE4initE11ClosedRangeI3intE(%"ClosedRangeIterator<int>"* %1, %"ClosedRange<int>" %this.load)
  %.load = load %"ClosedRangeIterator<int>", %"ClosedRangeIterator<int>"* %1
  ret %"ClosedRangeIterator<int>" %.load
}

define i1 @_EN3std19ClosedRangeIteratorI3intE8hasValueE(%"ClosedRangeIterator<int>"* %this) {
  %current = getelementptr inbounds %"ClosedRangeIterator<int>", %"ClosedRangeIterator<int>"* %this, i32 0, i32 0
  %current.load = load i32, i32* %current
  %end = getelementptr inbounds %"ClosedRangeIterator<int>", %"ClosedRangeIterator<int>"* %this, i32 0, i32 1
  %end.load = load i32, i32* %end
  %1 = icmp sle i32 %current.load, %end.load
  ret i1 %1
}

define i32 @_EN3std19ClosedRangeIteratorI3intE5valueE(%"ClosedRangeIterator<int>"* %this) {
  %current = getelementptr inbounds %"ClosedRangeIterator<int>", %"ClosedRangeIterator<int>"* %this, i32 0, i32 0
  %current.load = load i32, i32* %current
  ret i32 %current.load
}

define void @_EN3std19ClosedRangeIteratorI3intE9incrementE(%"ClosedRangeIterator<int>"* %this) {
  %current = getelementptr inbounds %"ClosedRangeIterator<int>", %"ClosedRangeIterator<int>"* %this, i32 0, i32 0
  %current.load = load i32, i32* %current
  %1 = add i32 %current.load, 1
  store i32 %1, i32* %current
  ret void
}

define void @_EN3std19ClosedRangeIteratorI3intE4initE11ClosedRangeI3intE(%"ClosedRangeIterator<int>"* %this, %"ClosedRange<int>" %range) {
  %1 = alloca %"ClosedRange<int>"
  %2 = alloca %"ClosedRange<int>"
  %current = getelementptr inbounds %"ClosedRangeIterator<int>", %"ClosedRangeIterator<int>"* %this, i32 0, i32 0
  store %"ClosedRange<int>" %range, %"ClosedRange<int>"* %1
  %3 = call i32 @_EN3std11ClosedRangeI3intE5startE(%"ClosedRange<int>"* %1)
  store i32 %3, i32* %current
  %end = getelementptr inbounds %"ClosedRangeIterator<int>", %"ClosedRangeIterator<int>"* %this, i32 0, i32 1
  store %"ClosedRange<int>" %range, %"ClosedRange<int>"* %2
  %4 = call i32 @_EN3std11ClosedRangeI3intE3endE(%"ClosedRange<int>"* %2)
  store i32 %4, i32* %end
  ret void
}

define i32 @_EN3std11ClosedRangeI3intE5startE(%"ClosedRange<int>"* %this) {
  %start = getelementptr inbounds %"ClosedRange<int>", %"ClosedRange<int>"* %this, i32 0, i32 0
  %start.load = load i32, i32* %start
  ret i32 %start.load
}

define i32 @_EN3std11ClosedRangeI3intE3endE(%"ClosedRange<int>"* %this) {
  %end = getelementptr inbounds %"ClosedRange<int>", %"ClosedRange<int>"* %this, i32 0, i32 1
  %end.load = load i32, i32* %end
  ret i32 %end.load
}
