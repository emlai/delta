
%"ArrayIterator<int>" = type { i32*, i32* }
%StringBuffer = type { %"List<char>" }
%"List<char>" = type { i8*, i32, i32 }

@0 = private unnamed_addr constant [6 x i8] c"%.*s\0A\00", align 1

define i32 @main() {
  %__iterator = alloca %"ArrayIterator<int>"
  %1 = alloca [3 x i32]
  %e = alloca i32*
  %a = alloca [2 x i32]
  %__iterator1 = alloca %"ArrayIterator<int>"
  %e2 = alloca i32*
  store [3 x i32] [i32 1, i32 2, i32 3], [3 x i32]* %1
  %2 = getelementptr inbounds [3 x i32], [3 x i32]* %1, i32 0, i32 0
  %3 = getelementptr inbounds i32, i32* %2, i32 3
  %4 = insertvalue %"ArrayIterator<int>" undef, i32* %2, 0
  %5 = insertvalue %"ArrayIterator<int>" %4, i32* %3, 1
  store %"ArrayIterator<int>" %5, %"ArrayIterator<int>"* %__iterator
  br label %loop.condition

loop.condition:                                   ; preds = %loop.increment, %0
  %6 = call i1 @_EN3std13ArrayIteratorI3intE8hasValueE(%"ArrayIterator<int>"* %__iterator)
  br i1 %6, label %loop.body, label %loop.end

loop.body:                                        ; preds = %loop.condition
  %7 = call i32* @_EN3std13ArrayIteratorI3intE5valueE(%"ArrayIterator<int>"* %__iterator)
  store i32* %7, i32** %e
  %e.load = load i32*, i32** %e
  call void @_EN3std7printlnI3intEEP3int(i32* %e.load)
  br label %loop.increment

loop.increment:                                   ; preds = %loop.body
  call void @_EN3std13ArrayIteratorI3intE9incrementE(%"ArrayIterator<int>"* %__iterator)
  br label %loop.condition

loop.end:                                         ; preds = %loop.condition
  store [2 x i32] [i32 4, i32 5], [2 x i32]* %a
  %8 = getelementptr inbounds [2 x i32], [2 x i32]* %a, i32 0, i32 0
  %9 = getelementptr inbounds i32, i32* %8, i32 2
  %10 = insertvalue %"ArrayIterator<int>" undef, i32* %8, 0
  %11 = insertvalue %"ArrayIterator<int>" %10, i32* %9, 1
  store %"ArrayIterator<int>" %11, %"ArrayIterator<int>"* %__iterator1
  br label %loop.condition3

loop.condition3:                                  ; preds = %loop.increment6, %loop.end
  %12 = call i1 @_EN3std13ArrayIteratorI3intE8hasValueE(%"ArrayIterator<int>"* %__iterator1)
  br i1 %12, label %loop.body4, label %loop.end7

loop.body4:                                       ; preds = %loop.condition3
  %13 = call i32* @_EN3std13ArrayIteratorI3intE5valueE(%"ArrayIterator<int>"* %__iterator1)
  store i32* %13, i32** %e2
  %e.load5 = load i32*, i32** %e2
  call void @_EN3std7printlnI3intEEP3int(i32* %e.load5)
  br label %loop.increment6

loop.increment6:                                  ; preds = %loop.body4
  call void @_EN3std13ArrayIteratorI3intE9incrementE(%"ArrayIterator<int>"* %__iterator1)
  br label %loop.condition3

loop.end7:                                        ; preds = %loop.condition3
  ret i32 0
}

define i1 @_EN3std13ArrayIteratorI3intE8hasValueE(%"ArrayIterator<int>"* %this) {
  %current = getelementptr inbounds %"ArrayIterator<int>", %"ArrayIterator<int>"* %this, i32 0, i32 0
  %current.load = load i32*, i32** %current
  %end = getelementptr inbounds %"ArrayIterator<int>", %"ArrayIterator<int>"* %this, i32 0, i32 1
  %end.load = load i32*, i32** %end
  %1 = icmp ne i32* %current.load, %end.load
  ret i1 %1
}

define i32* @_EN3std13ArrayIteratorI3intE5valueE(%"ArrayIterator<int>"* %this) {
  %current = getelementptr inbounds %"ArrayIterator<int>", %"ArrayIterator<int>"* %this, i32 0, i32 0
  %current.load = load i32*, i32** %current
  ret i32* %current.load
}

define void @_EN3std7printlnI3intEEP3int(i32* %value) {
  %s = alloca %StringBuffer
  %1 = call %StringBuffer @_EN3std3int8toStringE(i32* %value)
  store %StringBuffer %1, %StringBuffer* %s
  %2 = call i32 @_EN3std12StringBuffer4sizeE(%StringBuffer* %s)
  %3 = call i8* @_EN3std12StringBuffer4dataE(%StringBuffer* %s)
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @0, i32 0, i32 0), i32 %2, i8* %3)
  call void @_EN3std12StringBuffer6deinitE(%StringBuffer* %s)
  ret void
}

define void @_EN3std13ArrayIteratorI3intE9incrementE(%"ArrayIterator<int>"* %this) {
  %current = getelementptr inbounds %"ArrayIterator<int>", %"ArrayIterator<int>"* %this, i32 0, i32 0
  %current.load = load i32*, i32** %current
  %1 = getelementptr inbounds i32, i32* %current.load, i32 1
  store i32* %1, i32** %current
  ret void
}

declare %StringBuffer @_EN3std3int8toStringE(i32*)

declare i32 @_EN3std12StringBuffer4sizeE(%StringBuffer*)

declare i8* @_EN3std12StringBuffer4dataE(%StringBuffer*)

declare i32 @printf(i8*, ...)

declare void @_EN3std12StringBuffer6deinitE(%StringBuffer*)
