
%"ArrayRef<int>" = type { i32*, i32 }

define i32 @main() {
  %three = alloca [3 x i32]
  %b = alloca i32
  %ref = alloca %"ArrayRef<int>"
  %bb = alloca i32
  store [3 x i32] [i32 0, i32 42, i32 0], [3 x i32]* %three
  %1 = getelementptr inbounds [3 x i32], [3 x i32]* %three, i32 0, i32 0
  %2 = insertvalue %"ArrayRef<int>" undef, i32* %1, 0
  %3 = insertvalue %"ArrayRef<int>" %2, i32 3, 1
  call void @_EN4main3fooEAR_3int(%"ArrayRef<int>" %3)
  call void @_EN4main3barEPA3_3int([3 x i32]* %three)
  store i32 3, i32* %b
  %4 = getelementptr inbounds [3 x i32], [3 x i32]* %three, i32 0, i32 0
  %5 = insertvalue %"ArrayRef<int>" undef, i32* %4, 0
  %6 = insertvalue %"ArrayRef<int>" %5, i32 3, 1
  store %"ArrayRef<int>" %6, %"ArrayRef<int>"* %ref
  %ref.load = load %"ArrayRef<int>", %"ArrayRef<int>"* %ref
  %size = extractvalue %"ArrayRef<int>" %ref.load, 1
  store i32 %size, i32* %bb
  ret i32 0
}

define void @_EN4main3fooEAR_3int(%"ArrayRef<int>" %ints) {
  %a = alloca i32
  %b = alloca i32
  %c = alloca %"ArrayRef<int>"
  %1 = extractvalue %"ArrayRef<int>" %ints, 0
  %2 = getelementptr inbounds i32, i32* %1, i32 1
  %.load = load i32, i32* %2
  store i32 %.load, i32* %a
  %size = extractvalue %"ArrayRef<int>" %ints, 1
  store i32 %size, i32* %b
  store %"ArrayRef<int>" %ints, %"ArrayRef<int>"* %c
  ret void
}

define void @_EN4main3barEPA3_3int([3 x i32]* %ints) {
  %b = alloca i32
  %ref = alloca %"ArrayRef<int>"
  store i32 3, i32* %b
  %1 = getelementptr inbounds [3 x i32], [3 x i32]* %ints, i32 0, i32 0
  %2 = insertvalue %"ArrayRef<int>" undef, i32* %1, 0
  %3 = insertvalue %"ArrayRef<int>" %2, i32 3, 1
  store %"ArrayRef<int>" %3, %"ArrayRef<int>"* %ref
  ret void
}
