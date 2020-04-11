
%"ArrayRef<int>" = type { i32*, i32 }

define void @_EN4main3fooEAR_3int(%"ArrayRef<int>" %a) {
  ret void
}

define i32 @main() {
  %a = alloca [3 x i32]
  store [3 x i32] [i32 1, i32 2, i32 3], [3 x i32]* %a
  %1 = getelementptr inbounds [3 x i32], [3 x i32]* %a, i32 0, i32 0
  %2 = insertvalue %"ArrayRef<int>" undef, i32* %1, 0
  %3 = insertvalue %"ArrayRef<int>" %2, i32 3, 1
  call void @_EN4main3fooEAR_3int(%"ArrayRef<int>" %3)
  call void @_EN4main3bazEPA3_3int([3 x i32]* %a)
  ret i32 0
}

define void @_EN4main3bazEPA3_3int([3 x i32]* %b) {
  ret void
}
