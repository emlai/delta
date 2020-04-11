
%"ArrayRef<int>" = type { i32*, i32 }

define i32 @main() {
  %1 = alloca [3 x i32]
  %2 = alloca [3 x i32]
  store [3 x i32] [i32 1, i32 2, i32 3], [3 x i32]* %1
  %3 = getelementptr inbounds [3 x i32], [3 x i32]* %1, i32 0, i32 0
  %4 = insertvalue %"ArrayRef<int>" undef, i32* %3, 0
  %5 = insertvalue %"ArrayRef<int>" %4, i32 3, 1
  call void @_EN4main1fI3intEEAR_3int(%"ArrayRef<int>" %5)
  store [3 x i32] [i32 1, i32 2, i32 3], [3 x i32]* %2
  %6 = getelementptr inbounds [3 x i32], [3 x i32]* %2, i32 0, i32 0
  %7 = insertvalue %"ArrayRef<int>" undef, i32* %6, 0
  %8 = insertvalue %"ArrayRef<int>" %7, i32 3, 1
  call void @_EN4main1fI3intEEAR_3int(%"ArrayRef<int>" %8)
  ret i32 0
}

define void @_EN4main1fI3intEEAR_3int(%"ArrayRef<int>" %a) {
  %s = alloca i32
  %size = extractvalue %"ArrayRef<int>" %a, 1
  store i32 %size, i32* %s
  ret void
}
