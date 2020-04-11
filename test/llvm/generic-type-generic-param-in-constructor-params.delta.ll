
%"ArrayRef<int>" = type { i32*, i32 }

define i32 @main() {
  %b = alloca [3 x i32]
  %1 = alloca {}
  store [3 x i32] [i32 1, i32 2, i32 3], [3 x i32]* %b
  %2 = getelementptr inbounds [3 x i32], [3 x i32]* %b, i32 0, i32 0
  %3 = insertvalue %"ArrayRef<int>" undef, i32* %2, 0
  %4 = insertvalue %"ArrayRef<int>" %3, i32 3, 1
  call void @_EN4main1MI3intE4initEAR_3int({}* %1, %"ArrayRef<int>" %4)
  ret i32 0
}

define void @_EN4main1MI3intE4initEAR_3int({}* %this, %"ArrayRef<int>" %a) {
  ret void
}
