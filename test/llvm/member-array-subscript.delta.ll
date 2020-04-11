
%C = type { %"ArrayRef<int>" }
%"ArrayRef<int>" = type { i32*, i32 }

define i32 @main() {
  %1 = alloca %C
  %2 = alloca [5 x i32]
  store [5 x i32] [i32 0, i32 1, i32 2, i32 3, i32 4], [5 x i32]* %2
  %3 = getelementptr inbounds [5 x i32], [5 x i32]* %2, i32 0, i32 0
  %4 = insertvalue %"ArrayRef<int>" undef, i32* %3, 0
  %5 = insertvalue %"ArrayRef<int>" %4, i32 5, 1
  call void @_EN4main1C4initEAR_3int(%C* %1, %"ArrayRef<int>" %5)
  call void @_EN4main1C3fooE(%C* %1)
  ret i32 0
}

define void @_EN4main1C4initEAR_3int(%C* %this, %"ArrayRef<int>" %a) {
  %a1 = getelementptr inbounds %C, %C* %this, i32 0, i32 0
  store %"ArrayRef<int>" %a, %"ArrayRef<int>"* %a1
  ret void
}

define void @_EN4main1C3fooE(%C* %this) {
  %a = getelementptr inbounds %C, %C* %this, i32 0, i32 0
  %a.load = load %"ArrayRef<int>", %"ArrayRef<int>"* %a
  %1 = extractvalue %"ArrayRef<int>" %a.load, 0
  %2 = getelementptr inbounds i32, i32* %1, i32 4
  ret void
}
