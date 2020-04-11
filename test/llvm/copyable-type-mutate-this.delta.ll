
%S = type { i32 }

define i32 @main() {
  %1 = alloca %S
  call void @_EN4main1S4initE3int(%S* %1, i32 1)
  call void @_EN4main1S1sE(%S* %1)
  ret i32 0
}

define void @_EN4main1S4initE3int(%S* %this, i32 %i) {
  %i1 = getelementptr inbounds %S, %S* %this, i32 0, i32 0
  store i32 %i, i32* %i1
  ret void
}

define void @_EN4main1S1sE(%S* %this) {
  %i = getelementptr inbounds %S, %S* %this, i32 0, i32 0
  store i32 2, i32* %i
  ret void
}
