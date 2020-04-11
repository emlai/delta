
%A = type { i32, i32, i1 }
%B = type { i32, i32 }
%O = type { i32, i32 }

define i32 @main() {
  %a = alloca %A
  %b = alloca %B
  call void @_EN4main1A4initE4bool3int(%A* %a, i1 false, i32 0)
  call void @_EN4main1B4initE3int(%B* %b, i32 4)
  ret i32 0
}

define void @_EN4main1A4initE4bool3int(%A* %this, i1 %b, i32 %p) {
  %1 = bitcast %A* %this to %O*
  call void @_EN4main1O4initE3int(%O* %1, i32 %p)
  %b1 = getelementptr inbounds %A, %A* %this, i32 0, i32 2
  store i1 %b, i1* %b1
  ret void
}

define void @_EN4main1B4initE3int(%B* %this, i32 %p) {
  %p1 = getelementptr inbounds %B, %B* %this, i32 0, i32 0
  store i32 %p, i32* %p1
  %r = getelementptr inbounds %B, %B* %this, i32 0, i32 1
  store i32 %p, i32* %r
  ret void
}

define void @_EN4main1O4initE3int(%O* %this, i32 %p) {
  %p1 = getelementptr inbounds %O, %O* %this, i32 0, i32 0
  store i32 %p, i32* %p1
  %r = getelementptr inbounds %O, %O* %this, i32 0, i32 1
  store i32 %p, i32* %r
  ret void
}
