
%vec2 = type { i32, i32 }

define %vec2 @_EN4mainmlE4vec24vec2(%vec2 %a, %vec2 %b) {
  %1 = alloca %vec2
  %x = extractvalue %vec2 %a, 0
  %x1 = extractvalue %vec2 %b, 0
  %2 = mul i32 %x, %x1
  %y = extractvalue %vec2 %a, 1
  %y2 = extractvalue %vec2 %b, 1
  %3 = mul i32 %y, %y2
  call void @_EN4main4vec24initE3int3int(%vec2* %1, i32 %2, i32 %3)
  %.load = load %vec2, %vec2* %1
  ret %vec2 %.load
}

define void @_EN4main4vec24initE3int3int(%vec2* %this, i32 %x, i32 %y) {
  %x1 = getelementptr inbounds %vec2, %vec2* %this, i32 0, i32 0
  store i32 %x, i32* %x1
  %y2 = getelementptr inbounds %vec2, %vec2* %this, i32 0, i32 1
  store i32 %y, i32* %y2
  ret void
}

define i32 @main() {
  %v = alloca %vec2
  %1 = alloca %vec2
  %2 = alloca %vec2
  call void @_EN4main4vec24initE3int3int(%vec2* %v, i32 3, i32 2)
  call void @_EN4main4vec24initE3int3int(%vec2* %1, i32 2, i32 4)
  %.load = load %vec2, %vec2* %1
  %v.load = load %vec2, %vec2* %v
  %3 = call %vec2 @_EN4mainmlE4vec24vec2(%vec2 %.load, %vec2 %v.load)
  store %vec2 %3, %vec2* %v
  %v.load1 = load %vec2, %vec2* %v
  call void @_EN4main4vec24initE3int3int(%vec2* %2, i32 -1, i32 3)
  %.load2 = load %vec2, %vec2* %2
  %4 = call i1 @_EN4maineqE4vec24vec2(%vec2 %v.load1, %vec2 %.load2)
  %x = getelementptr inbounds %vec2, %vec2* %v, i32 0, i32 0
  %x.load = load i32, i32* %x
  %5 = call i32 @_EN4main4vec2ixE3int(%vec2* %v, i32 %x.load)
  ret i32 0
}

define i1 @_EN4maineqE4vec24vec2(%vec2 %a, %vec2 %b) {
  %x = extractvalue %vec2 %a, 0
  %x1 = extractvalue %vec2 %b, 0
  %1 = icmp eq i32 %x, %x1
  ret i1 %1
}

define i32 @_EN4main4vec2ixE3int(%vec2* %this, i32 %index) {
  ret i32 %index
}
