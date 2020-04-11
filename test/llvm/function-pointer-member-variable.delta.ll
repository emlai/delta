
%X = type { i32 (i32)* }

define i32 @_EN4main3fooE3int(i32 %a) {
  ret i32 %a
}

define i32 @main() {
  %x = alloca %X
  call void @_EN4main1X4initEF3int_3int(%X* %x, i32 (i32)* @_EN4main3fooE3int)
  %p = getelementptr inbounds %X, %X* %x, i32 0, i32 0
  %p.load = load i32 (i32)*, i32 (i32)** %p
  %1 = call i32 %p.load(i32 42)
  call void @_EN4main1X1fE(%X* %x)
  ret i32 0
}

define void @_EN4main1X4initEF3int_3int(%X* %this, i32 (i32)* %p) {
  %p1 = getelementptr inbounds %X, %X* %this, i32 0, i32 0
  store i32 (i32)* %p, i32 (i32)** %p1
  ret void
}

define void @_EN4main1X1fE(%X* %this) {
  %p2 = alloca i32 (i32)*
  %p = getelementptr inbounds %X, %X* %this, i32 0, i32 0
  %p.load = load i32 (i32)*, i32 (i32)** %p
  %1 = call i32 %p.load(i32 42)
  %p1 = getelementptr inbounds %X, %X* %this, i32 0, i32 0
  %p.load2 = load i32 (i32)*, i32 (i32)** %p1
  %2 = call i32 %p.load2(i32 42)
  %p3 = getelementptr inbounds %X, %X* %this, i32 0, i32 0
  %p.load4 = load i32 (i32)*, i32 (i32)** %p3
  store i32 (i32)* %p.load4, i32 (i32)** %p2
  %p2.load = load i32 (i32)*, i32 (i32)** %p2
  %3 = call i32 %p2.load(i32 42)
  ret void
}
