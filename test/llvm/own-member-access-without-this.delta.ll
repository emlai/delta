
%A = type { i32, i32 }
%B = type { i32, i32 }

define i32 @main() {
  %a = alloca %A
  %b = alloca %B
  call void @_EN4main1A4initE(%A* %a)
  %1 = call i32 @_EN4main1A3fooE(%A* %a)
  call void @_EN4main1A3barE(%A* %a)
  call void @_EN4main1B4initE(%B* %b)
  %2 = call i32 @_EN4main1B3fooE(%B* %b)
  call void @_EN4main1B3barE(%B* %b)
  ret i32 0
}

define void @_EN4main1A4initE(%A* %this) {
  %j = getelementptr inbounds %A, %A* %this, i32 0, i32 1
  store i32 42, i32* %j
  ret void
}

define i32 @_EN4main1A3fooE(%A* %this) {
  %j = getelementptr inbounds %A, %A* %this, i32 0, i32 1
  %j.load = load i32, i32* %j
  ret i32 %j.load
}

define void @_EN4main1A3barE(%A* %this) {
  %j = getelementptr inbounds %A, %A* %this, i32 0, i32 1
  store i32 1, i32* %j
  ret void
}

define void @_EN4main1B4initE(%B* %this) {
  %j = getelementptr inbounds %B, %B* %this, i32 0, i32 1
  store i32 42, i32* %j
  ret void
}

define i32 @_EN4main1B3fooE(%B* %this) {
  %j = getelementptr inbounds %B, %B* %this, i32 0, i32 1
  %j.load = load i32, i32* %j
  ret i32 %j.load
}

define void @_EN4main1B3barE(%B* %this) {
  %j = getelementptr inbounds %B, %B* %this, i32 0, i32 1
  store i32 1, i32* %j
  ret void
}
