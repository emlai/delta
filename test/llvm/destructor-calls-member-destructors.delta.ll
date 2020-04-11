
%A = type { %B, %B, i32 }
%B = type { i32 }
%C = type { %B, %B }

declare i1 @f()

define i32 @main() {
  %a = alloca %A
  %c = alloca %C
  call void @_EN4main1A4initE(%A* %a)
  call void @_EN4main1C4initE(%C* %c)
  call void @_EN4main1C6deinitE(%C* %c)
  call void @_EN4main1A6deinitE(%A* %a)
  ret i32 0
}

define void @_EN4main1A4initE(%A* %this) {
  ret void
}

define void @_EN4main1C4initE(%C* %this) {
  ret void
}

define void @_EN4main1C6deinitE(%C* %this) {
  %b = getelementptr inbounds %C, %C* %this, i32 0, i32 0
  %bb = getelementptr inbounds %C, %C* %this, i32 0, i32 1
  call void @_EN4main1B6deinitE(%B* %bb)
  call void @_EN4main1B6deinitE(%B* %b)
  ret void
}

define void @_EN4main1A6deinitE(%A* %this) {
  %b = getelementptr inbounds %A, %A* %this, i32 0, i32 0
  %bb = getelementptr inbounds %A, %A* %this, i32 0, i32 1
  %1 = call i1 @f()
  br i1 %1, label %if.then, label %if.else

if.then:                                          ; preds = %0
  call void @_EN4main1B6deinitE(%B* %bb)
  call void @_EN4main1B6deinitE(%B* %b)
  ret void

if.else:                                          ; preds = %0
  br label %if.end

if.end:                                           ; preds = %if.else
  call void @_EN4main1B6deinitE(%B* %bb)
  call void @_EN4main1B6deinitE(%B* %b)
  ret void
}

define void @_EN4main1B6deinitE(%B* %this) {
  ret void
}
