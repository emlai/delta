
%Foo = type { i32 }
%Bar = type { i32, i32 }

define i32 @main() {
  %foo = alloca %Foo
  %bar = alloca %Bar
  call void @_EN4main3Foo4initE(%Foo* %foo)
  call void @_EN4main3Bar4initE3int(%Bar* %bar, i32 -1)
  ret i32 0
}

define void @_EN4main3Foo4initE(%Foo* %this) {
  %i = getelementptr inbounds %Foo, %Foo* %this, i32 0, i32 0
  store i32 42, i32* %i
  ret void
}

define void @_EN4main3Bar4initE3int(%Bar* %this, i32 %j) {
  %i = getelementptr inbounds %Bar, %Bar* %this, i32 0, i32 0
  store i32 42, i32* %i
  %j1 = getelementptr inbounds %Bar, %Bar* %this, i32 0, i32 1
  store i32 %j, i32* %j1
  ret void
}
