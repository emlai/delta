
%Foo = type { i32 }
%Bar = type { i32 }

define void @_EN4main3quxEP3FooP3Bar(%Foo* %f, %Bar* %b) {
  call void @_EN4main3Foo3fooE(%Foo* %f)
  call void @_EN4main3Bar3barE(%Bar* %b)
  ret void
}

define void @_EN4main3Foo3fooE(%Foo* %this) {
  ret void
}

define void @_EN4main3Bar3barE(%Bar* %this) {
  ret void
}

define i32 @main() {
  %f = alloca %Foo
  %b = alloca %Bar
  call void @_EN4main3Foo4initE(%Foo* %f)
  call void @_EN4main3Bar4initE(%Bar* %b)
  call void @_EN4main3quxEP3FooP3Bar(%Foo* %f, %Bar* %b)
  ret i32 0
}

define void @_EN4main3Foo4initE(%Foo* %this) {
  %i = getelementptr inbounds %Foo, %Foo* %this, i32 0, i32 0
  store i32 0, i32* %i
  ret void
}

define void @_EN4main3Bar4initE(%Bar* %this) {
  %i = getelementptr inbounds %Bar, %Bar* %this, i32 0, i32 0
  store i32 0, i32* %i
  ret void
}
