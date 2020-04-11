
%Foo = type { float }

define i32 @main() {
  %f = alloca %Foo
  call void @_EN4main3Foo4initE3int(%Foo* %f, i32 5)
  call void @_EN4main3Foo6deinitE(%Foo* %f)
  ret i32 0
}

define void @_EN4main3Foo4initE3int(%Foo* %this, i32 %i) {
  %f = getelementptr inbounds %Foo, %Foo* %this, i32 0, i32 0
  store float 0.000000e+00, float* %f
  ret void
}

define void @_EN4main3Foo6deinitE(%Foo* %this) {
  ret void
}
