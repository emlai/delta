
%Foo = type { i32 }

define void @_EN4main3ptrEP3Foo(%Foo* %p) {
  ret void
}

define i32 @main() {
  %f = alloca %Foo
  call void @_EN4main3ptrEP3Foo(%Foo* %f)
  ret i32 0
}
