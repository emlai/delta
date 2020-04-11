
%Foo = type { i32 }

define void @_EN4main3fooEP3Foo(%Foo* %ref_f) {
  ret void
}

define i32 @main() {
  %f = alloca %Foo
  call void @_EN4main3fooEP3Foo(%Foo* %f)
  ret i32 0
}
