
define void @_EN4main3fooEOP3int(i32* %p) {
  ret void
}

define i32 @main() {
  %i = alloca i32
  %x = alloca i32*
  store i32 42, i32* %i
  store i32* %i, i32** %x
  %x.load = load i32*, i32** %x
  call void @_EN4main3fooEOP3int(i32* %x.load)
  call void @_EN4main3fooEOP3int(i32* %i)
  ret i32 0
}
