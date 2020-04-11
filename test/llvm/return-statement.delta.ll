
define i32 @main() {
  %1 = call i32 @_EN4main3fooE()
  ret i32 %1
}

define i32 @_EN4main3fooE() {
  %1 = call i32 @_EN4main3fooE()
  ret i32 42
}
