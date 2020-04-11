
define i32* @_EN4main3fooE() {
  ret i32* null
}

define i32 @main() {
  %isNull = alloca i1
  %isNonNull = alloca i1
  %ptr = alloca i32*
  %ptr2 = alloca i1*
  %1 = call i32* @_EN4main3fooE()
  %2 = icmp eq i32* %1, null
  store i1 %2, i1* %isNull
  %3 = call i32* @_EN4main3fooE()
  %4 = icmp ne i32* %3, null
  store i1 %4, i1* %isNonNull
  %5 = call i32* @_EN4main3fooE()
  store i32* %5, i32** %ptr
  store i1* null, i1** %ptr2
  ret i32 0
}
