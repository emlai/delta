
define i32 @main() {
  %lambda = alloca i32 ({}*)*
  store i32 ({}*)* @_EN4main9__lambda0EP1X, i32 ({}*)** %lambda
  ret i32 0
}

define i32 @_EN4main9__lambda0EP1X({}* %x) {
  %1 = call i32 @_EN4main1X3fooE({}* %x)
  ret i32 %1
}

define i32 @_EN4main1X3fooE({}* %this) {
  ret i32 42
}
