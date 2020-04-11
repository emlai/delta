
define i32 @main() {
  %foo = alloca i32
  %bar = alloca i32
  %qux = alloca i64
  %qux2 = alloca i8
  %qux3 = alloca i32
  %qux4 = alloca i32
  store i32 -1, i32* %foo
  store i32 0, i32* %bar
  store i64 0, i64* %qux
  store i8 0, i8* %qux2
  store i32 -1, i32* %qux3
  %qux3.load = load i32, i32* %qux3
  store i32 %qux3.load, i32* %qux4
  ret i32 0
}
