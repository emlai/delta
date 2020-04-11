
define i32 @main() {
  %foo = alloca i32
  store i32 0, i32* %foo
  %foo.load = load i32, i32* %foo
  %1 = add i32 %foo.load, -1
  store i32 %1, i32* %foo
  store i32 666, i32* %foo
  %foo.load1 = load i32, i32* %foo
  %2 = add i32 %foo.load1, 1
  store i32 %2, i32* %foo
  ret i32 0
}
