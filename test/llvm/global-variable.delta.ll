
@foo = private global i32 42
@qux = private global i32 42
@bar = private global i32 42

define i32 @main() {
  %d = alloca i32
  store i32 43, i32* @foo
  %bar.load = load i32, i32* @bar
  store i32 %bar.load, i32* %d
  ret i32 0
}
