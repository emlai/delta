
define i32 @main() {
  %p = alloca i32*
  %o = alloca i32*
  %p.load = load i32*, i32** %p
  store i32* %p.load, i32** %o
  ret i32 0
}
