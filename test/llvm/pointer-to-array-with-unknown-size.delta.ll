
define i32 @main() {
  %a = alloca [3 x i32]
  %p = alloca i32*
  %t = alloca i32
  store [3 x i32] [i32 1, i32 2, i32 3], [3 x i32]* %a
  %1 = getelementptr inbounds [3 x i32], [3 x i32]* %a, i32 0, i32 0
  store i32* %1, i32** %p
  %2 = bitcast [3 x i32]* %a to i32*
  store i32* %2, i32** %p
  %3 = bitcast [3 x i32]* %a to i32*
  store i32* %3, i32** %p
  %p.load = load i32*, i32** %p
  %4 = getelementptr inbounds i32, i32* %p.load, i32 1
  %.load = load i32, i32* %4
  store i32 %.load, i32* %t
  ret i32 0
}
