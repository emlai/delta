
define void @_EN4main1fEP3int(i32* %a) {
  %p = alloca i32*
  store i32* %a, i32** %p
  %p.load = load i32*, i32** %p
  %p.load.load = load i32, i32* %p.load
  %1 = add i32 %p.load.load, 1
  store i32 %1, i32* %p.load
  %a.load = load i32, i32* %a
  %2 = add i32 %a.load, -1
  store i32 %2, i32* %a
  ret void
}
