
declare void @f(i32*)

define void @_EN4main1aEP3int(i32* %p) {
  %q = alloca i32*
  store i32* %p, i32** %q
  %q.load = load i32*, i32** %q
  call void @f(i32* %q.load)
  call void @f(i32* %p)
  ret void
}
