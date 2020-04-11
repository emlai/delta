
%X = type { i32 }
%Y = type { i32 }

define i1 @_EN4mainltEP1XP1X(%X* %a, %X* %b) {
  ret i1 true
}

define void @_EN4main2fxEP1XP1X(%X* %a, %X* %b) {
  %1 = call i1 @_EN4mainltEP1XP1X(%X* %a, %X* %b)
  %2 = call i1 @_EN4mainltEP1XP1X(%X* %a, %X* %b)
  ret void
}

define void @_EN4main2fyEP1YP1Y(%Y* %a, %Y* %b) {
  %address = ptrtoint %Y* %a to i64
  %address1 = ptrtoint %Y* %b to i64
  %1 = icmp ult i64 %address, %address1
  ret void
}
