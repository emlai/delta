
define void @_EN4main1fEP4char(i8* %pb) {
  %b = alloca i1
  store i1 true, i1* %b
  %pb.load = load i8, i8* %pb
  %1 = icmp ne i8 %pb.load, 0
  store i1 %1, i1* %b
  %b.load = load i1, i1* %b
  %2 = icmp ne i1 %b.load, false
  ret void
}
