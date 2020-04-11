
define void @_EN4main3fooEOP3intP3int(i32* %a, i32* %b) {
  %1 = icmp eq i32* %a, %b
  %2 = icmp ne i32* %a, %b
  ret void
}
