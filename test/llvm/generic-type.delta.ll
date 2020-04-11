
%"A<int>" = type { i32 }
%"A<A<A<bool>>>" = type { %"A<A<bool>>" }
%"A<A<bool>>" = type { %"A<bool>" }
%"A<bool>" = type { i1 }

define i32 @main() {
  %a = alloca %"A<int>"
  %aaa = alloca %"A<A<A<bool>>>"
  ret i32 0
}
