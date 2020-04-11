
%Foo = type { i32 }

define i32 @main() {
  %f = alloca %Foo
  %rf = alloca %Foo*
  %pf = alloca %Foo*
  store %Foo* %f, %Foo** %rf
  store %Foo* %f, %Foo** %pf
  ret i32 0
}
