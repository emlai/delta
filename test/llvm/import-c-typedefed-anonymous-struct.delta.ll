
%Foo = type { i32 }

define i32 @main() {
  %x = alloca %Foo
  %x.load = load %Foo, %Foo* %x
  call void @foo(%Foo %x.load)
  ret i32 0
}

declare void @foo(%Foo)
