
%Foo = type { i32 }

define i32 @main() {
  %x = alloca %Foo, align 8
  %x.load = load %Foo, %Foo* %x, align 4
  call void @foo(%Foo %x.load)
  ret i32 0
}

declare void @foo(%Foo)
