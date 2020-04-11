
%Foo = type { i32, i8* }

@0 = private unnamed_addr constant [8 x i8] c"foo bar\00", align 1
@1 = private unnamed_addr constant [4 x i8] c"foo\00", align 1

define i32 @main() {
  %f = alloca %Foo
  %bar = alloca i32
  %1 = call i32 @puts(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @0, i32 0, i32 0))
  %bar1 = getelementptr inbounds %Foo, %Foo* %f, i32 0, i32 0
  store i32 47, i32* %bar1
  %baz = getelementptr inbounds %Foo, %Foo* %f, i32 0, i32 1
  store i8* getelementptr inbounds ([4 x i8], [4 x i8]* @1, i32 0, i32 0), i8** %baz
  %2 = call i32 @getBar(%Foo* %f)
  store i32 %2, i32* %bar
  ret i32 0
}

declare i32 @puts(i8*)

declare i32 @getBar(%Foo*)
