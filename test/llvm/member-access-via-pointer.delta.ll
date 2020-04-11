
%Foo = type { i32 }

define void @_EN4main3getEP3Foo(%Foo* %f) {
  %m = alloca i32
  %i = getelementptr inbounds %Foo, %Foo* %f, i32 0, i32 0
  %i.load = load i32, i32* %i
  store i32 %i.load, i32* %m
  ret void
}

define i32 @main() {
  %f = alloca %Foo
  %rf = alloca %Foo*
  %n = alloca i32
  store %Foo* %f, %Foo** %rf
  %rf.load = load %Foo*, %Foo** %rf
  %i = getelementptr inbounds %Foo, %Foo* %rf.load, i32 0, i32 0
  %i.load = load i32, i32* %i
  store i32 %i.load, i32* %n
  %rf.load1 = load %Foo*, %Foo** %rf
  call void @_EN4main3getEP3Foo(%Foo* %rf.load1)
  ret i32 0
}
