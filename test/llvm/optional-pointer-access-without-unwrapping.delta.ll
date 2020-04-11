
%Foo = type { i32 }

define i32 @main() {
  %f = alloca %Foo*
  %a = alloca i32
  %b = alloca i32*
  %c = alloca i32*
  store %Foo* null, %Foo** %f
  %f.load = load %Foo*, %Foo** %f
  %i = getelementptr inbounds %Foo, %Foo* %f.load, i32 0, i32 0
  %f.load1 = load %Foo*, %Foo** %f
  %i2 = getelementptr inbounds %Foo, %Foo* %f.load1, i32 0, i32 0
  %i.load = load i32, i32* %i2
  store i32 %i.load, i32* %a
  %f.load3 = load %Foo*, %Foo** %f
  %i4 = getelementptr inbounds %Foo, %Foo* %f.load3, i32 0, i32 0
  store i32* %i4, i32** %b
  %f.load5 = load %Foo*, %Foo** %f
  %i6 = getelementptr inbounds %Foo, %Foo* %f.load5, i32 0, i32 0
  store i32* %i6, i32** %c
  %f.load7 = load %Foo*, %Foo** %f
  call void @_EN4main3Foo3barE(%Foo* %f.load7)
  %f.load8 = load %Foo*, %Foo** %f
  %f.load9 = load %Foo*, %Foo** %f
  %f.load.load = load %Foo, %Foo* %f.load9
  store %Foo %f.load.load, %Foo* %f.load8
  ret i32 0
}

define void @_EN4main3Foo3barE(%Foo* %this) {
  ret void
}
