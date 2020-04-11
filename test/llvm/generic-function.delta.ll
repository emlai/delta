
%string = type { %"ArrayRef<char>" }
%"ArrayRef<char>" = type { i8*, i32 }

@0 = private unnamed_addr constant [4 x i8] c"bar\00", align 1

define i32 @main() {
  %b = alloca %string
  %__str = alloca %string
  %five = alloca i32
  call void @_EN4main3fooI3intEE3int(i32 1)
  call void @_EN4main3fooI4boolEE4bool(i1 false)
  call void @_EN4main3fooI4boolEE4bool(i1 true)
  call void @_EN3std6string4initEP4char3int(%string* %__str, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @0, i32 0, i32 0), i32 3)
  %__str.load = load %string, %string* %__str
  %1 = call %string @_EN4main3barI6stringEE6string(%string %__str.load)
  store %string %1, %string* %b
  %2 = call i32 @_EN4main3quxI3intEE3int(i32 -5)
  store i32 %2, i32* %five
  ret i32 0
}

define void @_EN4main3fooI3intEE3int(i32 %t) {
  ret void
}

define void @_EN4main3fooI4boolEE4bool(i1 %t) {
  ret void
}

declare void @_EN3std6string4initEP4char3int(%string*, i8*, i32)

define %string @_EN4main3barI6stringEE6string(%string %t) {
  ret %string %t
}

define i32 @_EN4main3quxI3intEE3int(i32 %t) {
  %1 = icmp slt i32 %t, 0
  br i1 %1, label %if.then, label %if.else

if.then:                                          ; preds = %0
  %2 = sub i32 0, %t
  ret i32 %2

if.else:                                          ; preds = %0
  br label %if.end

if.end:                                           ; preds = %if.else
  ret i32 %t
}
