
%string = type { %"ArrayRef<char>" }
%"ArrayRef<char>" = type { i8*, i32 }

@0 = private unnamed_addr constant [4 x i8] c"foo\00", align 1
@1 = private unnamed_addr constant [4 x i8] c"bar\00", align 1

define i32 @main() {
  %a = alloca [2 x %string]
  %__str = alloca %string
  %__str1 = alloca %string
  call void @_EN3std6string4initEP4char3int(%string* %__str, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @0, i32 0, i32 0), i32 3)
  %__str.load = load %string, %string* %__str
  %1 = insertvalue [2 x %string] undef, %string %__str.load, 0
  call void @_EN3std6string4initEP4char3int(%string* %__str1, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @1, i32 0, i32 0), i32 3)
  %__str.load2 = load %string, %string* %__str1
  %2 = insertvalue [2 x %string] %1, %string %__str.load2, 1
  store [2 x %string] %2, [2 x %string]* %a
  ret i32 0
}

declare void @_EN3std6string4initEP4char3int(%string*, i8*, i32)
