
%string = type { %"ArrayRef<char>" }
%"ArrayRef<char>" = type { i8*, i32 }

@0 = private unnamed_addr constant [3 x i8] c"\5Cn\00", align 1

define i32 @main() {
  %__str = alloca %string
  call void @_EN3std6string4initEP4char3int(%string* %__str, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @0, i32 0, i32 0), i32 2)
  ret i32 0
}

declare void @_EN3std6string4initEP4char3int(%string*, i8*, i32)
