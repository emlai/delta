
%string = type { %"ArrayRef<char>" }
%"ArrayRef<char>" = type { i8*, i32 }

@0 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1

define i32 @main() {
  %__str = alloca %string
  call void @_EN3std6string4initEP4char3int(%string* %__str, i8* getelementptr inbounds ([1 x i8], [1 x i8]* @0, i32 0, i32 0), i32 0)
  %1 = call i32 @_EN3std6string4sizeE(%string* %__str)
  ret i32 %1
}

declare void @_EN3std6string4initEP4char3int(%string*, i8*, i32)

declare i32 @_EN3std6string4sizeE(%string*)
