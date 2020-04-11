
%StringBuffer = type { %"List<char>" }
%"List<char>" = type { i8*, i32, i32 }
%string = type { %"ArrayRef<char>" }
%"ArrayRef<char>" = type { i8*, i32 }

@0 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1

define i32 @main() {
  %a = alloca %StringBuffer
  %__str = alloca %string
  call void @_EN3std12StringBuffer4initE(%StringBuffer* %a)
  %a.load = load %StringBuffer, %StringBuffer* %a
  call void @_EN3std6string4initEP4char3int(%string* %__str, i8* getelementptr inbounds ([1 x i8], [1 x i8]* @0, i32 0, i32 0), i32 0)
  %__str.load = load %string, %string* %__str
  %1 = call %StringBuffer @_EN3stdplE12StringBuffer6string(%StringBuffer %a.load, %string %__str.load)
  store %StringBuffer %1, %StringBuffer* %a
  call void @_EN3std12StringBuffer6deinitE(%StringBuffer* %a)
  ret i32 0
}

declare void @_EN3std12StringBuffer4initE(%StringBuffer*)

declare void @_EN3std6string4initEP4char3int(%string*, i8*, i32)

declare %StringBuffer @_EN3stdplE12StringBuffer6string(%StringBuffer, %string)

declare void @_EN3std12StringBuffer6deinitE(%StringBuffer*)
