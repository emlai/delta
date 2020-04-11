
@0 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@1 = private unnamed_addr constant [58 x i8] c"Unwrap failed at pointer-to-array-of-pointers.delta:8:21\0A\00", align 1

declare i8** @b()

define i32 @main() {
  %s = alloca i8**
  %i = alloca i32
  %1 = call i8** @b()
  store i8** %1, i8*** %s
  store i32 0, i32* %i
  %s.load = load i8**, i8*** %s
  %assert.condition = icmp eq i8** %s.load, null
  br i1 %assert.condition, label %assert.fail, label %assert.success

assert.fail:                                      ; preds = %0
  call void @_EN3std10assertFailEP4char(i8* getelementptr inbounds ([58 x i8], [58 x i8]* @1, i32 0, i32 0))
  unreachable

assert.success:                                   ; preds = %0
  %i.load = load i32, i32* %i
  %2 = getelementptr inbounds i8*, i8** %s.load, i32 %i.load
  %.load = load i8*, i8** %2
  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @0, i32 0, i32 0), i8* %.load)
  ret i32 0
}

declare void @_EN3std10assertFailEP4char(i8*)

declare i32 @printf(i8*, ...)
