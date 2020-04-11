
@0 = private unnamed_addr constant [36 x i8] c"Unwrap failed at unwrap.delta:6:37\0A\00", align 1
@1 = private unnamed_addr constant [35 x i8] c"Unwrap failed at unwrap.delta:8:8\0A\00", align 1

declare i8* @f()

define i32 @main() {
  %byte = alloca i8*
  %ptr = alloca [1 x i8]*
  %1 = call i8* @f()
  %assert.condition = icmp eq i8* %1, null
  br i1 %assert.condition, label %assert.fail, label %assert.success

assert.fail:                                      ; preds = %0
  call void @_EN3std10assertFailEP4char(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @0, i32 0, i32 0))
  unreachable

assert.success:                                   ; preds = %0
  store i8* %1, i8** %byte
  %2 = call i8* @f()
  %3 = bitcast i8* %2 to [1 x i8]*
  store [1 x i8]* %3, [1 x i8]** %ptr
  %ptr.load = load [1 x i8]*, [1 x i8]** %ptr
  %assert.condition1 = icmp eq [1 x i8]* %ptr.load, null
  br i1 %assert.condition1, label %assert.fail2, label %assert.success3

assert.fail2:                                     ; preds = %assert.success
  call void @_EN3std10assertFailEP4char(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @1, i32 0, i32 0))
  unreachable

assert.success3:                                  ; preds = %assert.success
  %4 = getelementptr inbounds [1 x i8], [1 x i8]* %ptr.load, i32 0, i32 0
  store i8 1, i8* %4
  ret i32 0
}

declare void @_EN3std10assertFailEP4char(i8*)
