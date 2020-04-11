
declare void @foo()

declare void @bar()

define i32 @main() {
  br i1 false, label %if.then, label %if.else

if.then:                                          ; preds = %0
  call void @foo()
  br label %if.end

if.else:                                          ; preds = %0
  call void @bar()
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  ret i32 0
}
