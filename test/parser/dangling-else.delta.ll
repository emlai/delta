
define i32 @main() {
  br i1 true, label %if.then, label %if.else

if.then:                                          ; preds = %0
  br i1 false, label %if.then1, label %if.else2

if.else:                                          ; preds = %0
  br label %if.end

if.end:                                           ; preds = %if.else
  ret i32 2

if.then1:                                         ; preds = %if.then
  ret i32 1

if.else2:                                         ; preds = %if.then
  ret i32 0
}
