
define i32 @main() {
  br label %loop.condition

loop.condition:                                   ; preds = %0
  br i1 true, label %loop.body, label %loop.end

loop.body:                                        ; preds = %loop.condition
  br i1 true, label %if.then, label %if.else

loop.end:                                         ; preds = %if.end, %if.then, %loop.condition
  switch i32 1, label %switch.default [
  ]

if.then:                                          ; preds = %loop.body
  br label %loop.end

if.else:                                          ; preds = %loop.body
  br label %if.end

if.end:                                           ; preds = %if.else
  br label %loop.end

switch.default:                                   ; preds = %loop.end
  br label %switch.end

switch.end:                                       ; preds = %switch.default
  ret i32 0
}
