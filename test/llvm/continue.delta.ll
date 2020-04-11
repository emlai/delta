
define void @_EN4main3bazE4bool(i1 %foo) {
  br label %loop.condition

loop.condition:                                   ; preds = %if.end, %if.then, %0
  br i1 %foo, label %loop.body, label %loop.end

loop.body:                                        ; preds = %loop.condition
  br i1 %foo, label %if.then, label %if.else

loop.end:                                         ; preds = %loop.condition
  ret void

if.then:                                          ; preds = %loop.body
  call void @_EN4main3bazE4bool(i1 %foo)
  br label %loop.condition

if.else:                                          ; preds = %loop.body
  br label %if.end

if.end:                                           ; preds = %if.else
  br label %loop.condition
}
