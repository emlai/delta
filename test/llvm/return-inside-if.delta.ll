
declare i1 @foo()

define void @_EN4main3barE() {
  %1 = call i1 @foo()
  br i1 %1, label %if.then, label %if.else

if.then:                                          ; preds = %0
  ret void

if.else:                                          ; preds = %0
  ret void

if.end:                                           ; No predecessors!
  %2 = call i1 @foo()
  br i1 %2, label %if.then1, label %if.else2

if.then1:                                         ; preds = %if.end
  ret void

if.else2:                                         ; preds = %if.end
  ret void

if.end3:                                          ; No predecessors!
  br label %loop.condition

loop.condition:                                   ; preds = %if.end3
  %3 = call i1 @foo()
  br i1 %3, label %loop.body, label %loop.end

loop.body:                                        ; preds = %loop.condition
  ret void

loop.end:                                         ; preds = %loop.condition
  br label %loop.condition4

loop.condition4:                                  ; preds = %loop.end
  %4 = call i1 @foo()
  br i1 %4, label %loop.body5, label %loop.end6

loop.body5:                                       ; preds = %loop.condition4
  ret void

loop.end6:                                        ; preds = %loop.condition4
  ret void
}
