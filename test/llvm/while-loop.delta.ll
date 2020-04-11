
declare i1 @foo()

define i32 @main() {
  br label %loop.condition

loop.condition:                                   ; preds = %loop.body, %0
  %1 = call i1 @foo()
  br i1 %1, label %loop.body, label %loop.end

loop.body:                                        ; preds = %loop.condition
  %2 = call i1 @foo()
  br label %loop.condition

loop.end:                                         ; preds = %loop.condition
  ret i32 0
}
