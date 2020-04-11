
define i32 @main() {
  %sum = alloca i32
  %i = alloca i32
  store i32 0, i32* %sum
  store i32 3, i32* %i
  br label %loop.condition

loop.condition:                                   ; preds = %loop.increment, %0
  %i.load = load i32, i32* %i
  %1 = icmp slt i32 %i.load, 9
  br i1 %1, label %loop.body, label %loop.end

loop.body:                                        ; preds = %loop.condition
  %sum.load = load i32, i32* %sum
  %i.load1 = load i32, i32* %i
  %2 = add i32 %sum.load, %i.load1
  store i32 %2, i32* %sum
  br label %loop.increment

loop.increment:                                   ; preds = %loop.body
  %i.load2 = load i32, i32* %i
  %3 = add i32 %i.load2, 1
  store i32 %3, i32* %i
  br label %loop.condition

loop.end:                                         ; preds = %loop.condition
  ret i32 0
}
