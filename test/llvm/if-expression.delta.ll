
declare i1 @a()

declare i1 @b()

declare i1 @c()

declare i1 @d()

declare i1 @e()

define i32 @main() {
  %x = alloca i1
  %1 = call i1 @a()
  br i1 %1, label %if.then, label %if.else

if.then:                                          ; preds = %0
  %2 = call i1 @b()
  br label %if.end3

if.else:                                          ; preds = %0
  %3 = call i1 @c()
  br i1 %3, label %if.then1, label %if.else2

if.then1:                                         ; preds = %if.else
  %4 = call i1 @d()
  br label %if.end

if.else2:                                         ; preds = %if.else
  %5 = call i1 @e()
  br label %if.end

if.end:                                           ; preds = %if.else2, %if.then1
  %phi = phi i1 [ %4, %if.then1 ], [ %5, %if.else2 ]
  br label %if.end3

if.end3:                                          ; preds = %if.end, %if.then
  %phi4 = phi i1 [ %2, %if.then ], [ %phi, %if.end ]
  store i1 %phi4, i1* %x
  ret i32 0
}
