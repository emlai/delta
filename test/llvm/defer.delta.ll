
declare void @foo()

declare void @bar()

define i32 @main() {
  br i1 false, label %if.then, label %if.else

if.then:                                          ; preds = %0
  call void @bar()
  call void @foo()
  ret i32 0

if.else:                                          ; preds = %0
  br label %if.end

if.end:                                           ; preds = %if.else
  br i1 false, label %if.then1, label %if.else2

if.then1:                                         ; preds = %if.end
  call void @bar()
  br label %if.end3

if.else2:                                         ; preds = %if.end
  br label %if.end3

if.end3:                                          ; preds = %if.else2, %if.then1
  br label %loop.condition

loop.condition:                                   ; preds = %loop.body, %if.end3
  br i1 false, label %loop.body, label %loop.end

loop.body:                                        ; preds = %loop.condition
  call void @bar()
  call void @foo()
  br label %loop.condition

loop.end:                                         ; preds = %loop.condition
  br i1 true, label %if.then4, label %if.else5

if.then4:                                         ; preds = %loop.end
  call void @bar()
  call void @foo()
  ret i32 0

if.else5:                                         ; preds = %loop.end
  br label %if.end6

if.end6:                                          ; preds = %if.else5
  call void @foo()
  call void @bar()
  call void @foo()
  ret i32 0
}

define void @_EN4main13shouldBeEmptyE() {
  ret void
}
