
define i8 @_EN4main3fooE4bool(i1 %b) {
  br i1 %b, label %if.then, label %if.else

if.then:                                          ; preds = %0
  br label %if.end

if.else:                                          ; preds = %0
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  %phi = phi i8 [ -1, %if.then ], [ -18, %if.else ]
  ret i8 %phi
}
