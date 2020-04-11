
%S = type { i32 }

define void @_EN4main3fooE3int(i32 %i) {
  switch i32 %i, label %switch.default [
    i32 0, label %switch.case.0
    i32 -1, label %switch.case.1
    i32 42, label %switch.case.2
  ]

switch.case.0:                                    ; preds = %0
  br label %switch.end

switch.case.1:                                    ; preds = %0
  call void @_EN4main3fooE3int(i32 %i)
  call void @_EN4main3fooE3int(i32 %i)
  br label %switch.end

switch.case.2:                                    ; preds = %0
  br label %switch.end

switch.default:                                   ; preds = %0
  br label %switch.end

switch.end:                                       ; preds = %switch.default, %switch.case.2, %switch.case.1, %switch.case.0
  switch i32 %i, label %switch.default2 [
    i32 0, label %switch.case.01
  ]

switch.case.01:                                   ; preds = %switch.end
  br label %switch.end3

switch.default2:                                  ; preds = %switch.end
  call void @_EN4main3fooE3int(i32 %i)
  br label %switch.end3

switch.end3:                                      ; preds = %switch.default2, %switch.case.01
  ret void
}

define i32 @_EN4main1fEP1S(%S* %s) {
  %i = getelementptr inbounds %S, %S* %s, i32 0, i32 0
  %i.load = load i32, i32* %i
  switch i32 %i.load, label %switch.default [
    i32 0, label %switch.case.0
  ]

switch.case.0:                                    ; preds = %0
  ret i32 0

switch.default:                                   ; preds = %0
  ret i32 1
}
