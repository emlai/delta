
define void @_EN4main3fooE3int(i32 %i) {
  switch i32 %i, label %switch.default [
    i32 42, label %switch.case.0
  ]

switch.case.0:                                    ; preds = %0
  ret void

switch.default:                                   ; preds = %0
  br label %switch.end

switch.end:                                       ; preds = %switch.default
  ret void
}
