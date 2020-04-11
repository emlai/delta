
%"Optional<int>" = type { i1, i32 }

@0 = private unnamed_addr constant [56 x i8] c"Unwrap failed at optional-type-non-pointer.delta:25:16\0A\00", align 1

define i32 @main() {
  %a = alloca %"Optional<int>"
  %1 = alloca %"Optional<int>"
  %b = alloca %"Optional<int>"
  %c = alloca %"Optional<int>"
  %2 = alloca %"Optional<int>"
  %d = alloca %"Optional<int>"
  %3 = alloca %"Optional<int>"
  %4 = alloca %"Optional<int>"
  call void @_EN3std8OptionalI3intE4initE(%"Optional<int>"* %1)
  %.load = load %"Optional<int>", %"Optional<int>"* %1
  store %"Optional<int>" %.load, %"Optional<int>"* %a
  call void @_EN3std8OptionalI3intE4initE3int(%"Optional<int>"* %b, i32 2)
  call void @_EN3std8OptionalI3intE4initE3int(%"Optional<int>"* %2, i32 3)
  %.load1 = load %"Optional<int>", %"Optional<int>"* %2
  store %"Optional<int>" %.load1, %"Optional<int>"* %c
  call void @_EN3std8OptionalI3intE4initE(%"Optional<int>"* %d)
  call void @_EN3std8OptionalI3intE4initE(%"Optional<int>"* %3)
  %5 = call i1 @_EN3stdeqI3intEEPO3intPO3int(%"Optional<int>"* %d, %"Optional<int>"* %3)
  br i1 %5, label %if.then, label %if.else

if.then:                                          ; preds = %0
  br label %if.end

if.else:                                          ; preds = %0
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  call void @_EN3std8OptionalI3intE4initE(%"Optional<int>"* %4)
  %6 = call i1 @_EN3stdneI3intEEPO3intPO3int(%"Optional<int>"* %d, %"Optional<int>"* %4)
  br i1 %6, label %if.then2, label %if.else3

if.then2:                                         ; preds = %if.end
  br label %if.end4

if.else3:                                         ; preds = %if.end
  br label %if.end4

if.end4:                                          ; preds = %if.else3, %if.then2
  %d.load = load %"Optional<int>", %"Optional<int>"* %d
  %7 = extractvalue %"Optional<int>" %d.load, 0
  br i1 %7, label %if.then5, label %if.else6

if.then5:                                         ; preds = %if.end4
  br label %if.end7

if.else6:                                         ; preds = %if.end4
  br label %if.end7

if.end7:                                          ; preds = %if.else6, %if.then5
  %d.load8 = load %"Optional<int>", %"Optional<int>"* %d
  %8 = extractvalue %"Optional<int>" %d.load8, 0
  %9 = xor i1 %8, true
  br i1 %9, label %if.then9, label %if.else10

if.then9:                                         ; preds = %if.end7
  br label %if.end11

if.else10:                                        ; preds = %if.end7
  br label %if.end11

if.end11:                                         ; preds = %if.else10, %if.then9
  ret i32 0
}

define void @_EN3std8OptionalI3intE4initE(%"Optional<int>"* %this) {
  %hasValue = getelementptr inbounds %"Optional<int>", %"Optional<int>"* %this, i32 0, i32 0
  store i1 false, i1* %hasValue
  ret void
}

define void @_EN3std8OptionalI3intE4initE3int(%"Optional<int>"* %this, i32 %value) {
  %hasValue = getelementptr inbounds %"Optional<int>", %"Optional<int>"* %this, i32 0, i32 0
  store i1 true, i1* %hasValue
  %value1 = getelementptr inbounds %"Optional<int>", %"Optional<int>"* %this, i32 0, i32 1
  store i32 %value, i32* %value1
  ret void
}

define i1 @_EN3stdeqI3intEEPO3intPO3int(%"Optional<int>"* %a, %"Optional<int>"* %b) {
  %hasValue = getelementptr inbounds %"Optional<int>", %"Optional<int>"* %a, i32 0, i32 0
  %hasValue.load = load i1, i1* %hasValue
  %hasValue1 = getelementptr inbounds %"Optional<int>", %"Optional<int>"* %b, i32 0, i32 0
  %hasValue.load2 = load i1, i1* %hasValue1
  %1 = icmp ne i1 %hasValue.load, %hasValue.load2
  br i1 %1, label %if.then, label %if.else

if.then:                                          ; preds = %0
  ret i1 false

if.else:                                          ; preds = %0
  br label %if.end

if.end:                                           ; preds = %if.else
  %hasValue3 = getelementptr inbounds %"Optional<int>", %"Optional<int>"* %b, i32 0, i32 0
  %hasValue.load4 = load i1, i1* %hasValue3
  %2 = xor i1 %hasValue.load4, true
  br i1 %2, label %if.then5, label %if.else6

if.then5:                                         ; preds = %if.end
  ret i1 true

if.else6:                                         ; preds = %if.end
  br label %if.end7

if.end7:                                          ; preds = %if.else6
  %value = getelementptr inbounds %"Optional<int>", %"Optional<int>"* %a, i32 0, i32 1
  %value.load = load i32, i32* %value
  %value8 = getelementptr inbounds %"Optional<int>", %"Optional<int>"* %b, i32 0, i32 1
  %value.load9 = load i32, i32* %value8
  %3 = icmp eq i32 %value.load, %value.load9
  ret i1 %3
}

define i1 @_EN3stdneI3intEEPO3intPO3int(%"Optional<int>"* %a, %"Optional<int>"* %b) {
  %1 = call i1 @_EN3stdeqI3intEEPO3intPO3int(%"Optional<int>"* %a, %"Optional<int>"* %b)
  %2 = xor i1 %1, true
  ret i1 %2
}

define void @_EN4main1fE() {
  %a = alloca %"Optional<int>"
  %1 = alloca %"Optional<int>"
  %b = alloca %"Optional<int>"
  %c = alloca %"Optional<int>"
  %2 = alloca %"Optional<int>"
  %d = alloca %"Optional<int>"
  %3 = alloca %"Optional<int>"
  %x = alloca i32
  %4 = alloca %"Optional<int>"
  %5 = alloca %"Optional<int>"
  call void @_EN3std8OptionalI3intE4initE(%"Optional<int>"* %1)
  %.load = load %"Optional<int>", %"Optional<int>"* %1
  store %"Optional<int>" %.load, %"Optional<int>"* %a
  call void @_EN3std8OptionalI3intE4initE3int(%"Optional<int>"* %b, i32 2)
  call void @_EN3std8OptionalI3intE4initE3int(%"Optional<int>"* %2, i32 3)
  %.load1 = load %"Optional<int>", %"Optional<int>"* %2
  store %"Optional<int>" %.load1, %"Optional<int>"* %c
  call void @_EN3std8OptionalI3intE4initE(%"Optional<int>"* %d)
  call void @_EN3std8OptionalI3intE4initE(%"Optional<int>"* %3)
  %.load2 = load %"Optional<int>", %"Optional<int>"* %3
  store %"Optional<int>" %.load2, %"Optional<int>"* %c
  %c.load = load %"Optional<int>", %"Optional<int>"* %c
  %6 = extractvalue %"Optional<int>" %c.load, 0
  br i1 %6, label %if.then, label %if.else

if.then:                                          ; preds = %0
  %7 = getelementptr inbounds %"Optional<int>", %"Optional<int>"* %c, i32 0, i32 1
  %.load3 = load i32, i32* %7
  %8 = add i32 %.load3, 1
  store i32 %8, i32* %x
  %9 = getelementptr inbounds %"Optional<int>", %"Optional<int>"* %c, i32 0, i32 1
  %.load4 = load i32, i32* %9
  %10 = add i32 1, %.load4
  call void @_EN3std8OptionalI3intE4initE3int(%"Optional<int>"* %4, i32 %10)
  %.load5 = load %"Optional<int>", %"Optional<int>"* %4
  store %"Optional<int>" %.load5, %"Optional<int>"* %c
  %c.load6 = load %"Optional<int>", %"Optional<int>"* %c
  %11 = extractvalue %"Optional<int>" %c.load6, 0
  %assert.condition = icmp eq i1 %11, false
  br i1 %assert.condition, label %assert.fail, label %assert.success

if.else:                                          ; preds = %0
  br label %if.end

if.end:                                           ; preds = %assert.success, %if.else
  ret void

assert.fail:                                      ; preds = %if.then
  call void @_EN3std10assertFailEP4char(i8* getelementptr inbounds ([56 x i8], [56 x i8]* @0, i32 0, i32 0))
  unreachable

assert.success:                                   ; preds = %if.then
  %12 = extractvalue %"Optional<int>" %c.load6, 1
  %13 = sub i32 0, %12
  call void @_EN3std8OptionalI3intE4initE3int(%"Optional<int>"* %5, i32 %13)
  %.load7 = load %"Optional<int>", %"Optional<int>"* %5
  store %"Optional<int>" %.load7, %"Optional<int>"* %c
  br label %if.end
}

declare void @_EN3std10assertFailEP4char(i8*)

define void @_EN4main1gE() {
  %p = alloca i8**
  store i8** null, i8*** %p
  %p.load = load i8**, i8*** %p
  %1 = icmp eq i8** %p.load, null
  br i1 %1, label %if.then, label %if.else

if.then:                                          ; preds = %0
  br label %if.end

if.else:                                          ; preds = %0
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  ret void
}
