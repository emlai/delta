
%"X<int>" = type { %"Y<int>"* }
%"Y<int>" = type { i32 }

@0 = private unnamed_addr constant [67 x i8] c"Unwrap failed at optional-type-unknown-identifier-bug.delta:11:18\0A\00", align 1

define i32 @main() {
  %x = alloca %"X<int>"
  call void @_EN4main1XI3intE4initE(%"X<int>"* %x)
  call void @_EN4main1XI3intE6deinitE(%"X<int>"* %x)
  ret i32 0
}

define void @_EN4main1XI3intE4initE(%"X<int>"* %this) {
  %y = getelementptr inbounds %"X<int>", %"X<int>"* %this, i32 0, i32 0
  store %"Y<int>"* null, %"Y<int>"** %y
  ret void
}

define void @_EN4main1XI3intE6deinitE(%"X<int>"* %this) {
  %a = alloca i32
  %y = getelementptr inbounds %"X<int>", %"X<int>"* %this, i32 0, i32 0
  %y.load = load %"Y<int>"*, %"Y<int>"** %y
  %assert.condition = icmp eq %"Y<int>"* %y.load, null
  br i1 %assert.condition, label %assert.fail, label %assert.success

assert.fail:                                      ; preds = %0
  call void @_EN3std10assertFailEP4char(i8* getelementptr inbounds ([67 x i8], [67 x i8]* @0, i32 0, i32 0))
  unreachable

assert.success:                                   ; preds = %0
  %a1 = getelementptr inbounds %"Y<int>", %"Y<int>"* %y.load, i32 0, i32 0
  %a.load = load i32, i32* %a1
  store i32 %a.load, i32* %a
  ret void
}

declare void @_EN3std10assertFailEP4char(i8*)
