
%"S<R>" = type { %"A<A<R>>" }
%"A<A<R>>" = type { %"A<R>" }
%"A<R>" = type { %R }
%R = type { i32 }

define i32 @main() {
  %s = alloca %"S<R>"
  call void @_EN4main1SI1RE4initE(%"S<R>"* %s)
  call void @_EN4main1SI1RE1sE(%"S<R>"* %s)
  ret i32 0
}

define void @_EN4main1SI1RE4initE(%"S<R>"* %this) {
  ret void
}

define void @_EN4main1SI1RE1sE(%"S<R>"* %this) {
  %t = alloca %"A<R>"
  %tt = alloca %R
  %a = getelementptr inbounds %"S<R>", %"S<R>"* %this, i32 0, i32 0
  %1 = call %"A<R>" @_EN4main1AI1AI1REEixE3int(%"A<A<R>>"* %a, i32 0)
  store %"A<R>" %1, %"A<R>"* %t
  %2 = call %R @_EN4main1AI1REixE3int(%"A<R>"* %t, i32 0)
  store %R %2, %R* %tt
  %a1 = getelementptr inbounds %"S<R>", %"S<R>"* %this, i32 0, i32 0
  %3 = call i32 @_EN4main1R1hE(%R* %tt)
  %4 = call %"A<R>" @_EN4main1AI1AI1REEixE3int(%"A<A<R>>"* %a1, i32 %3)
  ret void
}

define %"A<R>" @_EN4main1AI1AI1REEixE3int(%"A<A<R>>"* %this, i32 %i) {
  %t = getelementptr inbounds %"A<A<R>>", %"A<A<R>>"* %this, i32 0, i32 0
  %t.load = load %"A<R>", %"A<R>"* %t
  ret %"A<R>" %t.load
}

define %R @_EN4main1AI1REixE3int(%"A<R>"* %this, i32 %i) {
  %t = getelementptr inbounds %"A<R>", %"A<R>"* %this, i32 0, i32 0
  %t.load = load %R, %R* %t
  ret %R %t.load
}

define i32 @_EN4main1R1hE(%R* %this) {
  %i = getelementptr inbounds %R, %R* %this, i32 0, i32 0
  %i.load = load i32, i32* %i
  ret i32 %i.load
}
