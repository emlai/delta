
%"S<int>" = type { i32 }

define void @_EN4main1fEP1SI3intE(%"S<int>"* %s) {
  %1 = call i32* @_EN4main1SI3intEixE3int(%"S<int>"* %s, i32 0)
  %.load = load i32, i32* %1
  %2 = add i32 %.load, 1
  store i32 %2, i32* %1
  ret void
}

define i32* @_EN4main1SI3intEixE3int(%"S<int>"* %this, i32 %i) {
  %t = getelementptr inbounds %"S<int>", %"S<int>"* %this, i32 0, i32 0
  ret i32* %t
}
