
%"S<int>" = type { i32* }
%"S<bool>" = type { i1* }

define i32 @main() {
  %1 = alloca %"S<int>"
  %2 = alloca %"S<bool>"
  call void @_EN4main1SI3intE4initE(%"S<int>"* %1)
  call void @_EN4main1SI4boolE4initE(%"S<bool>"* %2)
  ret i32 0
}

define void @_EN4main1SI3intE4initE(%"S<int>"* %this) {
  %p = getelementptr inbounds %"S<int>", %"S<int>"* %this, i32 0, i32 0
  store i32* null, i32** %p
  ret void
}

define void @_EN4main1SI4boolE4initE(%"S<bool>"* %this) {
  %p = getelementptr inbounds %"S<bool>", %"S<bool>"* %this, i32 0, i32 0
  store i1* null, i1** %p
  ret void
}
