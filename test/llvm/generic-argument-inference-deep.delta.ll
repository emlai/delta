
%"S<int>" = type { i32 }
%"S<bool>" = type { i1 }

define void @_EN4main1fE1SI3intE1SI3intE(%"S<int>" %c, %"S<int>" %d) {
  %1 = call i1 @_EN4maineqI3intEE1SI3intE1SI3intE(%"S<int>" %c, %"S<int>" %d)
  ret void
}

define i1 @_EN4maineqI3intEE1SI3intE1SI3intE(%"S<int>" %a, %"S<int>" %b) {
  ret i1 true
}

define void @_EN4main1fEP1SI4boolEP1SI4boolE(%"S<bool>"* %c, %"S<bool>"* %d) {
  %c.load = load %"S<bool>", %"S<bool>"* %c
  %d.load = load %"S<bool>", %"S<bool>"* %d
  %1 = call i1 @_EN4maineqI4boolEE1SI4boolE1SI4boolE(%"S<bool>" %c.load, %"S<bool>" %d.load)
  ret void
}

define i1 @_EN4maineqI4boolEE1SI4boolE1SI4boolE(%"S<bool>" %a, %"S<bool>" %b) {
  ret i1 true
}
