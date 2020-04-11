
%E = type { i32, { [48 x i8] } }
%"S<E>" = type { %E* }

define i32 @main() {
  %e = alloca %E
  %enum = alloca %E
  %1 = alloca %"S<E>"
  %enum1 = alloca %E
  %2 = alloca %"S<E>"
  %tag = getelementptr inbounds %E, %E* %enum, i32 0, i32 0
  store i32 0, i32* %tag
  %tag2 = getelementptr inbounds %E, %E* %enum1, i32 0, i32 0
  store i32 0, i32* %tag2
  call void @_EN4main1SI1EE4initEOP1E(%"S<E>"* %2, %E* null)
  %.load = load %"S<E>", %"S<E>"* %2
  %3 = insertvalue { %"S<E>" } undef, %"S<E>" %.load, 0
  %associatedValue = getelementptr inbounds %E, %E* %enum1, i32 0, i32 1
  %4 = bitcast { [48 x i8] }* %associatedValue to { %"S<E>" }*
  store { %"S<E>" } %3, { %"S<E>" }* %4
  call void @_EN4main1SI1EE4initEOP1E(%"S<E>"* %1, %E* %enum1)
  %.load3 = load %"S<E>", %"S<E>"* %1
  %5 = insertvalue { %"S<E>" } undef, %"S<E>" %.load3, 0
  %associatedValue4 = getelementptr inbounds %E, %E* %enum, i32 0, i32 1
  %6 = bitcast { [48 x i8] }* %associatedValue4 to { %"S<E>" }*
  store { %"S<E>" } %5, { %"S<E>" }* %6
  %enum.load = load %E, %E* %enum
  store %E %enum.load, %E* %e
  ret i32 0
}

define void @_EN4main1SI1EE4initEOP1E(%"S<E>"* %this, %E* %e) {
  %e1 = getelementptr inbounds %"S<E>", %"S<E>"* %this, i32 0, i32 0
  store %E* %e, %E** %e1
  ret void
}
