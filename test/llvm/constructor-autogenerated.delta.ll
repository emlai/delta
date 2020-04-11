
%X = type { i32, i32* }
%"Generic<float>" = type { float }
%"Generic<int>" = type { i32 }

define i32 @main() {
  %b = alloca i32
  %x = alloca %X
  %y = alloca %X
  %e = alloca {}
  %g = alloca %"Generic<float>"
  %h = alloca %"Generic<int>"
  store i32 2, i32* %b
  call void @_EN4main1X4initE3intP3int(%X* %x, i32 4, i32* %b)
  call void @_EN4main1X4initE3intP3int(%X* %y, i32 4, i32* %b)
  call void @_EN4main5Empty4initE({}* %e)
  call void @_EN4main7GenericI5floatE4initE5float(%"Generic<float>"* %g, float 4.500000e+00)
  call void @_EN4main7GenericI3intE4initE3int(%"Generic<int>"* %h, i32 4)
  ret i32 0
}

define void @_EN4main1X4initE3intP3int(%X* %this, i32 %a, i32* %b) {
  %a1 = getelementptr inbounds %X, %X* %this, i32 0, i32 0
  store i32 %a, i32* %a1
  %b2 = getelementptr inbounds %X, %X* %this, i32 0, i32 1
  store i32* %b, i32** %b2
  ret void
}

define void @_EN4main5Empty4initE({}* %this) {
  ret void
}

define void @_EN4main7GenericI5floatE4initE5float(%"Generic<float>"* %this, float %i) {
  %i1 = getelementptr inbounds %"Generic<float>", %"Generic<float>"* %this, i32 0, i32 0
  store float %i, float* %i1
  ret void
}

define void @_EN4main7GenericI3intE4initE3int(%"Generic<int>"* %this, i32 %i) {
  %i1 = getelementptr inbounds %"Generic<int>", %"Generic<int>"* %this, i32 0, i32 0
  store i32 %i, i32* %i1
  ret void
}
