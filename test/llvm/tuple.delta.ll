
define { i1, { float } } @_EN4main1fE() {
  %d = alloca { float }
  %a = alloca { i32, { i1, { float } } }
  %b = alloca { i1, { float } }
  %e = alloca float
  %p = alloca { i1, { float } }*
  store { float } { float -1.000000e+00 }, { float }* %d
  %d.load = load { float }, { float }* %d
  %1 = insertvalue { i1, { float } } { i1 false, { float } undef }, { float } %d.load, 1
  %2 = insertvalue { i32, { i1, { float } } } { i32 42, { i1, { float } } undef }, { i1, { float } } %1, 1
  store { i32, { i1, { float } } } %2, { i32, { i1, { float } } }* %a
  %b1 = getelementptr inbounds { i32, { i1, { float } } }, { i32, { i1, { float } } }* %a, i32 0, i32 1
  %b.load = load { i1, { float } }, { i1, { float } }* %b1
  store { i1, { float } } %b.load, { i1, { float } }* %b
  %d2 = getelementptr inbounds { i1, { float } }, { i1, { float } }* %b, i32 0, i32 1
  %e3 = getelementptr inbounds { float }, { float }* %d2, i32 0, i32 0
  %e.load = load float, float* %e3
  store float %e.load, float* %e
  %b4 = getelementptr inbounds { i32, { i1, { float } } }, { i32, { i1, { float } } }* %a, i32 0, i32 1
  store { i1, { float } }* %b4, { i1, { float } }** %p
  %p.load = load { i1, { float } }*, { i1, { float } }** %p
  %p.load.load = load { i1, { float } }, { i1, { float } }* %p.load
  store { i1, { float } } %p.load.load, { i1, { float } }* %b
  %p.load5 = load { i1, { float } }*, { i1, { float } }** %p
  %d6 = getelementptr inbounds { i1, { float } }, { i1, { float } }* %p.load5, i32 0, i32 1
  %e7 = getelementptr inbounds { float }, { float }* %d6, i32 0, i32 0
  %e.load8 = load float, float* %e7
  store float %e.load8, float* %e
  store float 1.000000e+00, float* %e
  %b.load9 = load { i1, { float } }, { i1, { float } }* %b
  ret { i1, { float } } %b.load9
}

define { i1, float } @_EN4main1gE() {
  ret { i1, float } { i1 true, i32 1 }
}
