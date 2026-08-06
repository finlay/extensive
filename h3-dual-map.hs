-- There are 24 elements to describe

ije, iji, ijj, ijk
jke, jki, jkj, jkk,
kie, kii, kij, kik,

eie, eii, eij, eik,
eje, eji, ejj, ejk,
eke, eki, ekj, ekk

  :: T (Tensor (Tensor H H) H)

ije = (id - (sigma3 . conjugate3)) $               (e ⊗ k ⊗ e - i ⊗ k ⊗ i - j ⊗ k ⊗ j - k ⊗ k ⊗ k) - scale (2.0) (e ⊗ e ⊗ k - i ⊗ e ⊗ j)
iji = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (j ⊗ k ⊗ k + e ⊗ k ⊗ i - i ⊗ e ⊗ k - e ⊗ e ⊗ j)
ijj = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (e ⊗ e ⊗ i - j ⊗ e ⊗ k + e ⊗ k ⊗ j - i ⊗ k ⊗ k)
ijk = (id + (sigma3 . conjugate3)) $               (e ⊗ e ⊗ e + i ⊗ e ⊗ i + j ⊗ e ⊗ j - k ⊗ e ⊗ k) + scale (2.0) (e ⊗ k ⊗ k + i ⊗ k ⊗ j)

jke = (id - (sigma3 . conjugate3)) $               (e ⊗ i ⊗ e - i ⊗ i ⊗ i - j ⊗ i ⊗ j - k ⊗ i ⊗ k) + scale (2.0) (j ⊗ e ⊗ k - e ⊗ e ⊗ i)
jki = (id + (sigma3 . conjugate3)) $               (e ⊗ e ⊗ e - i ⊗ e ⊗ i + j ⊗ e ⊗ j + k ⊗ e ⊗ k) + scale (2.0) (j ⊗ i ⊗ k + e ⊗ i ⊗ i)
jkj = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (e ⊗ i ⊗ j - i ⊗ e ⊗ j - e ⊗ e ⊗ k - i ⊗ i ⊗ k)
jkk = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (e ⊗ e ⊗ j + i ⊗ i ⊗ j + e ⊗ i ⊗ k - i ⊗ e ⊗ k)

kie = (id - (sigma3 . conjugate3)) $               (e ⊗ j ⊗ e - i ⊗ j ⊗ i - j ⊗ j ⊗ j - k ⊗ j ⊗ k) - scale (2.0) (e ⊗ e ⊗ j + i ⊗ e ⊗ k)
kii = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (e ⊗ e ⊗ k + j ⊗ j ⊗ k + e ⊗ j ⊗ i - i ⊗ e ⊗ j)
kij = (id + (sigma3 . conjugate3)) $               (e ⊗ e ⊗ e + i ⊗ e ⊗ i - j ⊗ e ⊗ j + k ⊗ e ⊗ k) + scale (2.0) (e ⊗ j ⊗ j - i ⊗ j ⊗ k)
kik = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (i ⊗ j ⊗ j + e ⊗ j ⊗ k - j ⊗ e ⊗ k - e ⊗ e ⊗ i)

eie = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (i ⊗ j ⊗ j - e ⊗ j ⊗ k + i ⊗ k ⊗ k + e ⊗ k ⊗ j)
eii = (id - (sigma3 . conjugate3)) $ scale (2.0) $ (i ⊗ k ⊗ j - e ⊗ k ⊗ k - e ⊗ j ⊗ j - i ⊗ j ⊗ k)
eij = (id - (sigma3 . conjugate3)) $               (j ⊗ k ⊗ j - k ⊗ k ⊗ k - e ⊗ k ⊗ e - i ⊗ k ⊗ i) - scale (2.0) (j ⊗ j ⊗ k - e ⊗ j ⊗ i)
eik = (id - (sigma3 . conjugate3)) $               (e ⊗ j ⊗ e + i ⊗ j ⊗ i + j ⊗ j ⊗ j - k ⊗ j ⊗ k) + scale (2.0) (j ⊗ k ⊗ k + e ⊗ k ⊗ i)

eje = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (j ⊗ k ⊗ k - e ⊗ k ⊗ i - i ⊗ i ⊗ j + e ⊗ i ⊗ k)
eji = (id - (sigma3 . conjugate3)) $               (e ⊗ k ⊗ e - i ⊗ k ⊗ i + j ⊗ k ⊗ j + k ⊗ k ⊗ k) + scale (2.0) (i ⊗ i ⊗ k + e ⊗ i ⊗ j)
ejj = (id - (sigma3 . conjugate3)) $ scale (2.0) $ (j ⊗ i ⊗ k - e ⊗ i ⊗ i - i ⊗ k ⊗ j - e ⊗ k ⊗ k)
ejk = (id - (sigma3 . conjugate3)) $               (k ⊗ i ⊗ k - e ⊗ i ⊗ e - i ⊗ i ⊗ i - j ⊗ i ⊗ j) - scale (2.0) (i ⊗ k ⊗ k - e ⊗ k ⊗ j)

eke = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (e ⊗ j ⊗ i - e ⊗ i ⊗ j - i ⊗ i ⊗ k - j ⊗ j ⊗ k)
eki = (id - (sigma3 . conjugate3)) $               (i ⊗ j ⊗ i - e ⊗ j ⊗ e - j ⊗ j ⊗ j - k ⊗ j ⊗ k) - scale (2.0) (i ⊗ i ⊗ j - e ⊗ i ⊗ k)
ekj = (id - (sigma3 . conjugate3)) $               (e ⊗ i ⊗ e + i ⊗ i ⊗ i - j ⊗ i ⊗ j + k ⊗ i ⊗ k) + scale (2.0) (i ⊗ j ⊗ j + e ⊗ j ⊗ k)
ekk = (id - (sigma3 . conjugate3)) $ scale (2.0) $ (i ⊗ j ⊗ k - e ⊗ j ⊗ j - e ⊗ i ⊗ i - j ⊗ i ⊗ k)






