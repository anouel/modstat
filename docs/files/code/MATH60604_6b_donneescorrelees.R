data(mobilisation, package = "hecmodstat")

gls(mobilisation ~ agegest + nunite + sexe + anciennete, 
    data = mobilisation,
    correlation = corCompSymm(form=~1 | idunite))
