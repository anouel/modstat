library(hecmodstat)
summary(lm(lognutilisateur ~ celcius, data = bixicol))
summary(lm(lognutilisateur ~ farenheit, data = bixicol))
summary(lm(lognutilisateur ~ celcius + farenheit, data = bixicol))
