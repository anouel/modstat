library(nlme)
data(vengeance, package = "hecmodstat")
vengeance$t <- as.integer(vengeance$temps)

model1 <- gls(vengeance ~ sexe + age + vc + wom + t,
              correlation= corAR1(form=~1|id),
              data = vengeance, method = "ML")
model2 <- gls(vengeance ~ id + t,
              data = vengeance, 
              method = "ML")
model3 <- gls(vengeance ~ id + t,
              data = vengeance, 
              correlation= corAR1(form=~1|id),
              method = "ML")

anova(model2, model3) # coefficient AR(1) pas significatif si on estime par maximum de vraisemblance
# si on ajoute un effet groupe pour la moyenne -> moins de structure résiduelle 

model2b <- gls(vengeance ~ id + t,
              data = vengeance, 
              method = "REML")
model3b <- gls(vengeance ~ id + t,
              data = vengeance, 
              correlation= corAR1(form=~1|id),
              method = "REML")
anova(model2b, model3b)
# Mais significatif avec REML! 
# ce résultat paradoxal est dû au surajustement.
 
# Comparaison avec critères d'information (modèles non emboîtés) 
AIC(model1, model2)
BIC(model1, model2)
# Tous les modèles ont été ajustés avec le maximum de vraisemblance