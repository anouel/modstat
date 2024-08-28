
# Exercice 6.1
data(goldstein, package = "hecmodstat")
goldstein$rv <- relevel(factor(goldstein$rv), ref = 3)
goldstein$denom <- relevel(factor(goldstein$denom), ref = "autre")
goldstein$type <- relevel(factor(goldstein$type), ref = "mixte")
# Nombre d'élèves par classe
neleves <- summary(factor(goldstein$ecole))
summary(neleves)

library(lme4)
mod1 <- lmer(score ~ (1|ecole) + TLL + sexe + type + rv + denom,
             data = goldstein)
summary(mod1)
#Effets de type 3 avec tests de Wald (même sortie que SAS)
car::Anova(mod1, type = 3)

randeff <- ranef(mod1, condVar = TRUE)
randeff_m <- randeff$ecole[,1]
# Nuage de points des effets aléatoires
# cette commande illustre comment obtenir les erreurs-type 
randeff_et <- attr(randeff$ecole,"postVar")[1,1,]

par(mfrow = c(1,2), mar = c(4,4,1,1), bty = "l")
plot(resid(mod1), fitted(mod1), col = 1, 
     ylab = "résidus ordinaires", 
     xlab = "valeurs ajustées", pch = 20)
car::qqPlot(randeff_m, id=FALSE,
            xlab = "quantiles théoriques",
            ylab = "prédictions des effets aléatoires")


library(lattice)
# Diagramme des effets aléatoires avec +/- 1.96 fois erreur-type
# En anglais, "caterpillar plot"
trellis.par.set(canonical.theme(color = FALSE))
dotplot(ranef(mod1), main = FALSE)


# Paramètres de covariance
vars <- VarCorr(mod1)
vars <- c(vars[[1]], attr(vars, "sc")^2)
cov37 <- matrix(vars[1], nrow = 4, ncol = 4) + diag(vars[2], 4)
cov2cor(cov37)

# Classement avec covariables fixes au sein d'école 
# à l'effet duquel on ajoute la prédiction des effets aléatoires
goldstein_sous <- goldstein[!duplicated(goldstein$ecole),]
goldstein_sous$TLL <- 0
goldstein_sous$sexe <- 0
goldstein_sous$rv <- "1"
pred_rang <- predict(mod1, newdata = goldstein_sous, re.form = NULL)
goldstein_sous$ecole[order(pred_rang, decreasing = TRUE)][1:5]  



# Exercice 6.2
data(gsce, package = "hecmodstat")
library(nlme)
# Modèle d'équicorrélation intra-centre
mod1 <- gls(resultat ~ score + sexe, 
    data = gsce, 
    correlation = corCompSymm(form = ~1 | centre))
# Modèle avec effet principal pour centre
mod2 <- gls(resultat ~ score + sexe + centre, 
            data = gsce)
# Modèle linéaire mixte avec ordonnée à l'origine aléatoire par centre
mod3 <- lme(fixed = resultat ~ score + sexe, 
            data = gsce,
            random = ~1 | centre)
# Covariance des erreurs
getVarCov(mod3, levels = rep(1,3), type = "conditional")
# Covariance de Y
getVarCov(mod3, levels = rep(1,3), type = "marginal")
# Prédictions - level=0 pour marginale, level=1 pour conditionnelle (incluant effets aléatoires)
predict(mod3, newdata = data.frame(score = 91, centre = 2, sexe=1), 
        level = 0:1)
predict(mod3, newdata = data.frame(score = 100, sexe=1),
        level = 0)

