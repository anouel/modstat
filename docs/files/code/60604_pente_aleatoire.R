library(lme4)
library(hecmodstat)

## Exemple 1
data(poussin, package = "hecmodstat")
# Ajuster un modèle avec regime comme effet fixe
# Effets aléatoires corrélés
mod1 <- lme4::lmer(masse ~ temps + regime + (1 + temps | poussin),
           data = poussin)
# Effets aléatoires indépendants (matrice de covariance diagonale)
mod2 <- lme4::lmer(masse ~  temps + regime + 
                     (1 | poussin) +
                     (temps - 1 | poussin),
           data = poussin)
# Tester si la corrélation entre ordonnée à l'origine et pente
# aléatoires est significative
# H0: omega_{12}=0 versus H1: omega_{12} NEQ 0
anova(mod1, mod2)

# Ajuster un modèle alternatif avec un effet aléatoire pour régime
# les poussins est emboîté dans régime
# 
# Effet fixe ou aléatoire?
# Avec des variables catégorielles, on doit enlever l'effet
# fixe pour pouvoir estimer l'effet aléatoire
mod3 <- lme4::lmer(masse ~ temps +
                     (1 + temps | regime:poussin) + 
                     (1 | regime),
                   data = poussin)
# le modèle n'est pas ajustable - en cause, la corrélation négative
# quasi-parfaite entre pente et ordonnée à l'origine
rc3 <- rescov(mod3)
# Imprimer la matrice de covariance du modèle multi-niveau (emboîtés)
plot(rc3, corr = FALSE)
# Graphique de la matrice de corrélation
plot(rc3, corr = TRUE)

# Graphique de la matrice de corrélation intra-poussin
p38 <- poussin$poussin == 38
image(cov2cor(rc3$var_y[p38,p38]),
      sub = "", xlab = "", ylab = "")

library(nlme)
mod0 <- nlme::lme(salaire ~ domaine + annees + sexe,
                  random =~1 |echelon, 
                  data = college, 
                  correlation = varIdent(form=~1 | echelon))

### Deuxième exemple: données de mobilisation
# Pourquoi ne faut-il pas mettre d'effet aléatoire sur 
# des variables explicatives catégorielles comme "sexe"?
data(mobilisation, package = "hecmodstat")
mod5 <- lme4::lmer(mobilisation ~ anciennete + agegest + nunite + 
                     (1 | sexe) + (1 | idunite),
                   data = mobilisation)

# Graphique de la matrice de corrélation
plot(rescov(mod5), corr = TRUE)
# On voit que la matrice de corrélation résultante est dense...
# ces modèles ne sont pas ajustables en haute dimension
# on crée de la corrélation inter-groupe
