library(nlme)
library(ggplot2)
library(patchwork)
library(xts)
# Exercice 5.1 
 
data(renergie, package = "hecmodstat")
cols <- c(gray.colors(n = 16), 2)[c(1:10,17,11:16)]

# Tracer un graphique de la série chronologique 
g1 <- ggplot(data = renergie, 
       aes(y=pmoy, x = date, group = region, col = region)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  ylab("prix moyen au détail (dollars CAD)")
g2 <- ggplot(data = renergie, 
       aes(y=pmoy-pmin, x = date, group = region, col = region)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  ylab("marge de profit moyenne\n des détaillants (dollars CAD)")

g1 + g2
# Alternative à l'aide de la bibliothèque "xts" pour les séries chronologiques
# Transformer de format long vers format court
ren <- renergie
ren$marg <- renergie$pmoy - ren$pmin
ren$pmin <- NULL
ren <- tidyr::pivot_wider(data = ren, values_from = c("pmoy","marg"), names_from = region)
par(mar = c(3,3,1,1), mfrow = c(1,2), bty = "l", pch = 20)
plot(xts(x = ren[,19:35], order.by = ren$date), main =  "marge de profit moyenne")
plot(xts(x = ren[,2:18], order.by = ren$date), main =  "prix moyen de vente")



rener_mod1 <- nlme::gls(I(pmoy-pmin) ~ factor(region), data = renergie, 
                        correlation = corAR1(form=~1 | date))
plot(ACF(rener_mod1, resType = "normalized"), xlab = "décalage", ylab = "autocorrélation")
likH0 <- logLik(rener_mod1)

# Pour faire ce test d'hypothèse, il suffit d'ajuster
# les 17 modèles indépendaments et additionner leurs valeurs de REML
likH1 <- sum(sapply(1:17, function(i){
  logLik(nlme::gls(I(pmoy-pmin) ~ 1, data = renergie[renergie$region==i,],
                   correlation = corAR1(form=~1 | date)))}))
pchisq(2*as.numeric(likH0-likH1), df = 32, lower.tail = FALSE)


# Exercice 5.2
data(baumann, package = "hecmodstat")
# Créer une colonne d'identifiants
baumann$id <- factor(1:nrow(baumann))
# Transformer les données au format long
baumann_long <- tidyr::pivot_longer(data = baumann, 
                                    cols = c("mpre","mpost"),
                                    names_to = "test",
                                    names_prefix = "m",
                                    values_to = "score"
                                    )

# Transformer test en facteur
baumann_long$test <- factor(baumann_long$test)
baumann$dpp <- baumann$mpost - baumann$mpre
# ANOVA préliminaire - vérifier l'égalité des moyennes pré-interventions
anova(lm(mpre ~ groupe, data = baumann))

# Deux modèles concurrents (ANOVA et régression linéaire Two competing modeles (one way ANOVA and linear modele)
modele1 <- lm(dpp ~ groupe, data = baumann)
car::Anova(modele1, type = 3)
modele2 <- lm(mpost ~ groupe + mpre, data = baumann)
# Tests pour la significativité à l'aide des intervalles de confiance
confint(modele2)["mpre",]
# On peut aussi obtenir la valeur-p du test beta_mpre=1
# à l'aide de la statistique F (effet de type 3)
# / Pour avoir beta_mpre=0, garder le décalage et ajouter mpre comme covariable
car::Anova(lm(mpost ~ groupe + mpre, 
              offset=mpre, data = baumann), type=3)[3,]
TRV <- as.numeric(2*(logLik(modele2) - logLik(modele1)))
pchisq(TRV, df = 1, lower.tail = FALSE)

modele3 <- gls(score ~ groupe*test, 
              data = baumann_long,
              correlation = nlme::corSymm(form = ~ 1 | id))
modele4 <- gls(score ~ groupe*test, 
              data = baumann_long,
              correlation = nlme::corSymm(form = ~ 1 | id),
              weights = varIdent(form=~1|test))
#Test du rapport de vraisemblance (hétéroscedasticité)
anova(modele3, modele4)
#Matrice de covariance intra-individu
getVarCov(modele4)

# On peut ajuster des paramètres différents dans chaque groupe
# en partitionnant l'échantillon en trois
logvrais <- rep(0,3)
for(i in 1:3){
  logvrais[i] <- logLik(gls(score ~ test,
                      data = baumann_long,
                      subset = which(baumann_long$groupe == levels(baumann_long$groupe)[i]),
                      correlation = corSymm(form = ~ 1 | id)))
}
pchisq(sum(logvrais) - as.vector(logLik(modele3)), df = 4, lower.tail = FALSE)

#Finalement, vérifier l'effet de la méthode d'enseignement
summary(modele4)

# Exercice 5.3

data(tolerance, package = "hecmodstat")
tolerance$sexe <- factor(tolerance$sexe, labels = c("femme","homme"))
# Analyse graphique exploratoire
g1 <- ggplot(data = tolerance) + 
  geom_boxplot(aes(y = tolerance, x = sexe)) + 
  ylab("tolérance")
g2 <- ggplot(data = tolerance) + 
  geom_point(aes(y = tolerance, x = exposition)) + 
  ylab("tolérance")
g3 <- ggplot(data = tolerance) + 
  geom_boxplot(aes(y = tolerance, x = factor(age))) + 
  xlab("âge") + ylab("tolérance")
#pdf("E5p3-tolerance_eda-fr.pdf", width = 8, height = 3)
g1 + g2 + g3
dev.off()

# pdf("E5p3-tolerance_spagh-fr.pdf", width = 8, height = 5)
ggplot(data = tolerance, aes(x = age, y = tolerance, group = id, color=id)) +
  geom_line() + 
  theme(legend.position="none") + 
  labs(y="tolérance", x="âge") 

dev.off()

mod1 <- gls(tolerance ~ sexe + exposition + age, data = tolerance, 
    correlation = corCompSymm(form= ~ 1 | id))
mod2 <- gls(tolerance ~ sexe + exposition + age, data = tolerance, 
    correlation = corAR1(form= ~ 1 | id))
mod3 <- gls(tolerance ~ sexe + exposition + age, data = tolerance)
anova(mod1, mod3)
anova(mod2, mod3)
cbind("AIC" = c(AIC(mod1), AIC(mod2), AIC(mod3)), 
      "BIC" = c(BIC(mod1), BIC(mod2), BIC(mod3)))


