# Les deux paquets suivants sont les plus fréquemment employés pour ajuster des modèles mixtes
library(lme4) #modèles linéaires généralisés mixtes - plus moderne, mieux maintenu et permet d'ajuster des données de grandes tailles
library(nlme) #modèles linéaires mixtes - avec matrices de covariance différentes de l'identité pour les aléas
data(vengeance, package = "hecmodstat")
vengeance$t <- as.integer(vengeance$temps)

# Équicorrélation pour erreurs
modele1 <- nlme::gls(vengeance ~ sexe + age + vc + wom + t,
              correlation= corCompSymm(form=~1|id),
              data = vengeance, method = "REML")
#ordonnée à l'origine aléatoire, erreurs indépendantes
modele2 <- nlme::lme(vengeance ~ sexe + age + vc + wom + t, 
                    random = ~ 1  | id, 
                    data = vengeance, method = "REML")
# Même estimés des coefficients?
isTRUE(all.equal(coef(modele1), fixef(modele2)))
# Imprimer les matrices de covariance
getVarCov(modele1, individual = 1,
          type = "marginal")
getVarCov(modele2, individual = 1,
          type = "marginal")

# La covariance marginale est la somme de la variance induite par les effets aléatoires
# et la covariance des aléas
getVarCov(modele2, type = "random.effects")
getVarCov(modele2, individual = 1, type = "conditional")

