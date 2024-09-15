library(patchwork) # collages de graphiques
library(ggplot2) # grammaires des graphiques
library(dplyr) # manipulation des données
library(hecmodstat) # bases de données 
library(hecedsm)
# changer le thème ggplot2 pour les graphes
theme_set(theme_classic())

#######################################
####### Analyse exploratoire  #########
#######################################

# Données de l'exemple 1
data(LC19_S1, package = "hecedsm") # charger les données
?hecedsm::LC19_S1 # description de la base de donnée (menu d'aide)
str(LC19_S1) # résumé sommaire
summary(LC19_S1) # statistiques descriptives
with(LC19_S1, cor(familiarity, prodeval)) # corrélation linéaire
length(unique(LC19_S1$prodeval)) # nombre de valeurs uniques

# Nuage de point de familiarité vs évaluation de produits
ggplot(data = LC19_S1,
       mapping = aes(x = familiarity,
                     y = prodeval)) +
  geom_point()

# Données de l'exemple 2
data("BSJ92", package = "hecedsm")
?hecedsm::BSJ92
# pré-test vs post-test 1
ggplot(data = BSJ92,
       mapping = aes(x = pretest1,
                     y = posttest1,
                     col = group)) +
  # décaler les points pour voir les doublons
  geom_point(position = position_jitter(),
             size = 2) +
  # ajouter la ligne pour score pre-test = score post-test
  geom_abline(intercept = 0, slope = 1) +
  # changer les limites pour capturer l'ensemble des scores possibles
  scale_y_continuous(limits = c(1,16)) + 
  scale_x_continuous(limits = c(1,16)) 
summary(BSJ92) # Statistiques descriptives

# Données de l'exemple 3
data(college, package = "hecmodstat")
?hecmodstat::college
# Différences de salaire notables, mais pas nécessairement à cause du sexe
t.test(salaire ~ sexe, data = college)
# Statistiques descriptives
summary(college)
# On remarque qu'il y a peu de femmes (données fortement débalancées)


# Nuage de point du salaire en fonction du nombre d'années de service
# pour chaque échelon
ggplot(data = college,
       mapping = aes(color = sexe, 
                     fill = sexe, 
                     y = salaire, 
                     x = service)) +
  geom_point() +
  facet_grid(~echelon, scales = "free_x")
# Le salaire moyen augmente avec les échelons
# et le nombre d'années de service
# Beaucoup plus de variabilité dans les échelons supérieurs
# Maximum de 6 ans (congé de maternité?) pour les adjoints
# Certains profs ne prennent pas leur retraite malgré 60 ans de service...

# Répartition homme/femme au sein des échelons très différente
with(college, table(sexe, echelon))
# Forte corrélation entre le nombre d'années depuis le doctorat 
# et le nombre d'années de service
ggplot(data = college,
       mapping = aes(color = sexe, 
                     fill = sexe, 
                     y = service, 
                     x = annees)) +
  geom_point()
# Calculer la corrélation linéaire (Pearson) entre deux variables
with(college, cor(annees, service))

# Données 4
data(MV23_S1, package = "hecedsm")
?hecedsm::MV23_S1
str(MV23_S1)

summary(MV23_S1)
# 73% des gens ont donné un montant
# Les dons sont entre 0.25 et 25, seuls les dons non-nuls sont affichés
# "donate" est un dérivé de la variable réponse "amount": il faut l'exclure
# Remplacer les valeurs manquantes de "amount" par la valeur logique zéro.
MV23_S1 <- MV23_S1 |> dplyr::mutate(amount = ifelse(is.na(amount), 0, amount))
# Calcul des statistiques descriptives par groupe
MV23_S1 |>
  dplyr::group_by(condition) |> # regrouper par variable catégorielle
  dplyr::summarise(moy = mean(amount)) # calcul de nouvelles variables


# Modèle de régression linéaire
mod <- lm(amount ~ condition, data = MV23_S1)
# Le modèle est réponse ~ variables explicatives, séparées par des signes plus ("+")
coef(mod) # estimations des coefficients betas pour la moyenne
summary(mod) # Tableau récapitulatifs

# Vérifier la matrice du modèle
head(model.matrix(~condition, data = MV23_S1))
# Comparer avec les étiquettes/libellés des facteurs
head(MV23_S1)



## Exemple 3 - analyse de variance

# Par défaut, le contraste est contr.treat
# donc la catégorie de référence est assimilée à l'ordonnée à l'origine
# et les autres coefficients sont les différences de moyennes
# 
# contr.sum impose que la moyenne globale est l'ordonnée à l'origine, 
# et que les coefficients sont la différence de moyenne entre groupe
# et la moyenne globale (le coefficient manquant est -somme(coefs)

# Si la catégorie de référence (première valeur en ordre alphanumérique)
# n'est pas la variable d'intérêt, on peut utiliser "relevel" pour changer
# BSJ92 |> mutate(group = relevel(group, ref = "DRTA"))

# Différentes paramétrisations pour les moyennes de groupe
lm(posttest1 ~ group, data = BSJ92)
# Paramétrisation usuelle pour l'ANOVA
lm(posttest1 ~ group, data = BSJ92, contrasts = list(group = "contr.sum"))
# Enlever l'ordonnée à l'origine (trois moyennes de groupe)
lm(posttest1 ~ -1 + group, data = BSJ92)
# Les erreurs-type des moyennes sont égales dans le cas d'échantillons balancés
# échantillon balancé = même nombre d'observations par groupe expérimental

# Analyse de covariance modèle linéaire pour post en fonction de pré
lm(posttest1 ~ group +  pretest1,
   data = BSJ92)
# Ajuster une régression linéaire
linmod <- lm(
  posttest1 ~ group +  pretest1,
  data = BSJ92 |> 
    dplyr::mutate( # centrer le pré-test
      pretest1 = pretest1 - mean(pretest1)))
# Seul le coefficient de l'ordonnée à l'origine change
coef(linmod) 
# Coefficients de la moyenne
## (Intercept)    pretest1   groupDRTA     groupTA 
##       6.188       0.693       3.627       2.036
summary(linmod)



## Estimation

# En utilisant la formule des moindres carrés
# Définir la variable réponse et la matrice du modèle
y <- BSJ92$posttest1
X <- model.matrix(linmod)
mco <- function(X, y){ as.numeric(solve(t(X) %*% X) %*% t(X) %*% y)}
# Coefficients (MCO) de la moyenne
betas <- as.numeric(coef(linmod))
# En pratique, il est préférable d'utiliser une décomposition en valeurs
# singulières ou QR (par défaut avec "lm") pour inverser la matrice
isTRUE(all.equal(
  betas,
  mco(X = X, y = y)))

# Autres quantités d'intérêt
residus <- resid(linmod)
# Estimation de l'écart-type sigma, avec n-p-1 comme dénominateur
sd_linmod <- sqrt(sum(residus^2)/(length(y) - length(betas)))
# Matrice de covariance des paramètres de la moyenne
vcov_beta <- vcov(linmod)
# Extraire les erreurs-types des betas
se_beta <- sqrt(diag(vcov_beta))
isTRUE(all.equal(
  sd_linmod^2 * solve(t(X) %*% X),
  vcov_beta))

# Valeurs ajustées - prédictions
val_ajust <- fitted(linmod)
# Valeur du R2 (coefficient de détermination)
# carré de la corrélation linéaire entre y et ypred
cor(val_ajust, y)^2
