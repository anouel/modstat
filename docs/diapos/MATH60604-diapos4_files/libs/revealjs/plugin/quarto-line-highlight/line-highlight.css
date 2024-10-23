# Charger les données
data(attente, package = "hecmodstat")
n <- length(attente) # taille de l'échantillon

# Fonction de log-vraisemblance exponentielle
logvrais_exp <- function(lambda,  y = attente){
 #somme des contributions (log de la densité)
  sum(dexp(x = y, rate = 1/lambda, log = TRUE))  
}
# Comparer les fonctions de log-vraisemblance et de vraisemblance
lambda <- seq(15, 60, length.out = 101)
lv <- sapply(lambda, function(l){logvrais_exp(l)})
par(mfrow = c(1,2))
# Fonction de vraisemblance
plot(x = lambda,
     y = exp(lv) - max(exp(lv)),
     type = "l",
     ylab = "vraisemblance",
     xlab = expression(lambda))
abline(v = mean(attente), lty = 2)
# Fonction de log-vraisemblance
plot(x = lambda, 
     y = lv - max(lv), 
     type = "l",
     ylab = "log-vraisemblance",
     xlab = expression(lambda))
abline(v = mean(attente), lty = 2)
graphics.off()

# Distribution d'échantillonnage des EMV
# Ici, l'EMV est la moyenne donc sa distribution d'échantillonage
# est approximativement normale
hist(replicate(n = 1e4L, mean(sample(attente, size = 10, replace = TRUE))), 
     main = "",
     breaks = 1e2L,
     freq = FALSE, # échelle de la densité
     ylab = "densité",
     xlab = expression(lambda))


# Score de la loi exponentielle
score_exp <- function(lambda, y = attente){
  length(attente)*(-1/lambda + mean(attente)/(lambda^2))
}
# Toujours vérifier en pratique ses dérivées
isTRUE(all.equal(numDeriv::grad(func = logvrais_exp, x = 0.2), score_exp(0.2)))


# Information observée != observation de Fisher
# Démonstration avec le modèle exponentiel
obs_info_exp <- function(lambda, y = attente){
  -length(y) * (lambda^(-2) -2*lambda^(-3)*mean(y))
}
fisher_info_exp <- function(lambda, n = length(attente)){n*lambda^(-2)}
# On trace les deux courbes
plot(x = lambda, 
     y = obs_info_exp(lambda),
     type = "l",
     ylab = "information",
     xlab = expression(lambda))
# Ajouter information de Fisher (traitillé)
lines(x = lambda, y = fisher_info_exp(lambda), lty = 2)
abline(v = mean(attente))

## Deuxième modèle - loi de Weibull

# log-vraisemblance Weibull, sa dérivée première (gradient) et seconde (hessienne)
logvrais_weib <- function(pars, y = attente){
  sum(dweibull(x = attente, shape = pars[2], scale = pars[1], log = TRUE))
}
# Fonction de score (gradient en fonction des paramètres
score_weib <- function(pars){numDeriv::grad(func = logvrais_weib, x = pars)}
# Matrice d'information observée (négatif de la hessienne de la log-vraisemblance)
info_obs_weib <- function(pars){-numDeriv::hessian(func = logvrais_weib, x = pars)}



# Démonstration de l'algorithme de Newton-Raphson
# Initialiser l'algorithme (valeurs des paramètres initiaux du modèle exponentiel)
pars <- depart <- c(mean(attente), 1)
niter <- 0 # Calculer le nombre d'itération
# Boucle: itérer tant que le score est non-nul
while(!isTRUE(all.equal(score_weib(pars), rep(0, 2), tolerance = 1e-10))){
  niter <- niter + 1
  pars <- pars + solve(info_obs_weib(pars)) %*% score_weib(pars)
}
# Retourner les paramètres (estimations du maximum de vraisemblance
emv_weib <- pars 
score_weib(pars) # équation du score (gradient=0 si c'est un EMV)
# Calcul de l'information
info_weib <- info_obs_weib(pars)
# vérifier que la matrice d'info est définie positive aux EMVs
# en calculant les valeurs propres de la matrice et en vérifiant
# si elles sont strictement positives
isTRUE(all(eigen(info_weib, only.values = TRUE)$values > 0)) 
# Matrice de covariance estimée basée sur l'approximation de la loi d'échantillonnage 
cov_weib <- solve(info_weib)
se_weib <- sqrt(diag(cov_weib))
# Comparaison avec la routine du paquet MASS pour l'optimisation
MASS::fitdistr(x = attente, densfun = "weibull", 
               start = list(scale = mean(attente), shape = 1))

# Diagramme quantile-quantile Weibull
plot(x = qweibull(ppoints(n = 62), shape = pars[2], scale = pars[1]), 
     y = sort(attente),
     panel.first = {abline(a = 0, b = 1)},
     xlab = "quantiles théoriques",
     ylab = "quantiles empiriques")

# Calcul des erreurs-type pour une transformation 
# Méthode delta - fonction avec sortie scalaire des paramètres
# Exemple 1: espérance de loi Weibull
g <- function(pars){ pars[1]*gamma(1+1/pars[2])}
# Calcul du jacobien de la transformation
grad_g <- function(pars){numDeriv::grad(func = g, x = pars)}
nabla <- grad_g(pars)
# Calcul de la covariance
vcov_mu_weib <- t(nabla) %*% cov_weib %*% nabla
# Erreur-type pour l'espérance de la loi Weibull
se_mu <- sqrt(diag(vcov_mu_weib))
# Comparer ce résultat avec l'estimateur usuel
se_moy <- sd(attente)/sqrt(n)


# Exemple 2: probabilité d'excéder 60 secondes (modèle Weibull)
g <- function(pars){ 
  pweibull(60, shape = pars[2], scale = pars[1], lower.tail = FALSE)
}
# Calcul du jacobien de la transformation
grad_g <- function(pars){numDeriv::grad(func = g, x = pars)}
nabla <- grad_g(pars)
# Calcul des erreurs-type
se_p60_weib <- c(sqrt(t(nabla) %*% cov_weib %*% nabla))
# Intervalle de confiance de Wald pour Pr(Y > 60)
g(emv_weib) + qnorm(c(0.025, 0.975))*c(se_p60_weib)
# IC inclut des valeurs négatives pour les probabilités!


# Tests pour comparer le modèle simple sous H0 (exponentiel) et 
# le modèle complet sous l'alternative Ha (Weibull)

# Paramètres des EMV sous H0
pars0 <- c(mean(attente), 1)
# Tests de Wald - forme quadratique (wald2) et normale (wald1)
wald2 <- c(t(emv_weib - pars0) %*% info_weib %*% (emv_weib - pars0))
wald <- (emv_weib[2] - 1) / sqrt(solve(info_weib)[2,2])
# Wald2 est techniquement le carré de Wald
# ce n'est pas exactement le cas en raison de problèmes d'estimation numérique
wald^2 - wald2
# Valeurs-p (comparaison à une loi normale pour wald, khi-deux avec 1 ddl pour wald2
2*pnorm(abs(wald), lower.tail = FALSE)
pchisq(wald2, df = 1, lower.tail = FALSE)
# Test du rapport de vraisemblance
R <- 2*(logvrais_weib(pars) - logvrais_weib(pars0))
pchisq(R, df = 1, lower.tail = FALSE)
# Le test du score nécessite l'obtention de l'information de Fisher de la vraisemblance profilée
# On fait le calcul pour un IC avec le modèle exponentiel (plus simple)

# Test du score, version unidimensionnelle (directionnelle)
test_score_exp <- function(lambda){
  score_exp(lambda)/sqrt(fisher_info_exp(lambda))
}
s <- test_score_exp(lambda)
#Tracer la courbe (normale)
plot(x = lambda, y = s, type = "l", ylab = "statistique du score")
# Limites pour les intervalles de confiance (quantiles de la loi normale)
abline(h = qnorm(c(0.025, 0.975)), lty = 3, col = "grey")
# Obtenir les bornes pour le score
ic_sc <- predict(smooth.spline(x = s, y = lambda), x = qnorm(c(0.025, 0.975)))$y
# Vérifier que l'approximation est bonne
abline(v = ic_sc, lty = 3, col = "grey")
# Intervalle de confiance du score pour le paramètre d'échelle du modèle exponentiel
ic_sc

# Vraisemblance profilée
# EMV conditionnels de lambda pour alpha donné
lambda_alpha <- function(alpha, y = attente) {
  (mean(y^alpha))^(1 / alpha)
}
# log-vraisemblance profilée pour alpha
prof_alpha_weibull <- function(par, y = attente) {
  sapply(par, function(a) {
    logvrais_weib(pars = c(lambda_alpha(a), a), y = y)
  })
}

# Calcul de la vraisemblance profilée
alpha_s <- seq(1,4, length.out = 1001)
prof <- prof_alpha_weibull(par = seq(1,4, length.out = 1001))
# Valeur maximale de la log-vraisemblance ou vraisemblance profilée identique
plot(x = alpha_s,
     y = prof - logvrais_weib(emv_weib),
     type = "l", # tracer une courbe (plutôt que "p" pour points)
     xlab = expression(alpha),
     ylim = c(-8, 0),
     ylab = "log-vraisemblance profilée",
     panel.first = {abline(h = -qchisq(0.95, 1)/2, lty = 2)})
# Calcul des bornes de l'IC basé sur le rapport de vraisemblance
r <- sign(alpha_s - emv_weib[2])*sqrt(2*(logvrais_weib(emv_weib) - prof))
# Plus la courbe r est "linéaire", plus la log-vraisemblance est
# quadratique et meilleure est l'approximation de Wald.
# Intervalle de confiance à 95%
ic_rv <- predict(smooth.spline(x = r, y = alpha_s), x = qnorm(c(0.025, 0.975)))$y
abline(v = ic_rv, lty = 3, col = "grey")
