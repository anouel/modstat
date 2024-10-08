# Diapos 2, MATH 60604

# Paquets R pour les graphiques
library(ggplot2)
library(patchwork)
## Charger les données
data(attente, package = "hecmodstat")
# Si vous obtenez un message d'erreur, rechargez les données
# remotes::install_github("lbelzile/hecstatmod")
#
## Histogramme du temps d'attente avec traits pour les observations
ggplot(data = data.frame(time = attente), mapping = aes(x = time)) +
  geom_histogram(bins = 10) +
  geom_rug() +
  labs(x = "temps d'attente (en secondes)")

# Log vraisemblance exponentielle
exp_loglik <- function(lambda) {
  sum(dexp(attente, rate = 1 / lambda, log = TRUE))
}
# Créer un vecteur de valeurs pour le paramètre d'échelle "lambda"
lambda_cand <- seq(min(attente) + 10, max(attente), by = 1)
# Calculer la log vraisemblance pour chaque valeur de cette grille
ll_attente <- sapply(lambda_cand, exp_loglik)
# Tracer la log vraisemblance
ggplot(data = data.frame(x = lambda_cand, y = ll_attente),
       mapping = aes(x = x, y = y)) +
  geom_line() +
  # Ajouter une ligne verticale pour l'EMV
  geom_vline(xintercept = mean(attente), linetype = "dashed") +
  labs(x = expression(lambda), y = "log vraisemblance")

# Calculer la probabilité d'attendre plus de 60 secondes
pexp(q = 60,
     rate = 1 / mean(attente),
     lower.tail = FALSE)

### Démo R

# Négatif de la log vraisemblance pour un échantillon Weibull
nll_weibull <- function(pars, y) {
  # Gérer le cas de paramètres négatifs (impossible)
  if (isTRUE(any(pars <= 0))) {
    return(1e10) # retourner une valeur large finie (pour éviter les messages d'avertissement)
  }
  - sum(dweibull(
    x = y,
    scale = pars[1],
    shape = pars[2],
    log = TRUE
  ))
}
# Gradient du négatif de la fonction de log vraisemblance Weibull
gr_nll_weibull <- function(pars, y) {
  scale <- pars[1]
  shape <- pars[2]
  n <- length(y)
  grad_ll <- c(
    scale = -n * shape / scale + shape * scale^(-shape - 1) * sum(y^shape),
    shape = n / shape - n * log(scale) + sum(log(y)) -
      sum(log(y / scale) * (y / scale)^shape)
  )
  return(-grad_ll)
}

# Utiliser les EMV du modèle exponentiel pour l'initialisation
valinit <- c(mean(attente), 1)
# Vérifier préalablement que le gradient est correct!
# La commande retourne TRUE si la dérivée numérique égale sa version analytique à tolérance donnée
isTRUE(all.equal(
  numDeriv::grad(nll_weibull, x = valinit, y = attente),
  gr_nll_weibull(pars = valinit, y = attente),
  check.attributes = FALSE
))
# Optimisation numérique avec optim
opt_weibull <- optim(
  par = valinit,
  # valeurs initiales
  fn = nll_weibull,
  # passer la fonction à optimiser, son premier argument doit être le vecteur de paramètres
  gr = gr_nll_weibull,
  # gradient (optionnel)
  method = "BFGS",
  # algorithme BFGS est basé sur le gradient, une alternative robuste est"Nelder"
  y = attente,
  # vecteur d'observations passées en argument additionnel à "fn"
  hessian = TRUE # retourner la matrice de dérivée secondes évaluée aux EMV
) 
# Alternative avec un Newton
# nlm(f = nll_weibull, p = valinit, hessian = TRUE, y = attente)
# Estimations du maximum de vraisemblance
(mle_weibull <- opt_weibull$par)
# Vérifier la convergence numérique à l'aide du gradient
gr_nll_weibull(mle_weibull, y = attente)
# Vérifier que la hessienne est positive définite
# Toutes les valeurs propres sont positives
# Si oui, on a trouvé un maximum et la matrice est invertible
isTRUE(all(eigen(opt_weibull$hessian)$values > 0))

# La hessienne du négatif de la log vraisemblance, évaluée aux EMV
# est la matrice d'information observée
obsinfo_weibull <- opt_weibull$hessian
vmat_weibull <- solve(obsinfo_weibull)
# Erreurs-type
se_weibull <- sqrt(diag(vmat_weibull))

# Exemple de dérivation des erreurs-type pour une
# transformation des paramètres
# Ici, on calcule Pr(Y>60) selon le modèle exponentiel
lambda_hat <- mean(attente)
# Définir la fonction d'intérêt
phi_hat <- exp(-60 / lambda_hat)
# jacobien de la transformation
dphi <- function(lambda) {
  60 * exp(-60 / lambda) / (lambda^2)
}
# variance du paramètre exponentiel
V_lambda <- lambda_hat^2 / length(attente)
# variance de Pr(Y>60) via la méthode delta
V_phi <- dphi(lambda_hat)^2 * V_lambda
# extraire et imprimer les erreurs-type
(se_phi <- sqrt(V_phi))


# Calcul de la surface de log vraisemblance
nll <- matrix(nrow = 101, ncol = 100)
alpha <- seq(mle_weibull[2] - 2.5 * se_weibull[2],
             mle_weibull[2] + 2.5 * se_weibull[2],
             length.out = 100)
lambda <- seq(mle_weibull[1] - 2.5 * se_weibull[1],
              mle_weibull[1] + 2.5 * se_weibull[1],
              length.out = 101)
z <- rep(NA, length(nll))
for (i in seq_along(lambda)) {
  for (j in seq_along(alpha)) {
    z[(i - 1) * 100 + j] <- nll[i, j] <-
      nll_weibull(pars = c(lambda[i], alpha[j]), y = attente)
  }
}


# Tracer la surface de log vraisemblance avec les 
# régions de confiance basées sur le rapport de vraisemblance R
ggplot() +
  geom_raster(data = data.frame(
    x = rep(lambda, each = length(alpha)),
    y = rep(alpha, length.out = length(alpha) *
              length(lambda)),
    z = c(-z + opt_weibull$value)
  ),
  mapping = aes(
    x = x,
    y = y,
    fill = pchisq(-2 * z, df = 2)
  )) +
  geom_contour(
    data = data.frame(
      x = rep(lambda, each = length(alpha)),
      y = rep(alpha, length.out = length(alpha) *
                length(lambda)),
      z = c(-z + opt_weibull$value)
    ),
    mapping = aes(x = x, y = y, z = z),
    col = "white",
    breaks = -qchisq(seq(0.1, 0.9, by = 0.1), df = 2) / 2
  ) +
  # Ajouter une croix pour les EMV
  geom_point(
    shape = 4,
    color = "white",
    data = data.frame(x = mle_weibull[1], y = mle_weibull[2]),
    mapping = aes(x = x, y = y)
  ) +
  # Changer la palette de couleur
  scale_fill_viridis_c(direction = 1, option = "viridis") +
  labs(x = expression(paste("scale ", lambda)),
       y = expression(paste("shape ", alpha)),
       fill = "probability level",) +
  scale_y_continuous(expand = expansion(), limits = range(alpha)) +
  scale_x_continuous(expand = expansion(), limits = range(lambda)) +
  theme(legend.position = "bottom")


# Comparaison entre les modèles Weibull et exponentiel
# Calcul de la statistique de Wald pour H0: alpha=1
wald_exp <- (mle_weibull[2] - 1) / se_weibull[2]
# Calcul de la valeur-p
pchisq(wald_exp^2, df = 1, lower.tail = FALSE)
# la valeur-p est inférieure à 5%, rejet de l'hypothèse nulle
# Intervalle de confiance 95% de Wald pour le paramètre de forme alpha
mle_weibull[2] + qnorm(c(0.025, 0.975)) * se_weibull[2]
# la valeur 1 n'appartient pas à l'intervalle, rejetter H0

# Diagrammes quantile-quantile
n <- length(attente)
set.seed(1234)
g1 <- ggplot() +
  stat_qq(
    data = data.frame(y = attente),
    mapping = aes(sample = y),
    distribution = qexp,
    dparams  = list(rate = 1 / mean(attente))
  ) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "quantiles théoriques", y = "quantiles empiriques", subtitle = "exponentiel")
# Estimer les paramètres à l'aide des routines
fitweibull <- MASS::fitdistr(x = attente, densfun = "weibull")
# Extraire les estimations des paramètres
shape <- fitweibull$estimate['shape']
scale <- fitweibull$estimate['scale']
# Diagramme quantile-quantile du modèle Weibull
g2 <- ggplot() +
  # Calcul manuel des positions (pour illustrer l'opération)
  geom_point(data = data.frame(
    y = sort(attente),
    x = qweibull(ppoints(n), scale = scale, shape = shape)
  ),
  mapping = aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "quantiles théoriques", y = "quantiles empiriques", subtitle = "Weibull")
g1 + g2



# Log vraisemblance exponentielle
ll_exp <- function(lambda) {
  sum(dexp(attente, rate = 1 / lambda, log = TRUE))
}
# EMV du paramètre d'échelle
lambda_hat <- mean(attente)
# Recherche des zéros de la fonction pour obtenir
# les limites des intervalles de confiance
lrt_lb <- uniroot(
  # borne inférieure, en utilisant l'EMV
  f = function(r) {
    2 * (ll_exp(lambda_hat) - ll_exp(r)) - qchisq(0.95, 1)
  },
  interval = c(0.5 * min(attente), lambda_hat)
)$root
lrt_ub <- uniroot(
  # borne supérieure
  f = function(r) {
    2 * (ll_exp(lambda_hat) - ll_exp(r)) - qchisq(0.95, 1)
  },
  interval = c(lambda_hat, 2 * max(attente))
)$root


# Log vraisemblance profilée

# EMV conditionnels de lambda pour alpha donné
lambda_alpha <- function(alpha, y = attente) {
  (mean(y^alpha))^(1 / alpha)
}
# Log vraisemblance profilée pour alpha
prof_alpha_weibull <- function(par, y = attente) {
  sapply(par, function(a) {
    nll_weibull(pars = c(lambda_alpha(a), a), y = y)
  })
}
ggplot() +
  stat_function(
    fun = function(par) {
      opt_weibull$value - prof_alpha_weibull(par)
    },
    xlim = c(1.8, 3.5),
    n = 1001L
  ) +
  geom_hline(yintercept = -qchisq(c(0.95, 0.99), df = 1) / 2, linetype = "dashed") +
  labs(x = expression(paste("shape ", alpha)), y = "profile log likelihood")


# Vraisemblance profilée de l'espérance d'une loi Weibull
# Calculer les EMV par substitution (invariance)
mu_hat <- mle_weibull[1] * gamma(1 + 1 / mle_weibull[2])
# Créer une fonction pour la vraisemblance profilée
prof_weibull_mu <- function(mu) {
  # Pour chaque valeur de mu
  alpha_mu <- function(mu) {
    # Obtenir la vraisemblance profilée en optimisant (recherche linéaire)
    # pour une valeur de "mu" donnée afin de trouver alpha_mu
    opt <- optimize(
      f = function(alpha, mu) {
        # on minimise le négatif de la log vraisemblance 
        nll_weibull(c(mu / gamma(1 + 1 / alpha), alpha), y = attente)
      },
      mu = mu,
      interval = c(0.1, 10) #région pour les paramètres
    )
    # retourner la valeur de la log vraisemblance (négatif) et de alpha_mu
    return(c(nll = opt$objective, alpha = opt$minimum))
  }
  # Créer une base de données pour "mu" et les autres paramètres
  data.frame(mu = mu, t(sapply(mu, function(m) {
    alpha_mu(m)
  })))
}
# Créer une base de données pour la vraisemblance profilée
prof <- prof_weibull_mu(seq(22, 35, length.out = 101L))
# Calculer la racine directionnelle du rapport de vraisemblance, r
prof$r <- sign(prof$mu - mu_hat) * sqrt(2 * (prof$nll - opt_weibull$value))

# Astuce: ajouter une spline cubique pour obtenir les prédictions avec
# mu en fonction de r. On utilise ensuite ce modèle pour prédire la valeur
# de mu à laquelle r coupe les quantiles standard normaux de l'IC

fit.r <- stats::smooth.spline(x = cbind(prof$r, prof$mu), cv = FALSE)
pr <- predict(fit.r, qnorm(c(0.025, 0.975)))$y
# Tracer la racine directionnelle du rapport de vraisemblance
# Plus cette courbe s'approche d'une droite, plus la vraisemblance est symmétrique/quadratique
g1 <- ggplot(data = prof, mapping = aes(x = mu, y = r)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_line() +
  geom_hline(yintercept = qnorm(0.025, 0.975), linetype = "dashed") +
  labs(x = expression(paste("espérance ", mu)), y = "racine directionnelle du rapport de vraisemblance")
# Tracer la courbe de log vraisemblanc profilée
g2 <- ggplot(data = prof,
             mapping = aes(x = mu, y = opt_weibull$value - nll)) +
  geom_line() +
  geom_hline(yintercept = -qchisq(c(0.95), df = 1) / 2, linetype = "dashed") +
  geom_vline(linetype = "dotted", xintercept = pr) +
  labs(x = expression(paste("espérance ", mu)), y = "log vraisemblance profilée")

g1 + g2
