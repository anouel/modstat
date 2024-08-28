library(ggplot2)
library(patchwork)
set.seed(1234)
# Simuler de fausses données d'un modèle linéaire
n <- 100 #nombre d'observations
X1 <- rpois(n = n, lambda = 2) # covariables (fixées)


############################################################
#### Distribution d'échantillonage des MCO des betas #######
############################################################

# La distribution d'échantilonnage des estimateurs des moindres carrés ordinaires est
# beta_chapeau ~ N(beta, sigma^2*(X^TX)^(-1))
design_mat <- cbind(1, X1)
# Les erreurs-type de beta_chapeau (sigma=1)
theo_se <- sqrt(diag(solve(crossprod(design_mat))))
sampling_dist <- 
  replicate(n = 1000, expr = {
  eps <- rnorm(n, mean = 0, sd = 1) #aléas N(0,1)
  Y <- 8 + 0.2 * X1 + eps #erreurs indépendantes de  X1
  mod1 <- lm(Y ~ X1)
  coef(mod1)
 })
# Graphique de la distribution d'échantillonage "théorique" superposée (en rouge)
g1 <- ggplot(data = data.frame(intercept = sampling_dist[1,])) + 
               geom_histogram(bins = 30, aes(x=intercept, y=..density..)) +
               geom_vline(xintercept = 8) + 
  stat_function(fun = "dnorm", 
                args = list(mean = 8, sd = theo_se[1]), 
                col = "red") +
  labs(x = "ordonnée à l'origine", y = "densité")
g2 <- ggplot(data = data.frame(slope = sampling_dist[2,])) +
               geom_histogram(bins = 30, aes(x=slope, y=..density..)) +
               geom_vline(xintercept = 0.2) + 
  stat_function(fun = "dnorm", 
                args = list(mean = 0.2, sd = theo_se[2]), 
                col = "red") + 
  labs(x = "pente", y = "densité")

g1 + g2

############################################################
####      Vraisemblance profilée pour la pente       #######
############################################################

# Choisir un jeu de données
eps <- rnorm(n, mean = 0, sd = 1)
Y <- 8 + 0.2 * X1 + eps
mod1 <- lm(Y ~ X1)
summary(mod1)

# Estimateurs du maximum de vraisemblance (EMV) = moindres carrés ordinaires (MCO)
beta1hat <- coef(mod1)[2]
# Calculer les valeurs du maximum de (profilée) vraisemblance 
mll <- as.numeric(logLik(mod1))

# Valeurs de la vraisemblance profilée (évaluée ponctuellement)
pll <- function(beta){
  sapply(beta, function(b){
    as.numeric(logLik(lm(Y - b*X1 ~ 1)))
  })
}

# Calculer l'intervalle de confiance
ci_lrt_lb <- uniroot(function(x){2*(mll - pll(x)) - qchisq(0.95,1)}, 
                     interval = c(beta1hat-3, beta1hat))$root
ci_lrt_ub <- uniroot(function(x){2*(mll - pll(x)) - qchisq(0.95,1)}, 
                     interval = c(beta1hat, beta1hat+3))$root
# Calculer la vraisemblance profilée sur une grille de valeurs de beta_1
beta_grid <- seq(from = -2*theo_se[1] + 0.2, to = 2*theo_se[1] + 0.2, length.out = 101)
ggplot(data.frame(beta = beta_grid, 
                pll = pll(beta_grid)-mll),
       aes(x = beta, y = pll)) + 
  geom_line() + 
  geom_vline(xintercept = coef(mod1)[2], col = "red") + 
  geom_hline(yintercept = -qchisq(0.95, 1)/2) + 
  geom_vline(xintercept = ci_lrt_lb, col = "gray", lty = 2) + 
  geom_vline(xintercept = ci_lrt_ub, col = "gray", lty = 2) +
  ylab("log-vraisemblance profilée") + 
  xlab(expression(beta[1]))

############################################################
####  Comparaison du test-F versus loi khi-deux      #######
############################################################
# La loi F(q, n-p) est intimement reliée à la loi khi-deux
# q*khi-deux ~ F(q, n-p) pour n grand
# 
# Exemple avec les données assurance
ggplot() + 
  stat_function(fun = "df", 
                args = list(df1=3, df2=1330)) +
  stat_function(fun = "df", 
                args = list(df1=1, df2=1330), col  =2) +
  # stat_function(fun = function(x, df){df*dchisq(x*df, df = df)}, 
  #               args = list(df=3),
  #               col = "yellow", lty = 2) +
  xlim(0, 15)
# La loi normale et la loi Student donnent des points de coupure presque identiques
qt(0.975, df = 1330)
qnorm(0.975)
# La loi Fisher(1, n-p) est celle du carré d'une variable Student(n-p)^2
isTRUE(all.equal(
  qf(0.95, df1 = 1, df2 = 1330),
  qt(0.975, df = 1330)^2))


## Comparaison de modèles emboîtés
set.seed(1234)
n <- 100L; 
# Générer de fausses covariables
x1 <- rnorm(n)
x2 <- rbinom(n, 1, 0.3)
x3 <- rpois(n, 2)
# Simuler des observations d'un modèle linéaire
y <- 2 + 3*x1 + 2*x2 + 4*x3 + rnorm(100, sd=1)
# Ajuster le modèle linéaire
mod1 <- lm(y ~ x1)
mod2 <- lm(y ~ x1 + x2 + x3)
mod3 <- lm(y ~ x1 * x2 + x3)
summary(mod1)
summary(mod2)
summary(mod3)
# Calculer la statistique du rapport de vraisemblance
dev_a <- 2*as.numeric(logLik(mod2) - logLik(mod1))
dev_b <- 2*as.numeric(logLik(mod3) - logLik(mod2))
# Calculer les valeurs-p des tests
pchisq(dev_a, df = 2, lower.tail = FALSE)
pchisq(dev_b, df = 1, lower.tail = FALSE)

# Test du rapport de vraisemblance avec loi de référence asympotique (khi-deux)
lmtest::lrtest(mod1, mod2)
lmtest::lrtest(mod2, mod3)
# test du rapport de vraisemblance avec statistique F
anova(mod2, mod3) # loi F exacte
anova(mod2, mod3, test = "Chisq") # approximation asymptotique

# Comparer AIC/BIC
c(AIC(mod1), AIC(mod2), AIC(mod3))
c(BIC(mod1), BIC(mod2), BIC(mod3))



