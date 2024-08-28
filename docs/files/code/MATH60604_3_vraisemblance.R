n <- 10
pchapeau <- 0.8
nreussite <- 8

# Calculer l'EMV numériquement (correct à arrondi près)
optimize(f = function(x){-dbinom(x = nreussite, size = 10, prob = x, log = TRUE)}, interval = c(0,1))

# Calculer l'observation observée par différence finie
obsinfo <- -numDeriv::hessian(function(x){dbinom(x = nreussite, size = n, prob = x, log = TRUE)}, x = 0.8)
sqrt(solve(obsinfo))
# Vérifier avec la formule (information observée évaluée à l'EMV)
se_pchapeau <- sqrt(pchapeau*(1-pchapeau)/n)

#Intervalles de confiance
#Test de Wald
c(pchapeau - qnorm(0.975)*se_pchapeau, pchapeau + qnorm(0.975)*se_pchapeau)
#Test du rapport de vraisemblance
lrt <- function(p){2*(dbinom(x = nreussite, size = n, prob = pchapeau, log = TRUE) - dbinom(x = 8, size = n, prob = p, log = TRUE)) - qchisq(0.95, 1)}
c(uniroot(lrt, interval = c(0.4,pchapeau))$root, uniroot(lrt, interval = c(pchapeau,1))$root)
#Test du score
score <- function(p){
  (pchapeau-p)^2/(p*(1-p)/n) - qchisq(0.95, 1)
}
c(uniroot(score, interval = c(0,pchapeau))$root, uniroot(score, interval = c(pchapeau,1))$root)
