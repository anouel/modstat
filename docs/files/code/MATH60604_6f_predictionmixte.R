library(lme4)
data(mobilisation, package = "hecmodstat")

mixte1 <- lme4::lmer(mobilisation ~ ( 1 | idunite ) + 
                       sexe + agegest + anciennete + nunite, 
                     data = mobilisation)

# Prédictions moyenne et individus
nouv_mobilisation <- data.frame(nunite = rep(9, 2), 
                       idunite = factor(c(1,101)), 
                       anciennete = rep(5,2), 
                       sexe = rep(0,2),
                       idemploye = c(10,1),
                       agegest = rep(40,2),
                       mobilisation = rep(NA, 2))

# Prédiction de la moyenne marginale de la population
# sans effets aléatoires
predict(mixte1, newdata = nouv_mobilisation, re.form = NA)
# Pr/dictions conditionnelles (avec effet aléatoire si prédit)
predict(mixte1, newdata = nouv_mobilisation[1,])
