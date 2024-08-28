library(ggplot2)

data(intention, package = "hecmodstat")

pred_intent <- intention[rep(1L,7),]
pred_intent$fixation <- 0:6

mod1 <- lm(intention ~ fixation + emotion + sexe + age + revenu + educ,
           data = intention)
pred1 <- predict(mod1, newdata = pred_intent, interval = "confidence")
pred2 <- predict(mod1, newdata = pred_intent, interval = "prediction")

# Transformer la matrice de prédictions en 
# data.frame pour ggplot
pred1 <- data.frame(pred1)
pred2 <- data.frame(pred2)
pred1$x <- pred2$x <- pred_intent$fixation

ggplot(data = pred1, aes(x = x, y = fit)) +
  geom_line() +
  geom_ribbon(data = pred1, 
              mapping = aes(x = x, 
                            ymin = lwr,
                            ymax = upr),
              col = 1, alpha = 0.2) + 
  geom_ribbon(data = pred2, 
              mapping = aes(x = x, 
                            ymin = lwr,
                            ymax = upr),
              col = 1, alpha = 0.1) +
  xlab("temps de fixation (en secondes)") + 
  ylab("intention d'achat")

# Exemple plus réaliste avec les données "college"
data(college, package = "hecmodstat")
mod_col <- lm(salaire ~ domaine + echelon + service + sexe, 
              data = college)
nouv_college <- data.frame(annees = 5, 
                           domaine = factor("applique"),
                           echelon = factor("titulaire"),
                           service = 3,
                           sexe = "homme")
predict(mod_col, 
        newdata = nouv_college, 
        interval = "prediction")

