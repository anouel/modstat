data(intention, package = "hecmodstat")

mod <- lm(intention ~ fixation + emotion + revenu + 
             educ + statut + sexe, data = intention)
# Ajuster un modèle linéaire avec la machinerie GLM
mod <- glm(intention ~ fixation + emotion + revenu + 
             educ + statut + sexe, data = intention, family=gaussian)
summary(mod)
#Diagnostics graphiques pour modèles linéaire généralisés
boot::glm.diag.plots(mod)
