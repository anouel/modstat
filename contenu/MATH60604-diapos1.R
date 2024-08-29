# Diapos 1, MATH 60604

## Télécharger le paquet contenant les données
# remotes::install_github("lbelzile/hecmodstat")
## Charger la base de données
data(distraction, package = "hecmodstat")
# Calcul du test-t pour données appariées
# Hypothèse unilatérale
ttest <- with(distraction,
              t.test(x = t,
                     y = c,
                     paired = TRUE,
                     alternative = "greater"))
# Équivalent au test-t pour un échantillon 
# si on prend la différence comme variable réponse
ttest <- with(distraction,
              t.test(x = t - c,
                     alternative = "greater"))
# Extraire la valeur-p et l'intervalle de confiance
ttest$p.value
ttest$conf.int

# Calculs à la mitaine (pour vérifier la sortie)
d <- with(distraction, t - c) # différence de temps de distraction 
# (texter vs conversation)
n <- length(d) # taille d'échantillon
(moy_d <- mean(d)) # moyenne des différences
(errtype_d <- sd(d)/sqrt(n)) # erreur-type de la moyenne
(stat <- moy_d/errtype_d) # statistique du test-t
ddl <- n - 1L # degrés de liberté
crit <- qt(p = 0.05, df = ddl) # valeur critique, "qt" est la fonction quantile d'une loi t (Student)
(valp <- pt(q = stat, df = ddl, lower.tail = FALSE)) # Pr(T > stat)
(ic_inf <- moy_d + errtype_d*crit) # borne inférieure de l'intervalle de confiance de Wald
# Puisque c'est un test unilatéral, la borne supérieure est infinie

# Malheureusement, les sorties R pour les tests ne sont pas standardisées
# le paquet "broom" contient une fonction, "tidy", qui permet d'obtenir des
# libellés standards et retourne une base de données (data.frame) plutôt qu'une liste
broom::tidy(ttest)
