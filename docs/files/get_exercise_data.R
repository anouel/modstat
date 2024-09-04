exdir <- "../files/exercices"
linkstring <- "https://nbviewer.jupyter.org/github/lbelzile/modstat/blob/main/files/exercices/"
linkgithub <- "https://raw.githubusercontent.com/lbelzile/modstat/main/files/exercices/"

## exercise and solution files
fn <- FALSE

ex <- list.files(path = exdir, pattern = "MATH60604-Exercice[[:digit:]].pdf", 
    full.names = fn)
coder <- list.files(path = exdir, pattern = "MATH60604-Exercice[[:digit:]].R", 
                      full.names = fn)
rc <- rep("",7)
rc[as.integer(substr(coder, start = 19, stop = 19))] <- 
  paste0("[<span style='color: #276dc2;'><i class='fab fa-r-project fa-lg'></i></span>](", linkgithub, coder, ")")


so <- list.files(path = exdir, pattern = "MATH60604-Exercice[[:digit:]]-sol.pdf", 
    full.names = fn)



## Numbers + Topics
# exid <- as.numeric(gsub("[^0-9.-]+", "", ex))
topics <- 
  c("Bases de l'inférence statistique", 
    "Régression linéaire",
    "Vraisemblance",
    "Modèles linéaires généralisés",
    "Données corrélées et longitudinales",
    "Modèles linéaires mixtes",
    "Analyse de survie")

exos <- c(paste0("[<span style='color: #4b5357;'><i class='fas fa-file-pdf fa-lg'></i></span>](", linkgithub, ex, ")"),
  rep("", length.out = 7-length(ex)))
soln <- c(paste0("[<span style='color: #bfc2c5;'><i class='far fa-file-pdf fa-lg'></i></span>](", linkgithub, so, ")"),
          rep("", length.out = 7-length(so)))
exdat <- data.frame(Chapitre = topics,
                    Exercice = exos,
                    Solution = soln,
                    Code = rc)

