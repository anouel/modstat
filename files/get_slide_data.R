index <- c("0_","1a","1b","1c",
           paste0(2, letters[1:9]),"3_", 
           paste0(4, letters[1:8]),
           paste0(5, letters[1:8]),
           paste0(6, letters[1:6]),
           paste0(7, letters[1:5]))
index0 <- c("0","1a","1b","1c",
            paste0(2, letters[1:9]),"3", 
            paste0(4, letters[1:8]),
            paste0(5, letters[1:8]),
            paste0(6, letters[1:6]),
            paste0(7, letters[1:5]))
names <- c("Plan de cours",
           "Tests d'hypothèses",
           "Théorème central limite",
           "Analyse exploratoire",
           "Interprétation des paramètres  (modèle linéaire)",
           "Transformations linéaires",
           "Géométrie des moindres carrés",
           "Tests d'hypothèses (modèle linéaire)",
           "Coefficient de détermination",
           "Prédictions",
           "Interactions",
           "Colinéarité",
           "Diagnostics graphiques des résidus",
           "Vraisemblance",
           "Modèles linéaires généralisés",
           "Régression logistique",
           "Exemple de régression logistique",
           "Régression de Poisson",
           "Tableaux de contingence",
           "Surdispersion",
           "Taux et décalage",
           "Régression logistique pour les taux",
           "Introduction aux données corrélées",
           "Exemple de données longitudinales",
           "Formulation de modèles",
           "Structure d'équicorrélation",
           "Structure autorégressive",
           "Autres modèles de covariance",
           "Comparaison des modèles de covariance",
           "Hétéroscédasticité de groupe",
           "Effet groupe pour la moyenne",
           "Exemple de données corrélées",
           "Modèles linéaires mixtes",
           "Ordonnée à l'origine aléatoire",
           "Pente aléatoire",
           "Prédictions de modèles mixtes",
           "Concepts d'analyse de survie",
           "Survie et vraisemblance",
           "Estimateur de Kaplan-Meier",
           "Modèle à risques proportionnels de Cox",
           "Test du log rang"
           )
ns <- length(index)
url <- "https://lbelzile.github.io/MATH60604-diapos/"
codedir <- "../files/code"

get_github_list <- function(user, repo, pattern){
  req <- httr::GET(paste0("https://api.github.com/repos/", user, "/", repo, "/git/trees/main?recursive=1"))
  httr::stop_for_status(req)
  filelist <- unlist(lapply(httr::content(req)$tree,
                            "[", "path"), 
                     use.names = FALSE)
  grep(x = filelist, value = TRUE, pattern = pattern)
}


slides <- get_github_list(user = "lbelzile", 
                          repo = "MATH60604-diapos",
                          pattern = "MATH60604_d.*\\.html")
sl <- rep("", ns)
pmasl <- na.omit(pmatch(substr(slides, start = 12, stop = 13), index))
sl[pmasl] <- paste0("[<span style='color: #4b5357;'><i class='fas fa-desktop fa-lg'></i></span>](",url, slides,")")

slidespdf <- get_github_list(user = "lbelzile", 
                             repo = "MATH60604-diapos",
                             pattern = "MATH60604_d.*.pdf")
slpdf <- rep("", ns)
pmaslp <- na.omit(pmatch(substr(slidespdf, start = 12, stop = 13),index))
slpdf[pmaslp] <- paste0("[<span style='color: #4b5357;'><i class='fas fa-file-pdf fa-lg'></i></span>](",url, slidespdf,")")



video <- c("https://youtu.be/kC5S4h0bIaw", 
           "https://youtu.be/6Qe4mi6A9bU", 
           "https://youtu.be/a2cpzb1EzGk",
           "https://youtu.be/2vK0zEX6dSA",
           "https://youtu.be/-R-5muz1mGY",
           "https://youtu.be/4xczymEfVng",
           "https://youtu.be/2iKH0tBb6w4",
           "https://youtu.be/EjSlJJ5CUJY", #tests 2d
           "https://youtu.be/QtfhZCMHVAo", #correlation 2e
           "https://youtu.be/GZx7_5tmzCg", # prédictions 2f
           "https://youtu.be/0UrxG4hkDcg", #interactions 2g
           "https://youtu.be/6RxdcERzdFo", # colinearite 2h
           "https://youtu.be/AZ6Rr1B_r7c", # diagnostics 2i
           "https://youtu.be/VJItA6EX5-s", #vraisemblance 3
           "https://youtu.be/YUp6IZbrwQ0", #4a glm
           "https://youtu.be/r_1NfKdqy5M", #4b logistique
           "https://youtu.be/S6gXUa-9zvE", #4c logistique exemple
           "https://youtu.be/sjt9lKLQbZ8", #4d poisson
           "https://youtu.be/oNDi1Bel84g", #4e tableaux de contingence
           "https://youtu.be/YS7UE9N4HMc", #4f surdispersion
           "https://youtu.be/Hl2c48rX98g", #4g taux et décalage
           "https://youtu.be/6a8aoXy8Ag0", #4h régression logistique pour taux
           "https://youtu.be/ytwbRVTXAfc", #5a
           "https://youtu.be/X2aWOAwi3s8", #5b
           "https://youtu.be/dep7O7jZEqw", #5c
           "https://youtu.be/150FoT7b4TM", #5d
           "https://youtu.be/luy5E-thK_s", #5e
           "https://youtu.be/owY9s0HINYg", #5f
           "https://youtu.be/3dJ_u-Svr6Y", #5g
           "https://youtu.be/3S0GQ7FJfn0", #5h
           "https://youtu.be/7X09JMf_HtY", #6a
           "https://youtu.be/lpQSuZmmuH0", #6b
           "https://youtu.be/A-YotBQLd5Q", #6c
           "https://youtu.be/kPbguCg05e0", #6d
           "https://youtu.be/lnNfzyKGglA", #6e
           "https://youtu.be/V6-xQXuMTzo", #6f
           "https://youtu.be/YZbgs4aj_Bg", #7a
           "https://youtu.be/S_WjH5u4SC8", #7b
           "https://youtu.be/iWNid5x4BKE", #7c
           "https://youtu.be/d5wRThO3lLA", #7d
           "https://youtu.be/yuSZNBt8Pxk"
           )
videosl <- paste0("[<span style='color: red;'><i class='fab fa-youtube fa-lg'></i></span>](",video,")")
if(length(names) - length(video) > 0){
  videosl <- c(videosl, rep("", length(names) - length(video)))
}
linkgithub <- "https://raw.githubusercontent.com/lbelzile/modstat/main/files/"


codesas <- list.files(path = codedir, pattern = "MATH60604.*.sas")
codestr <- rep("", ns)
nid <- na.omit(pmatch(substr(codesas,11,12), index))
codestr[nid] <- paste0("[<span style='color: #4b5357;'><i class='fas fa-file-code fa-lg'></i></span>](",linkgithub, "code/", codesas,")")

codeR <- list.files(path = codedir, pattern = "MATH60604.*.R")
codeRstr <- rep("", ns)
nid <- na.omit(pmatch(substr(codeR,11,12), index))
codeRstr[nid] <- paste0("[<span style='color: #276dc2;'><i class='fab fa-r-project fa-lg'></i></span>](",linkgithub,"code/", codeR,")")


sldat <- data.frame('S' = index0, 
                    Contenu = names,
                    HTML = sl,
                    Diapos = slpdf,
                    Vidéos = videosl,
                    #"SAS" = codestr,
                    "R" = codeRstr)
