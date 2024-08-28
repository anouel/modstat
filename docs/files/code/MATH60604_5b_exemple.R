library(ggplot2)
data(vengeance, package = "hecmodstat")
vengeance$t <- as.integer(vengeance$temps)

vengeance[vengeance$id %in% c(1:3),]
# id < 4 ne fonctionne pas parce que "id" est un facteur...

# Summary statistics and linear correlation matrix
summary(vengeance[vengeance$temps == 1,c("sexe","age","vc","wom")])
cor(vengeance[vengeance$temps == 1,c("sexe","age","vc","wom")])

# Graphique spaghetti
ggplot(data=vengeance, 
       aes(x=t, y=vengeance, group=id)) + 
  geom_line(alpha = 0.2) + 
  scale_x_continuous(expand = c(0,0), limits = c(1,5)) + 
  xlab("vague de questionnaire")
