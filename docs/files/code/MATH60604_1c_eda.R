library(ggplot2)
library(poorman)
library(viridis)

theme_set(theme_minimal())
options(ggplot2.continuous.colour="viridis")
options(ggplot2.continuous.fill = "viridis")
scale_colour_discrete <- scale_colour_viridis_d
scale_fill_discrete <- scale_fill_viridis_d

data(renfe, package = "hecmodstat")
ggplot(data = renfe, 
       aes(x = forcats::fct_infreq(classe))) + 
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  labs(x = "classe", 
       y = "dénombrement")  +
  scale_y_continuous(expand = c(.125, 0)) + 
  theme(panel.grid.major.x = element_blank())


renfe %>% subset(tarif == "Promo") %>%
  ggplot(aes(x = prix)) + 
    geom_histogram(aes(y = ..density..), bins = 30) +
    geom_density() + 
    geom_rug(sides = "b") + 
    labs(x = "prix de billets au tarif Promo (en euros)", 
         y = "densité") 


renfe %>% subset(tarif == "Promo") %>%
    ggplot(aes(y = prix, x = classe, col = type)) + 
    geom_boxplot() + 
    labs(y = "prix (en euros)", col = "type de train") + 
    theme(legend.position = "bottom") +
    scale_colour_viridis_d()


renfe %>% subset(type != "REXPRESS") %>%
    ggplot(aes(x = duree, y = prix, col = type)) + 
    geom_point() + 
    labs(y = "prix (en euros)", 
         x = "durée de trajet (en minutes)",
         col = "type de train") + 
    theme(legend.position = "bottom") +
    scale_colour_viridis_d()

         