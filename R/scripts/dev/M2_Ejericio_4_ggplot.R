library(ggplot2)
library(dplyr)

mainDir <- "C:/Users/jlagosmi/Desktop/MASTER BIG DATA/PAQUETE_R/UnderStatPlots/data/Understat"
setwd(file.path(mainDir))

US_season_Total<-readRDS("US_season_Total.RData")
US_shots_Total<-readRDS("US_shots_Total.RData")


# Ejercicio 4.1

Ejercicio<-US_shots_Total %>%
  filter((h_a=='h' & h_team=="Real Madrid" | h_a=='a' & a_team=='Real Madrid'),result!='OwnGoal',year=='2020')

ggplot(Ejercicio,aes(X,Y*0.7,size=xG,colour=xG)) +
  geom_point() +
  scale_colour_gradient2(low = "gray", high = "red",mid = "yellow", midpoint = 0.2) +
  scale_size_area(max_size=10)+
  coord_flip() +
  labs(y = "Esto es la coordenada Y del campo",
       x = "Esto es la coordenada X del campo",
       title = "Zonas de disparo del Real Madrid",
       subtitle = "Temporadas de 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))






