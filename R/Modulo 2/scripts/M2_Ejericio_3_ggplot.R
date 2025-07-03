library(ggplot2)
library(dplyr)

mainDir <- "C:/Users/jlagosmi/Desktop/MASTER BIG DATA/PAQUETE_R/UnderStatPlots/data/Understat"
setwd(file.path(mainDir))

setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")


US_season_Total<-readRDS("US_season_Total.RData")
US_shots_Total<-readRDS("US_shots_Total.RData")

# Ejercicio 3.1

Ejercicio<-US_season_Total %>%
  group_by(league_name,year) %>%
  summarise(xG=mean(xG))

ggplot(US_season_Total,aes(x=(league_name),y=xG)) +
  geom_boxplot(aes(fill=league_name))+
  facet_wrap(~year) +
  coord_flip() +
  theme_bw() +
  labs(x = "Temporada", y = "Goles Esperados (xG)",
       title = "Evolución del promedio de xG por liga y temporada",
       subtitle = "Temporadas desde 2014 a 2020 de las principales ligas europeas",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos",
       color = "Ligas:") +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        legend.title = element_text(color = "black",
                                    size = 14, face = "bold"),
        panel.grid.minor =element_line(linetype = c("28")),
        strip.background = element_rect(color="black", fill="#FC4E07", size=1.5, linetype="solid"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold.italic"),
        strip.text.y = element_text(size = 12, color = "white", face = "bold.italic"),
        legend.position = "null")


# Ejercicio 3.2

Ejercicio<-US_season_Total %>%
  group_by(league_name,year) %>%
  summarise(xG=mean(xG))


cols <- c("Bundesliga" = "red", "EPL" = "blue",
          "La_liga" = "darkgreen", "Ligue_1" = "orange",
          "RFPL" = "gray","Serie_A" = "white")


ggplot(US_season_Total,aes(x=(league_name),y=xG,fill=league_name),color='black') +
  geom_boxplot(aes(fill=league_name))+
  scale_fill_manual(values = cols) +
  facet_wrap(~year) +
  coord_flip() +
  theme_bw() +
  labs(x = "Temporada", y = "Goles Esperados (xG)",
       title = "Evolución del promedio de xG por liga y temporada",
       subtitle = "Temporadas desde 2014 a 2020 de las principales ligas europeas",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos",
       color = "Ligas:") +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        legend.title = element_text(color = "black",
                                    size = 14, face = "bold"),
        panel.grid.minor =element_line(linetype = c("28")),
        strip.background = element_rect(color="black", fill="#FC4E07", size=1.5, linetype="solid"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold.italic"),
        strip.text.y = element_text(size = 12, color = "white", face = "bold.italic"),
        legend.position = "null")





# Ejercicio 3.3


Ejercicio<-US_shots_Total %>%
  filter((h_a=='h' & h_team=="Real Madrid" | h_a=='a' & a_team=='Real Madrid'),result!='OwnGoal')

ggplot(Ejercicio,aes(X,Y*0.7)) +
  geom_bin2d() +
  labs(y = "Esto es la coordenada Y del campo",
       x = "Esto es la coordenada X del campo",
       title = "Zonas de disparo del Real Madrid",
       subtitle = "Temporadas de 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))


# Ejercicio 3.4


Ejercicio<-US_shots_Total %>%
  filter((h_a=='h' & h_team=="Real Madrid" | h_a=='a' & a_team=='Real Madrid'),result!='OwnGoal')

ggplot(Ejercicio,aes(X,Y*0.7)) +
  geom_bin2d() +
  scale_fill_gradient2(low = "gray", high = "red",mid = "yellow", midpoint = 40) +
  labs(y = "Esto es la coordenada Y del campo",
       x = "Esto es la coordenada X del campo",
       title = "Zonas de disparo del Real Madrid",
       subtitle = "Temporadas de 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))



# Ejercicio 3.5


Ejercicio<-US_shots_Total %>%
  filter((h_a=='h' & h_team=="Real Madrid" | h_a=='a' & a_team=='Real Madrid'),result!='OwnGoal',year=='2020')

ggplot(Ejercicio,aes(X,Y*0.7,size=xG)) +
  geom_point() +
  labs(y = "Esto es la coordenada Y del campo",
       x = "Esto es la coordenada X del campo",
       title = "Zonas de disparo del Real Madrid",
       subtitle = "Temporadas de 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))


# Ejercicio 3.6

Ejercicio<-US_shots_Total %>%
  filter((h_a=='h' & h_team=="Real Madrid" | h_a=='a' & a_team=='Real Madrid'),result!='OwnGoal',year=='2020')

ggplot(Ejercicio,aes(X,Y*0.7,size=xG,colour=xG)) +
  geom_point() +
  scale_colour_gradient2(low = "gray", high = "red",mid = "yellow", midpoint = 0.2) +
  scale_size_area(max_size=10)+
  labs(y = "Esto es la coordenada Y del campo",
       x = "Esto es la coordenada X del campo",
       title = "Zonas de disparo del Real Madrid",
       subtitle = "Temporadas de 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))



