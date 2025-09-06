library(ggplot2)
library(dplyr)

mainDir <- "C:/Users/jlagosmi/Desktop/MASTER BIG DATA/PAQUETE_R/UnderStatPlots/data/Understat"
setwd(file.path(mainDir))

US_season_Total<-readRDS("US_season_Total.RData")
US_shots_Total<-readRDS("US_shots_Total.RData")


# Ejercicio 5.1

Ejercicio<-US_shots_Total %>%
  filter((h_a=='h' & h_team=="Real Madrid" | h_a=='a' & a_team=='Real Madrid'),result!='OwnGoal')

ggplot(Ejercicio,aes(X,Y*0.7)) +
  geom_hex(bins=10) +
  scale_fill_gradientn(colours = rainbow(5)) +
  coord_flip() +
  xlim(0.6,1) +
  facet_grid(.~year)+
  labs(y = "Esto es la coordenada Y del campo",
       x = "Esto es la coordenada X del campo",
       title = "Zonas de disparo del Real Madrid",
       subtitle = "Temporadas de 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))



# Ejercicio 5.2

Ejercicio<-US_shots_Total %>%
  filter((h_a=='h' & h_team=="Real Madrid" | h_a=='a' & a_team=='Real Madrid'),result!='OwnGoal')

ggplot(Ejercicio,aes(X,Y*0.7)) +
  geom_hex(bins=10) +
  scale_fill_gradientn(colours = rainbow(5)) +
  coord_flip() +
  xlim(0.6,1) +
  facet_grid(year~.)+
  labs(y = "Esto es la coordenada Y del campo",
       x = "Esto es la coordenada X del campo",
       title = "Zonas de disparo del Real Madrid",
       subtitle = "Temporadas de 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))



# Ejercicio 5.3

Ejercicio<-US_shots_Total %>%
  filter((h_a=='h' & h_team=="Real Madrid" | h_a=='a' & a_team=='Real Madrid') |
           (h_a=='h' & h_team=="Barcelona" | h_a=='a' & a_team=='Barcelona'),result!='OwnGoal') %>%
  mutate(team_name=case_when(
    h_a=='h' ~ h_team,
    h_a=='a' ~ a_team
  ))

ggplot(Ejercicio,aes(X,Y*0.7)) +
  geom_hex(bins=10) +
  scale_fill_gradientn(colours = rainbow(5)) +
  coord_flip() +
  xlim(0.6,1) +
  facet_grid(team_name~year)+
  labs(y = "Esto es la coordenada Y del campo",
       x = "Esto es la coordenada X del campo",
       title = "Zonas de disparo del Real Madrid y Barcelona",
       subtitle = "Temporadas de 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))





# Ejercicio 5.4

Ejercicio<-US_shots_Total %>%
  filter((h_a=='h' & h_team=="Real Madrid" | h_a=='a' & a_team=='Real Madrid') |
           (h_a=='h' & h_team=="Barcelona" | h_a=='a' & a_team=='Barcelona'),result!='OwnGoal') %>%
  mutate(team_name=case_when(
    h_a=='h' ~ h_team,
    h_a=='a' ~ a_team
  ))

ggplot(Ejercicio,aes(X,Y*0.7)) +
  geom_hex(bins=10) +
  scale_fill_gradientn(colours = rainbow(5)) +
  coord_flip() +
  xlim(0.6,1) +
  facet_wrap(.~year)+
  labs(y = "Esto es la coordenada Y del campo",
       x = "Esto es la coordenada X del campo",
       title = "Zonas de disparo del Real Madrid y Barcelona",
       subtitle = "Temporadas de 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))
