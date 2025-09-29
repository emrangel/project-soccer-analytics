library(ggplot2)
library(tidyverse)
mainDir <- "C:/Users/jlagosmi/Desktop/MASTER BIG DATA/PAQUETE_R/UnderStatPlots/data/Understat"
setwd(file.path(mainDir))
##Mateo setwd

setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")
US_season_Total<-readRDS("US_season_Total.RData")

Ejercicio<-US_season_Total %>%
  group_by(league_name,year) %>%
  summarise(xG=mean(xG))

# Ejercicio 1.1

ggplot(data=Ejercicio,aes(x=year,y=xG))

# Ejercicio 1.2

ggplot(Ejercicio,aes(year,xG)) +
  geom_point()

# Ejercicio 1.3

ggplot(Ejercicio,aes(x=year,y=xG,color=league_name,size=xG,shape=league_name)) +
  geom_point()


# Ejercicio 1.4

ggplot(Ejercicio,aes(year,xG,color=league_name)) +
  geom_point(size=10,alpha=0.5)


# Ejercicio 1.5

cols <- c("Bundesliga" = "red", "EPL" = "blue",
          "La_liga" = "darkgreen", "Ligue_1" = "orange",
          "RFPL" = "gray","Serie_A" = "black")

ggplot(Ejercicio,aes(year,xG,color=league_name)) +
  geom_point(size=5,alpha=0.5)+
  scale_colour_manual(values = cols)


# Ejercicio 1.6

cols <- c("Bundesliga" = "red", "EPL" = "blue",
          "La_liga" = "darkgreen", "Ligue_1" = "orange",
          "RFPL" = "gray","Serie_A" = "black")

ggplot(Ejercicio,aes(year,xG,color=league_name)) +
  geom_point(size=10,alpha=0.5)+
  scale_colour_manual(values = cols) +
  stat_smooth( se = FALSE)



# Ejercicio 1.7
#coord_flip organiza las X en Y
#face_wrap crea un gráfico por cada dato o variable

cols <- c("Bundesliga" = "red", "EPL" = "blue",
          "La_liga" = "darkgreen", "Ligue_1" = "orange",
          "RFPL" = "gray","Serie_A" = "black")

ggplot(Ejercicio,aes(year,xG,color=league_name)) +
  geom_point(size=10,alpha=0.5)+
  scale_colour_manual(values = cols) +
  stat_smooth( se = FALSE) +
  coord_flip() +
  facet_wrap(~league_name)


# Ejercicio 1.8
cols <- c("Bundesliga" = "red", "EPL" = "blue",
          "La_liga" = "darkgreen", "Ligue_1" = "orange",
          "RFPL" = "gray","Serie_A" = "black")

promedio_total<-Ejercicio %>%
  group_by(year) %>%
  summarise(xG=mean(xG))

ggplot(Ejercicio,aes(year,xG,color=league_name)) +
  geom_point(size=10,alpha=0.5)+
  scale_colour_manual(values = cols) +
  stat_smooth( se = FALSE) +
  coord_flip() +
  facet_wrap(~league_name) +
  theme_bw()


# Ejercicio 1.9
cols <- c("Bundesliga" = "red", "EPL" = "blue",
          "La_liga" = "darkgreen", "Ligue_1" = "orange",
          "RFPL" = "gray","Serie_A" = "black")

promedio_total<-Ejercicio %>%
  group_by(year) %>%
  summarise(xG=mean(xG))

ggplot(Ejercicio,aes(year,xG,color=league_name)) +
  geom_point(size=5,alpha=0.5)+
  scale_colour_manual(values = cols) +
  scale_x_continuous(breaks = 2014:2020) +
  stat_smooth( se = FALSE) +
  coord_flip() +
  facet_wrap(~league_name) +
  geom_line(data = promedio_total, aes(year,xG),size=1,linetype="dashed",color = 'black') +
  theme_bw() +
  labs(x = "Temporada", y = "Goles Esperados (xG)",
       title = "Evolución  promedio de xG por liga y temporada",
       subtitle = "Temporadas desde 2014 a 2020 de las principales ligas europeas",
       caption = "Fuente: Understat  Elaborado por: Erick Rangel @mrangel",
       color = "Ligas:") +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        legend.title = element_text(color = "black",
                                    size = 14, face = "bold"),
        panel.grid.minor =element_line(linetype = c("28")),
        strip.background = element_rect(color = "black", fill = "#FC4E07", size = 1.5, linetype = "solid"),
        strip.placement = "inside",
        strip.text.y = element_text(size = 12, color = "white", face = "bold.italic"),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold.italic",hjust = 0),
        legend.position = "null")

#strip.background = element_blanck

# Ejercicio 1.10
cols <- c("Bundesliga" = "red", "EPL" = "blue",
          "La_liga" = "darkgreen", "Ligue_1" = "orange",
          "RFPL" = "gray","Serie_A" = "black")

promedio_total<-Ejercicio %>%
  group_by(year) %>%
  summarise(xG=mean(xG))

ggplot(US_season_Total,aes(x=as.factor(league_name),y=xG)) +
  geom_boxplot(aes(fill=league_name))+
  scale_colour_manual(values = cols) +
  facet_wrap(~year) +
  coord_flip() +
  theme_bw() +
  labs(x = "Temporada", y = "Goles Esperados (xG)",
       title = "Evolución del romedio de xG por liga y temporada",
       subtitle = "Temporadas desde 2014 a 2020 de las principales ligas europeas",
       caption = "Fuente: Understat  Elaborado por: Jes?s Lagos @vdot_spain",
       color = "Ligas:") +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        legend.title = element_text(color = "black",
                                    size = 14, face = "bold"),
        panel.grid.minor =element_line(linetype = c("28")),
        strip.background = element_blank(),
        strip.placement = "inside",
        strip.text.y = element_text(size = 12, color = "white", face = "bold.italic"),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold.italic",hjust = 0),
        legend.position = "null")

