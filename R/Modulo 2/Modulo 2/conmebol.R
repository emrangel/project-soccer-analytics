library(readxl)
library(ggplot2)
library(dplyr)
library(ggimage)


setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")


equipos<- read_excel("df1.xlsx")

Teams <- read_excel("Teams.xlsx")

equipos<-equipos %>%
  inner_join(Teams, by=c("Equipo"="Understat")) %>%
  mutate(liga="CO_")


#df$badge <- paste(system.file(package="FootballBadges"),"/Badges/",df$Teams ,".png",sep="")
equipos$badge <- paste("./",equipos$liga,equipos$Code,".png",sep="")

equipos <- equipos %>%
  group_by(badge, Equipo) %>%
  summarise_all(list(mean))  %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  ungroup()



#df$badge <- paste(system.file(package="FootballBadges"),"/Badges/",df$Teams ,".png",sep="")

Graf2=ggplot(equipos, aes(x =xG, y =Goles), col.axis = "red") +
  ggtitle(label = "Análisis xG vs Goles" ,subtitle = "Comparativa Promedio por Partido") +
  geom_image(aes(image = badge), size = 0.04,) +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed")+


  ##geom_vline(aes(xintercept = mean(xGA)), color = "red", linetype = "dashed") +
  ##geom_hline(aes(yintercept = mean(GolesR)), color = "red", linetype = "dashed") +
  labs(y = "Goles",
       x = "xG",
       caption = "Fuente: Opta, @emrangel.\nDescarga Baloa & Onefootball")+
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5, size=14, colour = "white"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size=10,colour = "white"),
        plot.caption = element_text(hjust = 0.5,
                                    size=10,colour = "white"),
        axis.text.x = element_text( colour="white"),
        axis.text.y = element_text(colour="white"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        panel.background = element_rect("#182849"),
        plot.background = element_rect(fill = "#182849"),
        panel.border = element_rect(fill = NA, color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot(Graf2)


