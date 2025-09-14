library(ggplot2)
library(dplyr)

library(gganimate)
library(ggrepel)
library(ggimage)
library(readxl)
library(ggalt)
library(gridExtra)


#Xga y Goles recibidos
##Betplay 2 Goles en Contra

setwd("C:/Users/Mateo/Desktop/Main/Rstudio/CursoFutbol/R Futbol/Modulo 2")

xgbet <- read_excel("df.xlsx",3)
xgbetcontra <- read_excel("df.xlsx",4)

Teams <- read_excel("Teams.xlsx")

equipos<- xgbet %>%
  inner_join(xgbetcontra, by=c("Equipo"="Equipo"))

equipos<-equipos %>%
  inner_join(Teams, by=c("Equipo"="Understat")) %>%
  mutate(liga="CO_")

colnames(equipos)[2]="xG"

#df$badge <- paste(system.file(package="FootballBadges"),"/Badges/",df$Teams ,".png",sep="")
equipos$badge <- paste("./",equipos$liga,equipos$Code,".png",sep="")

equipos <- equipos %>%
  group_by(badge) %>%
  summarise(xG=sum(xG),xGA=sum(xGA), Goles=sum(Goles), GolesR=sum(GolesR)) %>%
  ungroup()


Graf=ggplot(equipos, aes(x = xGA, y = GolesR), col.axis = "red") +
  ggtitle(label = "Análisis de xGA" ,subtitle = "Comparativa xGA y Goles Recibidos") +
  geom_image(aes(image = badge), size = 0.04,) +

  geom_segment(data = equipos,aes(x = median(xGA), y = min(GolesR), xend = median(xGA), yend = max(GolesR)),
               linetype="dashed",colour = "white",alpha=0.5) +
  geom_segment(data = equipos,aes(x = min(xGA), y = median(GolesR), xend = max(xGA), yend = median(GolesR)),
               linetype="dashed", colour = "white",alpha=0.5) +

  annotate(geom="text", x=max(equipos$xGA), y=(max(equipos$GolesR)-median(equipos$GolesR))/8+median(equipos$GolesR),
           label=paste("Median Goles =",format(round(median(equipos$GolesR), 2), nsmall = 2)),
           color="white",size=3) +
  annotate(geom="text", x=(max(equipos$xGA)-median(equipos$xGA))/8+median(equipos$xGA), y=max(equipos$Goles),
           label=paste("Median xGA =",format(round(median(equipos$xGA), 2), nsmall = 2)),
           color="white",size=3) +

  xlab("xGA totales") +
  ylab("Goles Recibidos") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5, size=14, colour = "white"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size=10,colour = "white"),
        axis.text.x = element_text( colour="white"),
        axis.text.y = element_text(colour="white"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        panel.background = element_rect("#182849"),
        plot.background = element_rect(fill = "#182849"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "white"))
Graf
