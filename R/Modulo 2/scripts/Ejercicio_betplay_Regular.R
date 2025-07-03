# Librerías ----

library(ggplot2)
library(ggimage)
library(readxl)
library(gganimate)
library(dplyr)

.folder_data_input = '/R/Modulo 2/data/input/'
.folder_data_out = '/R/Modulo 2/data/out/'
.folder_img = '/R/Modulo 2/imng/'

getwd()
#Primer intento
{
setwd("C:/Users/Mateo/Desktop/Main/Rstudio/CursoFutbol/R Futbol/Modulo 2")
Golbet <- read_excel("Ranking equipos.xlsx",2, )
str((Golbet))
xgbet <- read_excel("Ranking equipos.xlsx",3)
str(xgbet)
Golbetcontra <- read_excel("Ranking equipos.xlsx",4)
xgbetcontra <- read_excel("Ranking equipos.xlsx",5)


Teams <- read_excel("Teams.xlsx")


xgbet<- xgbet[complete.cases(xgbet),]
xgbetcontra<- xgbetcontra[complete.cases(xgbetcontra),]

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
  summarise(xG=sum(xG),xGA=sum(xGA)) %>%
  ungroup()



Graf=ggplot(equipos, aes(x = xG, y = xGA), col.axis = "red") +
  ggtitle(label = "Análisis de xG" ,subtitle = "Comparativa xG y xGA") +
  geom_image(aes(image = badge), size = 0.04,) +

  geom_segment(data = equipos,aes(x = median(xG), y = min(xGA), xend = median(xG), yend = max(xGA)),
               linetype="dashed",colour = "white",alpha=0.5) +
  geom_segment(data = equipos,aes(x = min(xG), y = median(xGA), xend = max(xG), yend = median(xGA)),
               linetype="dashed", colour = "white",alpha=0.5) +

  annotate(geom="text", x=max(equipos$xG), y=(max(equipos$xGA)-median(equipos$xGA))/8+median(equipos$xGA),
           label=paste("Median xGA =",format(round(median(equipos$xGA), 2), nsmall = 2)),
           color="white",size=3) +
  annotate(geom="text", x=(max(equipos$xG)-median(equipos$xG))/8+median(equipos$xG), y=max(equipos$xGA),
           label=paste("Median xG =",format(round(median(equipos$xG), 2), nsmall = 2)),
           color="white",size=3) +

  xlab("xG totales") +
  ylab("xGA totales") +
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
ggsave("equipos.png")

}

##Saving 12.4 x 8.2 in image##
help("geom_image")

#Ocultar
{
##Betplay 2


setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")

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



Graf=ggplot(equipos, aes(x = xG, y = Goles), col.axis = "red") +
  ggtitle(label = "Análisis de xG" ,subtitle = "Comparativa xG y xGA") +
  geom_image(aes(image = badge), size = 0.04,) +

  geom_segment(data = equipos,aes(x = median(xG), y = min(Goles), xend = median(xG), yend = max(Goles)),
               linetype="dashed",colour = "white",alpha=0.5) +
  geom_segment(data = equipos,aes(x = min(xG), y = median(Goles), xend = max(xG), yend = median(Goles)),
               linetype="dashed", colour = "white",alpha=0.5) +

  annotate(geom="text", x=max(equipos$xG), y=(max(equipos$Goles)-median(equipos$Goles))/8+median(equipos$Goles),
           label=paste("Median Goles =",format(round(median(equipos$Goles), 2), nsmall = 2)),
           color="white",size=3) +
  annotate(geom="text", x=(max(equipos$xG)-median(equipos$xG))/8+median(equipos$xG), y=max(equipos$Goles),
           label=paste("Median xG =",format(round(median(equipos$xG), 2), nsmall = 2)),
           color="white",size=3) +

  xlab("xG totales") +
  ylab("Goles") +
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
}


##Gráfico xG y xGA mean
{
  setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")


  equipos<- read_excel("df1_R.xlsx")

  Teams <- read_excel("Teams.xlsx")

  equipos<-equipos %>%
    inner_join(Teams, by=c("Equipo"="Understat")) %>%
    mutate(liga="CO_")


  #df$badge <- paste(system.file(package="FootballBadges"),"/Badges/",df$Teams ,".png",sep="")
  equipos$badge <- paste("./",equipos$liga,equipos$Code,".png",sep="")

  equipos <- equipos %>%
    group_by(badge) %>%
    summarise(xG=sum(xG),xGA=sum(xGA), Goles=sum(Goles), GolesR=sum(GolesR)) %>%
    ungroup()

  Graf=ggplot(equipos, aes(x = xG, y = xGA), col.axis = "red") +
    ggtitle(label = "Análisis de xG vs xGA" ,subtitle = "Sumatoria  Jornada 20 xG y xGA") +
    geom_image(aes(image = badge), size = 0.04,) +
    #geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed")+


    geom_vline(aes(xintercept = mean(xGA)), color = "gray", linetype = "dashed") +
    geom_hline(aes(yintercept = mean(GolesR)), color = "gray", linetype = "dashed") +
    labs(y = "xGA",
         x = "xG",
         caption = "Fuente: Opta  Elaborado por: Erick Rangel")+
    theme(legend.position="none") +
    theme(plot.title = element_text(hjust = 0.5, size=24, colour = "white"),
          plot.subtitle = element_text(hjust = 0.5,
                                       size=15,colour = "white"),
          plot.caption = element_text(hjust = 0.5,
                                      size=15,colour = "white"),
          axis.text.x = element_text( colour="white"),
          axis.text.y = element_text(colour="white"),
          axis.title.x = element_text(colour = "white"),
          axis.title.y = element_text(colour = "white"),
          panel.background = element_rect("#182849"),
          plot.background = element_rect(fill = "#182849"),
          panel.border = element_rect(fill = NA, color = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  plot(Graf)
}


ggsave("betR.png", height = 14.5, width = 14.5)

#Xga y Goles recibidos Median CAMBIOS

{
setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")

  equipos<- read_excel("df1R.xlsx")

  Teams <- read_excel("Teams.xlsx")


equipos<-equipos %>%
  inner_join(Teams, by=c("Equipo"="Understat")) %>%
  mutate(liga="CO_")

colnames(equipos)[2]="xG"


#df$badge <- paste(system.file(package="FootballBadges"),"/Badges/",df$Teams ,".png",sep="")
equipos$badge <- paste("./",equipos$liga,equipos$Code,".png",sep="")


Graf4=ggplot(equipos, aes(x = xGA, y = GolesR), col.axis = "red") +
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
plot(Graf4)

}


###xG y Goles lineal

{setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")

  equipos<- read_excel("df1_R.xlsx")

  Teams <- read_excel("Teams.xlsx")


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



Graf2=ggplot(equipos, aes(x = xG, y = Goles), col.axis = "red") +
    ggtitle(label = "Análisis de xG" ,subtitle = "Comparativa xG y Goles") +
    geom_image(aes(image = badge), size = 0.04,) +
    geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed")+


    ##geom_vline(aes(xintercept = mean(xGA)), color = "red", linetype = "dashed") +
    ##geom_hline(aes(yintercept = mean(GolesR)), color = "red", linetype = "dashed") +
    labs(y = "Goles",
         x = "xG",
         caption = "Fuente: Opta  Elaborado por: Erick Rangel")+
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
}

ggsave("betR2.png", height = 10.5, width = 10.5)


##Gráfico xGA y GolesR


{setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")

#my.theme = list(theme(axis.line.y.left = element_line(size = 3, color = "white"),
                     # axis.line.y.right = element_line(size = 3, color = "white"),
                     # axis.line.x.top =  element_line(size = 3, color = "white"),
                     # axis.line.x.bottom = element_line(size = 3, color = "white")))

  setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")


  equipos<- read_excel("df1_R.xlsx")

  Teams <- read_excel("Teams.xlsx")


equipos<-equipos %>%
  inner_join(Teams, by=c("Equipo"="Understat")) %>%
  mutate(liga="CO_")


#df$badge <- paste(system.file(package="FootballBadges"),"/Badges/",df$Teams ,".png",sep="")
equipos$badge <- paste("./",equipos$liga,equipos$Code,".png",sep="")


Graf1=ggplot(equipos, aes(x = xGA, y = GolesR), col.axis = "red") +
  ggtitle(label = "Análisis de xGA" ,subtitle = "Comparativa xGA y Goles Recibidos") +
  geom_image(aes(image = badge), size = 0.04,) +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed")+


  ##geom_vline(aes(xintercept = mean(xGA)), color = "red", linetype = "dashed") +
  ##geom_hline(aes(yintercept = mean(GolesR)), color = "red", linetype = "dashed") +
  labs(y = "Goles Recibidos",
       x = "xGA totales",
       caption = "Fuente: Opta  Elaborado por: Erick Rangel")+
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

plot(Graf1)

}

ggsave("betR3.png", height = 10.5, width = 10.5)



###ANIMATE SUM BETPLAY

{

  equipos<- read_excel(paste0(.folder_data_input,"df1R.xlsx"))


  Teams <- read_excel("Teams.xlsx")

equipos<-equipos[1:13]

equipos<-equipos %>%
    group_by(Equipo) %>%
    mutate(xGG=cumsum(xG), xGAA=cumsum(xGA))

equipos<-equipos %>%
  inner_join(Teams, by=c("Equipo"="Understat")) %>%
  mutate(liga="CO_")


  #df$badge <- paste(system.file(package="FootballBadges"),"/Badges/",df$Teams ,".png",sep="")
  equipos$badge <- paste("./",equipos$liga,equipos$Code,".png",sep="")


  Graf=ggplot(equipos, aes(x = xGG, y = xGAA), col.axis = "red") +
    ggtitle(label = "Análisis de xG vs xGA" ,subtitle = "Sumatoria xG y xGA") +
    geom_image(aes(image = badge), size = 0.04,) +
    #geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed")+


    #geom_vline(aes(xintercept = mean(xGAA)), color = "gray", linetype = "dashed") +
    #geom_hline(aes(yintercept = mean(xGG)), color = "gray", linetype = "dashed") +
    labs(y = "xGA",
         x = "xG",
         caption = "Fuente: Opta  Elaborado por: Erick Rangel")+
    theme(legend.position="none") +
    theme(plot.title = element_text(hjust = 0.5, size=14, colour = "white"),
          plot.subtitle = element_text(hjust = 0.5,
                                       size=10,colour = "white"),
          plot.caption = element_text(hjust = 0.5,
                                      size=10,colour = "white"),
          axis.text.x = element_text( colour="white", size=10),
          axis.text.y = element_text(colour="white",size=10),
          axis.title.x = element_text(colour = "white"),
          axis.title.y = element_text(colour = "white"),
          panel.background = element_rect("#182849"),
          plot.background = element_rect(fill = "#182849"),
          panel.border = element_rect(fill = NA, color = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  plot(Graf)
}


anim <- Graf +
  transition_states(Jornada)+
  labs(title= "Jornada {closest_state} Liga Betplay")+
  theme(plot.title = element_text(hjust = 0.5, size=20, colour = "white"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size=15,colour = "white"),
        plot.caption = element_text(hjust = 0.5,
                                    size=15,colour = "white"))
anim

animate(anim, nframes =150 ,
        renderer = gifski_renderer("bet_ReguSum.gif"),
        heigh=900, width=900)

##ANIMATE BET JORNADAxJORNADA

{
  setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")

  equipos<- read_excel("df1R.xlsx")

  Teams <- read_excel("Teams.xlsx")

  equipos<-equipos %>%
    inner_join(Teams, by=c("Equipo"="Understat")) %>%
    mutate(liga="CO_")


  #df$badge <- paste(system.file(package="FootballBadges"),"/Badges/",df$Teams ,".png",sep="")
  equipos$badge <- paste("./",equipos$liga,equipos$Code,".png",sep="")


  bet=ggplot(equipos, aes(x = xG, y = xGA), col.axis = "red") +
    ggtitle(label = "Análisis de xG vs xGA" ,subtitle = "Jornadas xG y xGA") +
    geom_image(aes(image = badge), size = 0.04,) +
    #geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed")+


    geom_vline(aes(xintercept = mean(xGA)), color = "gray", linetype = "dashed") +
    geom_hline(aes(yintercept = mean(GolesR)), color = "gray", linetype = "dashed") +
    labs(y = "xGA",
         x = "xG",
         caption = "Fuente: Opta  Elaborado por: Erick Rangel")+
    theme(legend.position="none") +
    theme(plot.title = element_text(hjust = 0.5, size=14, colour = "white"),
          plot.subtitle = element_text(hjust = 0.5,
                                       size=10,colour = "white"),
          plot.caption = element_text(hjust = 0.5,
                                      size=10,colour = "white"),
          axis.text.x = element_text( colour="white", size=10),
          axis.text.y = element_text(colour="white", size=10),
          axis.title.x = element_text(colour = "white"),
          axis.title.y = element_text(colour = "white"),
          panel.background = element_rect("#182849"),
          plot.background = element_rect(fill = "#182849"),
          panel.border = element_rect(fill = NA, color = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  plot(bet)
}

anim <- bet +
  transition_states(Jornada)+
  labs(x="xG",
       y="xGA",
       title= "Jornada {closest_state} Liga Betplay")+
  theme(plot.title = element_text(hjust = 0.5, size=20, colour = "white"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size=15,colour = "white"),
        plot.caption = element_text(hjust = 0.5,
                                    size=15,colour = "white"))

anim

animate(anim, nframes =260 ,
        renderer = gifski_renderer("bet_RegJor.gif"),
        heigh=900, width=900)



#Bet Regular

##Gráfico xG y xGA mean
{
  setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")


  equipos<- read_excel("df1R.xlsx")

  Teams <- read_excel("Teams.xlsx")




  equipos<-equipos %>%
    inner_join(Teams, by=c("Equipo"="Understat")) %>%
    mutate(liga="CO_")



  #df$badge <- paste(system.file(package="FootballBadges"),"/Badges/",df$Teams ,".png",sep="")
  equipos$badge <- paste("./",equipos$liga,equipos$Code,".png",sep="")



  Graf=ggplot(equipos, aes(x = xG, y = xGA), col.axis = "red") +
    ggtitle(label = "Análisis de xG vs xGA" ,subtitle = "Comparativa xG y xGA") +
    geom_image(aes(image = badge), size = 0.04) +
    #geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed")+


    geom_vline(aes(xintercept = mean(xGA)), color = "gray", linetype = "dashed") +
    geom_hline(aes(yintercept = mean(GolesR)), color = "gray", linetype = "dashed") +
    labs(y = "xGA",
         x = "xG",
         caption = "Fuente: Wyscout  Elaborado por: Erick Rangel")+
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

  plot(Graf)
}


##intento

{
  setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")

  equipos<- read_excel("df.xlsx")

  Teams <- read_excel("Teams.xlsx")


  equipos<- xgbet %>%
    inner_join(xgbetcontra, by=c("Equipo"="Equipo"))

  equipos<-equipos %>%
    inner_join(Teams, by=c("Equipo"="Understat")) %>%
    mutate(liga="CO_")


  #df$badge <- paste(system.file(package="FootballBadges"),"/Badges/",df$Teams ,".png",sep="")
  equipos$badge <- paste("./",equipos$liga,equipos$Code,".png",sep="")


  bet=ggplot(equipos, aes(x = xG, y = xGA), col.axis = "red") +
    ggtitle(label = "Análisis de xG vs xGA" ,subtitle = "Comparativa xG y xGA") +
    geom_image(aes(image = badge), size = 0.04,) +
    #geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed")+


    geom_vline(aes(xintercept = mean(xGA)), color = "gray", linetype = "dashed") +
    geom_hline(aes(yintercept = mean(GolesR)), color = "gray", linetype = "dashed") +
    labs(y = "xGA",
         x = "xG",
         caption = "Fuente: Wyscout  Elaborado por: Erick Rangel")+
    theme(legend.position="none") +
    theme(plot.title = element_text(hjust = 0.5, size=14, colour = "white"),
          plot.subtitle = element_text(hjust = 0.5,
                                       size=10,colour = "white"),
          plot.caption = element_text(hjust = 0.5,
                                      size=10,colour = "white"),
          axis.text.x = element_text( colour="white", size=10),
          axis.text.y = element_text(colour="white", size=10),
          axis.title.x = element_text(colour = "white"),
          axis.title.y = element_text(colour = "white"),
          panel.background = element_rect("#182849"),
          plot.background = element_rect(fill = "#182849"),
          panel.border = element_rect(fill = NA, color = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  plot(bet)
}

anim <- bet +
  transition_states(Jornada)+
  labs(x="xG",
       y="xGA",
       title= "Jornada {closest_state} Liga Betplay")+
  theme(plot.title = element_text(hjust = 0.5, size=20, colour = "white"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size=15,colour = "white"),
        plot.caption = element_text(hjust = 0.5,
                                    size=15,colour = "white"))

anim

##Intento editR


library(DataEditR)

data_edit(df)

