###Library
#library(magick)
#library(hrbrthemes)
#library(here)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggimage)



##Saving 12.4 x 8.2 in image##
help("geom_image")

##Leer logo

logo <- image_read("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2/conm.jpg")

##Gráfico xG y xGA mean

{
  setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")

  xgbet <- read_excel("df1.xlsx",3)
  xgbetcontra <- read_excel("df1.xlsx",4)

  Teams <- read_excel("Teams.xlsx")


  equipos<- xgbet %>%
    inner_join(xgbetcontra, by=c("Equipo"="Equipo"))

  equipos<-equipos %>%
    inner_join(Teams, by=c("Equipo"="Understat")) %>%
    mutate(liga="CON_")

  colnames(equipos)[2]="xG"


  #df$badge <- paste(system.file(package="FootballBadges"),"/Badges/",df$Teams ,".png",sep="")
  equipos$badge <- paste("./",equipos$liga,equipos$Code,".png",sep="")

  equipos <- equipos %>%
    group_by(badge) %>%
    summarise(xG=sum(xG),xGA=sum(xGA), Goles=sum(Goles), GolesR=sum(GolesR),
              xGP=sum(xGP),xGAP=sum(xGAP), GolesP=sum(GolesP), GolesRP=sum(GolesRP)) %>%
    ungroup()



  Graf<-ggplot(equipos, aes(x = xGP, y = xGAP), col.axis = "red") +
    ggtitle(label = "Análisis xG vs xGA" ,subtitle = "Comparativa Promedio por Partido") +
    geom_image(aes(image = badge), size = 0.05,) +
    #geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed")+


    geom_vline(aes(xintercept = mean(xGP)), color = "gray", linetype = "dashed") +
    geom_hline(aes(yintercept = mean(xGAP)), color = "gray", linetype = "dashed") +
    labs(y = "xGA",
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

  plot(Graf)

}

#grid::grid.raster(logo, x = 0.85, y = 0.8, just = c('left', 'bottom'), width = unit(1.5, 'inches')) %>%

 #dev.off()

#image_write(Graf1,"Graf1.png")

ggsave("CON1.png", width = 12.4, height = 8.2)


###xG y Goles

{setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")


  xgbet <- read_excel("df1.xlsx",3)
  xgbetcontra <- read_excel("df1.xlsx",4)

  Teams <- read_excel("Teams.xlsx")


  equipos<- xgbet %>%
    inner_join(xgbetcontra, by=c("Equipo"="Equipo"))

  equipos<-equipos %>%
    inner_join(Teams, by=c("Equipo"="Understat")) %>%
    mutate(liga="CON_")

  colnames(equipos)[2]="xG"


  #df$badge <- paste(system.file(package="FootballBadges"),"/Badges/",df$Teams ,".png",sep="")
  equipos$badge <- paste("./",equipos$liga,equipos$Code,".png",sep="")

  equipos <- equipos %>%
    group_by(badge) %>%
    summarise(xG=sum(xG),xGA=sum(xGA), Goles=sum(Goles), GolesR=sum(GolesR),
              xGP=sum(xGP),xGAP=sum(xGAP), GolesP=sum(GolesP), GolesRP=sum(GolesRP)) %>%
    ungroup()



Graf2=ggplot(equipos, aes(x = xGP, y = GolesP), col.axis = "red") +
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
}

ggsave("CON2.png", width = 12.4, height = 8.2)


##Gráfico xGA y GolesR


{setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")

#my.theme = list(theme(axis.line.y.left = element_line(size = 3, color = "white"),
                     # axis.line.y.right = element_line(size = 3, color = "white"),
                     # axis.line.x.top =  element_line(size = 3, color = "white"),
                     # axis.line.x.bottom = element_line(size = 3, color = "white")))

xgbet <- read_excel("df1.xlsx",3)
xgbetcontra <- read_excel("df1.xlsx",4)

Teams <- read_excel("Teams.xlsx")


equipos<- xgbet %>%
  inner_join(xgbetcontra, by=c("Equipo"="Equipo"))

equipos<-equipos %>%
  inner_join(Teams, by=c("Equipo"="Understat")) %>%
  mutate(liga="CON_")

colnames(equipos)[2]="xG"


#df$badge <- paste(system.file(package="FootballBadges"),"/Badges/",df$Teams ,".png",sep="")
equipos$badge <- paste("./",equipos$liga,equipos$Code,".png",sep="")

equipos <- equipos %>%
  group_by(badge) %>%
  summarise(xG=sum(xG),xGA=sum(xGA), Goles=sum(Goles), GolesR=sum(GolesR),
            xGP=sum(xGP),xGAP=sum(xGAP), GolesP=sum(GolesP), GolesRP=sum(GolesRP)) %>%
  ungroup()



Graf1=ggplot(equipos, aes(x = xGAP, y = GolesRP), col.axis = "red") +
  ggtitle(label = "Análisis xGA vs Goles Recibidos" ,subtitle = "Comparativa Promedio por Partido") +
  geom_image(aes(image = badge), size = 0.04,) +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed")+


  ##geom_vline(aes(xintercept = mean(xGA)), color = "red", linetype = "dashed") +
  ##geom_hline(aes(yintercept = mean(GolesR)), color = "red", linetype = "dashed") +
  labs(y = "Goles Recibidos",
       x = "xGA",
       caption = "Fuente: Opta, @emrangel\n Descarga Baloa & Onefootball")+
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

ggsave("CON3.png", width = 12.4, height = 8.2)



