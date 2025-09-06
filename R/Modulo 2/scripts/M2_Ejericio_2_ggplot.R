library(ggplot2)
library(dplyr)

mainDir <- "C:/Users/jlagosmi/Desktop/MASTER BIG DATA/PAQUETE_R/UnderStatPlots/data/Understat"
setwd(file.path(mainDir))

##Mateo setwd
setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")

US_season_Total<-readRDS("US_season_Total.RData")
US_shots_Total<-readRDS("US_shots_Total.RData")

# Ejercicio 2.1

Ejercicio<-US_season_Total %>%
  filter(team_name=="Real Madrid") %>%
  group_by(year) %>%
  summarise(xG=mean(xG))

ggplot(Ejercicio,aes(x=year,y=xG)) +
  geom_area() +
  scale_x_continuous(breaks = 2014:2020) +
  labs(x = "Temporada", y = "Goles Esperados (xG)",
       title = "Evoluciónn del promedio de xG del Real Madrid por temporada",
       subtitle = "Temporadas desde 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Emrangel") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))


#geom_line()

# Ejercicio 2.2

Ejercicio<-US_season_Total %>%
  filter(team_name=="Real Madrid")

ggplot(Ejercicio,aes(xG)) +
  geom_density() +
  labs(y = "Densidad",
       title = "Distribuci?n de denisdad de xG del Real Madrid por partido",
       subtitle = "Temporadas desde 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jes?s Lagos") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))



# Ejercicio 2.2bis

Ejercicio<-US_season_Total %>%
  filter(team_name %in% c("Real Madrid","Barcelona","Valencia"))

ggplot(Ejercicio,aes(xG,fill=team_name,colour=team_name)) +
  geom_density(alpha = 0.1) +
  labs(y = "Densidad",
       title = "Distribución de denisdad de xG por partido",
       subtitle = "Temporadas desde 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Erick R",
       fill = "Equipos:",
       colour = "Equipos:") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))




# Ejercicio 2.3

Ejercicio<-US_season_Total %>%
  filter(team_name=="Real Madrid")

ggplot(Ejercicio,aes(x=xG)) +
  geom_histogram(colour="white", fill="black") +
  labs(y = "N?mero de partdos",
       title = "Histograma de xG del Real Madrid por partido",
       subtitle = "Temporadas desde 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Erick R") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))


# Ejercicio 2.3bis

Ejercicio<-US_season_Total %>%
  filter(team_name %in% c("Real Madrid","Barcelona","Valencia"))

ggplot(Ejercicio,aes(xG,fill=team_name,colour=team_name)) +
  geom_histogram(position="identity",alpha=0.3) +
  labs(y = "Número de partdos",
       title = "Histograma de xG por partido",
       subtitle = "Temporadas desde 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Erick R",
       fill = "Equipos:",
       colour = "Equipos:") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")),
                legend.position = "bottom")

#legend.position borrar / por defecto aparece a la derecha

# Ejercicio 2.4

Ejercicio<-US_season_Total %>%
  filter(team_name=="Real Madrid",xG<1)

ggplot(Ejercicio,aes(year)) +
  geom_bar(fill="red") +
  scale_x_continuous(breaks = 2014:2020) +
  labs(y = "Numero de partidos",
       x= "Temporada",
       title = "N?mero de partidos con xG<1 del Real Madrid",
       subtitle = "Temporadas desde 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Erick R"
       ) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))


# Ejercicio 2.4bis

Ejercicio<-US_season_Total %>%
  filter(team_name %in% c("Real Madrid","Barcelona","Valencia"),xG<1)

ggplot(Ejercicio,aes(year,colour=team_name,fill=team_name)) +
  geom_bar() +
  scale_x_continuous(breaks = 2014:2020) +
  labs(y = "Numero de partidos",
       x= "Temporada",
       title = "Número de partidos con xG<1",
       subtitle = "Temporadas desde 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jes?s Lagos",
       fill = "Equipos:",
       colour = "Equipos:") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))

############################ importante
# Ejercicio 2.5

createPitch_StatsBomb <- function(xmax=120, ymax=80, grass_colour="gray", line_colour="white", background_colour="gray", goal_colour="white", data=NULL, halfPitch=FALSE){

  theme_blankPitch = function(size=12) {
    theme(
      #axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      #axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"),
      #axis.ticks.margin=unit(0, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background=element_rect(fill=background_colour, colour=NA),
      legend.key=element_rect(colour=background_colour,fill=background_colour),
      legend.key.size=unit(1.2, "lines"),
      legend.text=element_text(size=size),
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background=element_rect(fill=background_colour,colour=background_colour),
      #       panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      #panel.spacing=element_blank(),
      plot.background=element_blank(),
      #plot.margin=unit(c(0, 0, 0, 0), "lines"),
      plot.title=element_text(size=size*1.2),
      strip.text.y=element_text(colour=background_colour,size=size,angle=270),
      strip.text.x=element_text(size=size*1))}

  ymin <- 0 # minimum width
  ymax <- 80 # maximum width
  xmin <- 60 # minimum length
  xmax <- 120 # maximum length

  # Defining features along the length
  boxEdgeOff <- 102
  sixYardOff <- 114
  penSpotOff <- 108
  halfwayline <- 60

  # Defining features along the width
  boxEdgeLeft <- 18
  boxEdgeRight <- 62
  sixYardLeft <- 30
  sixYardRight <- 50
  goalPostLeft <- 36
  goalPostRight <- 44
  CentreSpot <- 40

  # other dimensions
  centreCirle_d <- 20

  ## define the circle function
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  #### create leftD arc ####
  dArc <- circleFun(c((40),(penSpotOff)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  dArc <- dArc[which(dArc$y <= (boxEdgeOff)),]

  ## initiate the plot, set some boundries to the plot
  p <- ggplot(data) + xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
    # add the theme
    theme_blankPitch() +
    # add the base rectangle of the pitch
    geom_rect(aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill = grass_colour, colour = line_colour) +
    # add the 18 yard box offensive
    geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=boxEdgeOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
    # add the six yard box offensive
    geom_rect(aes(xmin=sixYardLeft, xmax=sixYardRight, ymin=sixYardOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
    # add the arc circle
    geom_path(data=dArc, aes(x=x,y=y), colour = line_colour) +
    # add penalty spot
    geom_point(aes(x = CentreSpot , y = penSpotOff), colour = line_colour) +
    # add the goal offensive
    geom_segment(aes(x = goalPostLeft, y = xmax, xend = goalPostRight, yend = xmax),colour = goal_colour, size = 2)

  return(p)

}

h<-createPitch_StatsBomb()

h

# Ejercicio 2.6

Ejercicio<-US_season_Total %>%
  filter(team_name %in% c("Real Madrid","Barcelona","Valencia"))

ggplot(Ejercicio,aes(scored,missed,fill=team_name,colour=team_name)) +
  geom_point(size=4) +
  labs(y = "Goles en contra",
       x = "Goles a favor",
       title = "Goles a favor y en contra por partido",
       subtitle = "Temporadas desde 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos",
       fill = "Equipos:",
       colour = "Equipos:") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))


ggplot(Ejercicio,aes(scored,missed,fill=team_name,colour=team_name)) +
  geom_jitter() +
  labs(y = "Goles en contra",
       x = "Goles a favor",
       title = "Goles a favor y en contra por partido",
       subtitle = "Temporadas desde 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos",
       fill = "Equipos:",
       colour = "Equipos:") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))


# Ejercicio 2.7

Ejercicio<-US_season_Total %>%
  filter(league_name=='La_liga',year=='2020') %>%
  group_by(team_name) %>%
  mutate(xG=sum(xG),xGA=sum(xGA))

ggplot(Ejercicio,aes(xG,xGA,fill=team_name,colour=team_name)) +
  geom_point(size=4) +
  labs(y = "xG en contra",
       x = "xG a favor",
       title = "xG a favor y en contra",
       subtitle = "Temporada 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos",
       fill = "Equipos:",
       colour = "Equipos:") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))


# Ejercicio 2.8

Ejercicio<-US_season_Total %>%
  filter(league_name=='La_liga',year=='2020') %>%
  group_by(team_name) %>%
  mutate(xG=sum(xG),xGA=sum(xGA))

ggplot(Ejercicio,aes(xG,xGA)) +
  geom_text(aes(label=team_name)) +
  labs(y = "xG en contra",
       x = "xG a favor",
       title = "xG a favor y en contra",
       subtitle = "Temporada 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))


# Ejercicio 2.9

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

# Ejercicio 2.10

Ejercicio<-US_shots_Total %>%
  filter((h_a=='h' & h_team=="Real Madrid" | h_a=='a' & a_team=='Real Madrid'),result!='OwnGoal')

ggplot(Ejercicio,aes(X,Y*0.7)) +
  geom_density_2d_filled(alpha = 0.5 )+
  labs(y = "Esto es la coordenada Y del campo",
       x = "Esto es la coordenada X del campo",
       title = "Zonas de disparo del Real Madrid",
       subtitle = "Temporadas de 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))



# Ejercicio 2.11

Ejercicio<-US_shots_Total %>%
  filter((h_a=='h' & h_team=="Real Madrid" | h_a=='a' & a_team=='Real Madrid'),result!='OwnGoal')

ggplot(Ejercicio,aes(X,Y*0.7)) +
  geom_hex() +
  labs(y = "Esto es la coordenada Y del campo",
       x = "Esto es la coordenada X del campo",
       title = "Zonas de disparo del Real Madrid",
       subtitle = "Temporadas de 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))




# Ejercicio 2.12

Ejercicio<-US_shots_Total %>%
  filter((h_a=='h' & h_team=="Real Madrid" | h_a=='a' & a_team=='Real Madrid'),result=='Goal') %>%
  group_by(minute) %>%
  summarise(disparos=n()) %>%
  mutate(total_disparos=cumsum(disparos))


ggplot(Ejercicio,aes(minute,total_disparos)) +
  geom_step() +
  labs(y = "Goles",
       x = "Minutos",
       title = "Goles por minuto del Real Madrid",
       subtitle = "Temporadas de 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))





# Ejercicio 2.13

Ejercicio<-US_season_Total %>%
  filter(team_name %in% c("Real Madrid","Barcelona","Valencia"))


ggplot(Ejercicio,aes(as.factor(year),xGA)) +
  geom_boxplot(aes(colour=team_name)) +
  labs(y = "xGA",
       x = "Temporada",
       title = "Distribución de xGA por temporada",
       subtitle = "Temporadas de 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos",
       colour = "Equipos:") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))



# Ejercicio 2.14

Ejercicio<-US_season_Total %>%
  filter(team_name %in% c("Real Madrid","Barcelona"))


ggplot(Ejercicio,aes(as.factor(year),xGA)) +
  geom_dotplot(binaxis = "y", stackdir = "center",aes(colour=team_name,fill=team_name),position = "dodge",binwidth = 0.1) +
  labs(y = "xGA",
       x = "Temporada",
       title = "Distribución de xGA por temporada",
       subtitle = "Temporadas de 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos",
       colour = "Equipos:",
       fill = "Equipos:") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))


# Ejercicio 2.15

Ejercicio<-US_season_Total %>%
  filter(team_name %in% c("Real Madrid","Barcelona"))


ggplot(Ejercicio,aes(as.factor(year),xGA,group=team_name)) +
  geom_jitter(aes(fill=team_name,colour=team_name)) +
  labs(y = "xGA",
       x = "Temporada",
       title = "Distribución de xGA por temporada",
       subtitle = "Temporadas de 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Jesús Lagos",
       colour = "Equipos:",
       fill = "Equipos:") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 4, hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = 4, hjust = 0.5),
        panel.grid.minor =element_line(linetype = c("28")))

create_StatsBomb_ShotMap <- function(grass_colour, line_colour, background_colour, goal_colour){

  theme_blankPitch = function(size=12) {
    theme(
      #axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      #axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"),
      #axis.ticks.margin=unit(0, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background=element_rect(fill=background_colour, colour=NA),
      legend.key=element_rect(colour=background_colour,fill=background_colour),
      legend.key.size=unit(1.2, "lines"),
      legend.text=element_text(size=size),
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background=element_rect(fill=background_colour,colour=background_colour),
      #       panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.spacing=element_blank(),
      plot.background=element_blank(),
      plot.margin=unit(c(0, 0, 0, 0), "lines"),
      plot.title=element_text(size=size*1.2),
      strip.text.y=element_text(colour=background_colour,size=size,angle=270),
      strip.text.x=element_text(size=size*1))}

  ymin <- 0 # minimum width
  ymax <- 80 # maximum width
  xmin <- 60 # minimum length
  xmax <- 120 # maximum length

  # Defining features along the length
  boxEdgeOff <- 102
  sixYardOff <- 114
  penSpotOff <- 108
  halfwayline <- 60

  # Defining features along the width
  boxEdgeLeft <- 18
  boxEdgeRight <- 62
  sixYardLeft <- 30
  sixYardRight <- 50
  goalPostLeft <- 36
  goalPostRight <- 44
  CentreSpot <- 40

  # other dimensions
  centreCirle_d <- 20

  ## define the circle function
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  #### create leftD arc ####
  dArc <- circleFun(c((40),(penSpotOff)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  dArc <- dArc[which(dArc$y <= (boxEdgeOff)),]

  ## initiate the plot, set some boundries to the plot
  p <- ggplot() + xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
    # add the theme
    theme_blankPitch() +
    # add the base rectangle of the pitch
    geom_rect(aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill = grass_colour, colour = line_colour) +
    # add the 18 yard box offensive
    geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=boxEdgeOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
    # add the six yard box offensive
    geom_rect(aes(xmin=sixYardLeft, xmax=sixYardRight, ymin=sixYardOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
    # add the arc circle
    geom_path(data=dArc, aes(x=x,y=y), colour = line_colour) +
    # add penalty spot
    geom_point(aes(x = CentreSpot , y = penSpotOff), colour = line_colour) +
    # add the goal offensive
    geom_segment(aes(x = goalPostLeft, y = xmax, xend = goalPostRight, yend = xmax),colour = goal_colour, size = 1)

  return(p)

}

m <- create_StatsBomb_ShotMap("#ffffff", "#A9A9A9", "#ffffff", "#000000")

p <- create_StatsBomb_ShotMap("#538032", "#ffffff", "#538032", "#000000")
p
