library(ggplot2)
library(dplyr)

library(gganimate)
library(ggrepel)
library(ggimage)
library(readxl)
library(ggalt)
library(gridExtra)
mainDir <- "C:/Users/jlagosmi/Desktop/MASTER BIG DATA/PAQUETE_R/UnderStatPlots/data/Understat"
setwd(file.path(mainDir))

setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")
xgbet <- read_excel("Ranking equipos.xlsx",3)
xgbetcontra <- read_excel("Ranking equipos.xlsx",4)

US_season_Total<-readRDS("US_season_Total.RData")
US_shots_Total<-readRDS("US_shots_Total.RData")
US_player_Total<-readRDS("US_player_Total.RData")
US_player_Total1<-readRDS("US_player_Total1.RData")

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

# Ejercicio 6.1

Ejercicio<-US_shots_Total %>%
  filter((h_a=='h' & h_team=="Real Madrid" | h_a=='a' & a_team=='Real Madrid'),result!='OwnGoal',X>0.9, Y>0.4, Y<0.6)

Ejercicio1<- US_shots_Total %>%
  filter(X>0.5, X<0.7,Y>0.85, )

h<-createPitch_StatsBomb()

p <- h +
  geom_point(data=Ejercicio,aes(x=((1-Y)*80),y=(X*120))) +
  labs(subtitle = "Temporadas de 2014 a 2020",
       caption = "Fuente: Understat  Elaborado por: Erick")


anim <- p +
  transition_states(year,
                    transition_length = 1, # Cuanto dura la transición
                    state_length = 1) + # Cuanto dura la pausa entre estados
  ease_aes(y = 'bounce-out') # Sets special ease for y aesthetic

anim +
  ggtitle("Temporada {closest_state} del Real Madrid")

anim_save("ejericio6_2.gif", animation = last_animation())







# Ejercicio 6.2

jugadores<- US_player_Total %>%
  filter(year=='2020',time>200)


jugadores <- jugadores %>%
  mutate(goals90=(goals/time)*90,
         xG90=(xG/time)*90,
         assists90=(assists/time)*90,
         xA90=(xA/time)*90,
         key_passes90=(key_passes/time)*90,
         npg90=(npg/time)*90,
         npxG90=(npxG/time)*90,
         xGChain90=(xGChain/time)*90,
         xGBuildup90=(xGBuildup/time)*90,
         shots90=(shots/time)*90) %>%
  select(player_name,position,goals90,xG90,assists90,xA90,shots90,
         npg90,npxG90,xGChain90,xGBuildup90,position) %>%
  mutate(Posicion=case_when(
    substr(position,1,1)==('F') ~ "Delantero",
    substr(position,1,1)==('S') ~ "Sustituto",
    substr(position,1,1)==('M') ~ "Centrocampista",
    substr(position,1,1)==('D') ~ "Defensa"
  ))


kpi2<-jugadores %>%
  filter(player_name %in% c('Lionel Messi','Cristiano Ronaldo'))

kpi2<-reshape2::melt(kpi2)

kpi1<-reshape2::melt(jugadores)

kp3<-kpi1 %>%
  group_by(variable) %>%
  mutate(Quartile = ntile(value,10)) %>%
  ungroup()


ggplot() +
  ggforce::geom_sina(data=kp3,aes(x=variable,y=value,color=(Quartile)))+
  scale_color_gradient2(
    low = ("red"),
    mid = "yellow",
    high = ("green"),
    midpoint = mean(kp3$Quartile)
  ) +
  geom_point(data=kpi2,aes(x=variable,y=value),color="red",size=3) +
  coord_flip() +
  facet_wrap( ~ variable, scales = "free") + #, space = "free"
  theme_minimal() +
  theme(legend.position="none")+
  theme(strip.text.x = element_text(size=12, face="italic", color="darkblue")) +
  theme(panel.spacing.x = unit(.5, "cm")) +
  theme(panel.spacing.y = unit(1, "cm"))





# Ejercicio 6.3
library(ggrepel)

ggplot() +
  ggforce::geom_sina(data=kp3,aes(x=variable,y=value,color=(Quartile)))+
  scale_color_gradient2(
    low = ("red"),
    mid = "yellow",
    high = ("green"),
    midpoint = mean(kp3$Quartile)
  ) +
  geom_point(data=kpi2,aes(x=variable,y=value),color='red',size=3) +
  geom_label_repel(
    size = 3,
    data=kpi2,
    aes(x=variable, y=value,label=player_name),
    nudge_x=0.5) +
  coord_flip() +
  facet_wrap( ~ variable, scales = "free") + #, space = "free"
  theme_minimal() +
  theme(legend.position="none")+
  theme(strip.text.x = element_text(size=12, face="italic", color="darkblue")) +
  theme(panel.spacing.x = unit(.5, "cm")) +
  theme(panel.spacing.y = unit(1, "cm"))


# Ejercicio 6.4
library(ggimage)
library(readxl)
US_season_Total<-readRDS("US_season_Total.RData")
Teams <- read_excel("Teams.xlsx")


equipos<-US_season_Total %>%
  inner_join(Teams, by=c('team_name'='Understat')) %>%
  filter(year=='2020',Country=='Spain') %>%
  mutate(liga = case_when(
    league_name == 'Bundesliga' ~ 'GER_',
    league_name == 'EPL' ~ 'ENG_',
    league_name == 'La_liga' ~ 'ESP_',
    league_name == 'Ligue_1' ~ 'FRA_',
    league_name == 'Serie_A' ~ 'ITA_'
  )
  )

#df$badge <- paste(system.file(package="FootballBadges"),"/Badges/",df$Teams ,".png",sep="")
equipos$badge <- paste("./",equipos$liga,equipos$Code,".png",sep="")

equipos <- equipos %>%
  group_by(badge) %>%
  summarise(xG=sum(xG),xGA=sum(xGA)) %>%
  ungroup()



ggplot(data = equipos, aes(x = xG, y = xGA)) +
  ggtitle(label = "Análisis de xG" ,subtitle = "Comparativa xG y xGA") +
  geom_image(aes(image = badge), size = 0.04) +

  geom_segment(data = equipos,aes(x = median(xG), y = min(xGA), xend = median(xG), yend = max(xGA)),
               linetype="dashed",colour = "gray61",alpha=0.5) +
  geom_segment(data = equipos,aes(x = min(xG), y = median(xGA), xend = max(xG), yend = median(xGA)),
               linetype="dashed", colour = "gray61",alpha=0.5) +

  annotate(geom="text", x=max(equipos$xG), y=(max(equipos$xGA)-median(equipos$xGA))/8+median(equipos$xGA),
           label=paste("Median xGA =",format(round(median(equipos$xGA), 2), nsmall = 2)),
           color="gray",size=3) +
  annotate(geom="text", x=(max(equipos$xG)-median(equipos$xG))/8+median(equipos$xG), y=max(equipos$xGA),
           label=paste("Median xG =",format(round(median(equipos$xG), 2), nsmall = 2)),
           color="gray",size=3) +

  xlab("xG Totales") +
  ylab("xGA Totales") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5, face="bold.italic", size=14),
        plot.subtitle = element_text(hjust = 0.5, face="bold.italic", size=10),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "gray"))




# Ejercicio 6.5
US_season_Total<-readRDS("US_season_Total.RData")
Teams <- read_excel("Teams.xlsx")

equipos<-US_season_Total %>%
  inner_join(Teams, by=c('team_name'='Understat')) %>%
  filter(year=='2020',Country=='Spain') %>%
  mutate(liga = case_when(
    league_name == 'Bundesliga' ~ 'GER_',
    league_name == 'EPL' ~ 'ENG_',
    league_name == 'La_liga' ~ 'ESP_',
    league_name == 'Ligue_1' ~ 'FRA_',
    league_name == 'Serie_A' ~ 'ITA_'
  )
  )

equipos1<-US_season_Total %>%
  inner_join(Teams, by=c('team_name'='Understat')) %>%
  filter(year=='2020',Country=='England') %>%
  mutate(liga = case_when(
    league_name == 'Bundesliga' ~ 'GER_',
    league_name == 'EPL' ~ 'ENG_',
    league_name == 'La_liga' ~ 'ESP_',
    league_name == 'Ligue_1' ~ 'FRA_',
    league_name == 'Serie_A' ~ 'ITA_'
  )
  )

#df$badge <- paste(system.file(package="FootballBadges"),"/Badges/",df$Teams ,".png",sep="")
equipos$badge <- paste("./",equipos$liga,equipos$Code,".png",sep="")

equipos <- equipos %>%
  group_by(badge) %>%
  summarise(xG=sum(xG),xGA=sum(xGA))


equipos1$badge <- paste("./",equipos1$liga,equipos1$Code,".png",sep="")

equipos1 <- equipos1 %>%
  group_by(badge) %>%
  summarise(xG=sum(xG),xGA=sum(xGA))


grafico1<-ggplot(data = equipos, aes(x = xG, y = xGA)) +
  ggtitle(label = "Analisis de xG" ,subtitle = "Ratio Goles Reales / Goles Esperados") +
  geom_image(aes(image = badge), size = 0.04) +

  geom_segment(data = equipos,aes(x = median(xG), y = min(xGA), xend = median(xG), yend = max(xGA)), linetype="dashed",colour = "gray61",alpha=0.5) +
  geom_segment(data = equipos,aes(x = min(xG), y = median(xGA), xend = max(xG), yend = median(xGA)), linetype="dashed", colour = "gray61",alpha=0.5) +

  annotate(geom="text", x=max(equipos$xGA), y=(median(equipos$xGA)-median(equipos$xGA))/4+median(equipos$xGA), label=paste("Median Y =",format(round(median(equipos$xGA), 2), nsmall = 2)),
           color="gray",size=3) +
  annotate(geom="text", x=(median(equipos$xG)-median(equipos$xG))/4+median(equipos$xG), y=max(equipos$xGA), label=paste("Median X =",format(round(median(equipos$xG), 2), nsmall = 2)),
           color="gray",size=3) +

  xlab("Ratio de Goles a Favor") +
  ylab("Ratio de Goles en Contra") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5, face="bold.italic", size=14),
        plot.subtitle = element_text(hjust = 0.5, face="bold.italic", size=10),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "gray"))

grafico1



grafico2<-ggplot(data = equipos1, aes(x = xG, y = xGA)) +
  ggtitle(label = "Analisis de xG" ,subtitle = "Ratio Goles Reales / Goles Esperados") +
  geom_image(aes(image = badge), size = 0.04) +

  geom_segment(data = equipos1,aes(x = median(xG), y = min(xGA), xend = median(xG), yend = max(xGA)), linetype="dashed",colour = "gray61",alpha=0.5) +
  geom_segment(data = equipos1,aes(x = min(xG), y = median(xGA), xend = max(xG), yend = median(xGA)), linetype="dashed", colour = "gray61",alpha=0.5) +

  annotate(geom="text", x=max(equipos1$xGA), y=(median(equipos1$xGA)-median(equipos1$xGA))/4+median(equipos1$xGA), label=paste("Median Y =",format(round(median(equipos1$xGA), 2), nsmall = 2)),
           color="gray",size=3) +
  annotate(geom="text", x=(median(equipos1$xG)-median(equipos1$xG))/4+median(equipos1$xG), y=max(equipos1$xGA), label=paste("Median X =",format(round(median(equipos1$xG), 2), nsmall = 2)),
           color="gray",size=3) +

  xlab("Ratio de Goles a Favor") +
  ylab("Ratio de Goles en Contra") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5, face="bold.italic", size=14),
        plot.subtitle = element_text(hjust = 0.5, face="bold.italic", size=10),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "gray"))


player_plot <- list()
player_plot[[1]] <- grafico1
player_plot[[2]] <- grafico2

library(gridExtra)
grid.arrange(grobs = c(player_plot), ncol = 2, as.table = FALSE)




# Ejercicio 6.6

library(ggalt)


Ejercicio<-US_player_Total1 %>%
  filter(year=='2020') %>%
  arrange(desc(goals),desc(xG)) %>%
  top_n(n = 20,goals) %>%
  select(player_name,xG,goals) %>%
  mutate(diferencia=round(goals-xG,1))

Ejercicio$xG<-round(Ejercicio$xG,1)

Ejercicio <- arrange(Ejercicio, desc(diferencia))
Ejercicio$player_name <- factor(Ejercicio$player_name, levels=rev(Ejercicio$player_name))


gg <- ggplot()

gg <- gg + geom_segment(data=Ejercicio, aes(y=player_name, yend=player_name, x=20, xend=41), linetype='dashed',color="#b2b2b2", size=0.15)

gg <- gg + geom_dumbbell(data=Ejercicio, aes(y=player_name, x=xG, xend=goals),
                         size=1.5, color="#b2b2b2", size_x=6, size_xend=6,
                         colour_x="#9fb059", colour_xend="#edae52")

gg <- gg + geom_text(data=filter(Ejercicio, player_name=="Robert Lewandowski"),
                     aes(x=xG, y=player_name, label="xG"),
                     color="#9fb059", size=5, vjust=-1, fontface="bold")
gg <- gg + geom_text(data=filter(Ejercicio, player_name=="Robert Lewandowski"),
                     aes(x=goals, y=player_name, label="Goals"),
                     color="#edae52", size=5, vjust=-1, fontface="bold")

gg <- gg + geom_text(data=Ejercicio, aes(x=xG, y=player_name, label=(xG)),
                     color="white", size=2.75, vjust=0,nudge_y = -0.2)
gg <- gg + geom_text(data=Ejercicio, color="white", size=2.75, vjust=0,nudge_y = -0.2,
                     aes(x=goals, y=player_name, label=(goals)))

gg <- gg + geom_rect(data=Ejercicio, aes(xmin=41.3, xmax=42, ymin=Inf, ymax=Inf), fill="#efefe3")
gg <- gg + geom_text(data=Ejercicio, aes(label=diferencia, y=player_name, x=42.3), fontface="bold", size=3)
gg <- gg + geom_text(data=filter(Ejercicio, player_name=="Robert Lewandowski"), aes(x=42.3, y=player_name, label="DIFF"),
                     color="#7a7d7e", size=3.1, vjust=-2, fontface="bold")
gg <- gg + scale_x_continuous(expand=c(0,0), limits=c(10, 45))
gg <- gg + scale_y_discrete(expand=c(0.075,0))
gg <- gg + labs(x=NULL, y=NULL, title="Diferencia entre xG y Goles",
                subtitle="5 Grandes Ligas - Temporada 2020",
                caption="Fuente: Understat. Elaboración: Erick. @mrangel")
gg <- gg + theme_bw(base_family="Calibri")
gg <- gg + theme(panel.grid.major=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_blank())
gg <- gg + theme(plot.title=element_text(face="bold"))
gg <- gg + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(b=12)))
gg <- gg + theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e"))
gg

