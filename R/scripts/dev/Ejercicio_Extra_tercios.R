library(dplyr)
library(plotly)
library(ggimage)
library(ggplot2)
library(readxl)
library(ggrepel)
library(gridExtra)
library(readxl)
library(reshape2)

#mainDir <- "C:/Users/jlagosmi/Desktop/MASTER BIG DATA/PAQUETE_R/UnderStatPlots/data/Understat"
#setwd(file.path(mainDir))


setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")

equipos<-readRDS('ejericio_tercios.rds')

equipos <- read_excel('mls.xlsx')

# porcentajes por zona
equipos$intensidad1P<-as.numeric(format(round(((equipos$`PressuresDef 3rd`/(equipos$`PressuresDef 3rd`+equipos$`PressuresMid 3rd`+equipos$`PressuresAtt 3rd`))*100),2),nsmall = 2))
equipos$intensidad2P<-as.numeric(format(round(((equipos$`PressuresMid 3rd`/(equipos$`PressuresDef 3rd`+equipos$`PressuresMid 3rd`+equipos$`PressuresAtt 3rd`))*100),2),nsmall = 2))
equipos$intensidad3P<-as.numeric(format(round(((equipos$`PressuresAtt 3rd`/(equipos$`PressuresDef 3rd`+equipos$`PressuresMid 3rd`+equipos$`PressuresAtt 3rd`))*100),2),nsmall = 2))

# Esto es la normalizaci?n presi?n de cada zona por partido y posesion
equipos$intensidad1<-as.numeric(format(round(((equipos$`PressuresDef 3rd`/equipos$`Playing TimeMP`)/(100-equipos$Poss)),2),nsmall = 2))
equipos$intensidad2<-as.numeric(format(round(((equipos$`PressuresMid 3rd`/equipos$`Playing TimeMP`)/(100-equipos$Poss)),2),nsmall = 2))
equipos$intensidad3<-as.numeric(format(round(((equipos$`PressuresAtt 3rd`/equipos$`Playing TimeMP`)/(100-equipos$Poss)),2),nsmall = 2))

# Esto es el  promedio de la normalizaci?n presi?n de cada zona por partido y posesion
presion_total1 <- mean(equipos$intensidad1)
presion_total2 <- mean(equipos$intensidad2)
presion_total3 <- mean(equipos$intensidad3)

# la relaci?n de la presi?n por zonas respecto al promedio
equipos$intensidad1<-as.numeric(format(round(((equipos$intensidad1/presion_total1)*100)-100,2),nsmall = 2))
equipos$intensidad2<-as.numeric(format(round(((equipos$intensidad2/presion_total2)*100)-100,2),nsmall = 2))
equipos$intensidad3<-as.numeric(format(round(((equipos$intensidad3/presion_total3)*100)-100,2),nsmall = 2))


equipos<-select(equipos,Squad,intensidad1,intensidad2,intensidad3,intensidad1P,intensidad2P,intensidad3P,CODE)

equipos2<-select(equipos,Squad,intensidad1P,intensidad2P,intensidad3P,CODE) %>%
  mutate(p1=intensidad1P,
         p2=intensidad2P,
         p3=intensidad3P)

equipos2<-melt(equipos2)

equipos2<-equipos2 %>%
  mutate(zona=case_when(
    variable=='p1' ~ 16.6,
    variable=='p2' ~ 50,
    variable=='p3' ~ 83.4))

equipos<-select(equipos,Squad,intensidad1,intensidad2,intensidad3,CODE)

equipos<-melt(equipos)

equipos<-equipos %>%
  mutate(zona=case_when(
    variable=='intensidad1' ~ 16.6,
    variable=='intensidad2' ~ 50,
    variable=='intensidad3' ~ 83.4
  ))

equipos<-equipos %>%
  mutate(rango=case_when(
    value < -5 ~ 'Red',
    value < 5 ~ 'Yellow',
    value >= 5 ~ 'Green'
  ))


equipos$badge <- paste("./",equipos$CODE ,".png",sep="")

equipos_d<-distinct(equipos,Squad,badge)


OptaMAPcampofutbolblanco <- function(){

  #Creamos la plantilla del tema del gr?fico (esto es de soccermatics con algunos cambios)
  theme_blankPitch = function(size=12) {
    theme(
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.length=unit(0, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background=element_rect(fill="#FFFFFF", colour=NA),
      legend.key=element_rect(colour="#FFFFFF",fill="#FFFFFF"),
      legend.key.size=unit(1.2, "lines"),
      legend.text=element_text(size=size),
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = .5),
      panel.background=element_rect(fill="#FFFFFF",colour="#FFFFFF"),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.spacing=element_blank(),
      plot.background=element_blank(),
      plot.margin=unit(c(0, 0, 0, 0), "lines"),
      plot.title=element_text(size=size*1.2),
      strip.text.y=element_text(colour="#FFFFFF",size=size,angle=270),
      strip.text.x=element_text(size=size*1))}

  ymin <- 0
  xmin <- 0

  # Defining dimensions
  GoalWidth <- 732
  penspot <- 1100
  boxedgeW <- 4032
  boxedgeL <- 1650
  box6yardW <- 1832
  box6yardL <- 550

  ## dimensions calculations
  # The 18 Yard Box
  TheBoxWidth <- c(((7040 / 2) + (boxedgeW / 2)),((7040 / 2) - (boxedgeW / 2)))
  TheBoxHeight <- c(boxedgeL,10600-boxedgeL)
  GoalPosts <- c(((7040 / 2) + (GoalWidth / 2)),((7040 / 2) - (GoalWidth / 2)))

  # The 6 Yard Box
  box6yardWidth <- c(((7040 / 2) + (box6yardW / 2)),((7040 / 2) - (box6yardW / 2)))
  box6yardHeight <- c(box6yardL,10600-box6yardL)

  ## Centre circle dimensions
  centreCirle_d <- 1830

  ## define the circle function
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  #### create leftD arc ####
  Dleft <- circleFun(c((penspot),(7040/2)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  Dleft <- Dleft[which(Dleft$x >= (boxedgeL)),]

  ## create rightD arc  ####
  Dright <- circleFun(c((10600-(penspot)),(7040/2)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  Dright <- Dright[which(Dright$x <= (10600-(boxedgeL))),]

  #### create center circle ####
  center_circle <- circleFun(c((10600/2),(7040/2)),centreCirle_d,npoints = 100)

  ## create corner flag radius ####
  TopLeftCorner <- circleFun(c(xmin,7040),200,npoints = 1000)
  TopRightCorner <- circleFun(c(10600,7040),200,npoints = 1000)
  BottomLeftCorner <- circleFun(c(xmin,ymin),200,npoints = 1000)
  BottomRightCorner <- circleFun(c(10600,ymin),200,npoints = 1000)

  p <- ggplot() +

    xlim(c(-10,10600+10)) + ylim(c(-10,7040+10)) +
    theme_blankPitch() +
    geom_rect(aes(xmin=0, xmax=10600, ymin=0, ymax=7040), fill = "#ffffff", colour = "#000000") +
    geom_rect(aes(xmin=0, xmax=TheBoxHeight[1], ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = "#ffffff", colour = "#000000") +
    geom_rect(aes(xmin=TheBoxHeight[2], xmax=10600, ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = "#ffffff", colour = "#000000") +
    geom_rect(aes(xmin=0, xmax=box6yardHeight[1], ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = "#ffffff", colour = "#000000")  +
    geom_rect(aes(xmin=box6yardHeight[2], xmax=10600, ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = "#ffffff", colour = "#000000")  +
    geom_segment(aes(x = 10600/2, y = ymin, xend = 10600/2, yend = 7040),colour = "#000000") +
    geom_path(data=Dleft, aes(x=x,y=y), colour = "#000000") +
    geom_path(data=Dright, aes(x=x,y=y), colour = "#000000") +
    geom_path(data=center_circle, aes(x=x,y=y), colour = "#000000") +
    geom_point(aes(x = penspot , y = 7040/2), colour = "#000000") +
    geom_point(aes(x = (10600-(penspot)) , y = 7040/2), colour = "#000000") +
    geom_point(aes(x = (10600/2) , y = 7040/2), colour = "#000000") +
    geom_segment(aes(x = xmin, y = GoalPosts[1], xend = xmin, yend = GoalPosts[2]),colour = "#000000", size = 1) +
    geom_segment(aes(x = 10600, y = GoalPosts[1], xend = 10600, yend = GoalPosts[2]),colour = "#000000", size = 1)+
    theme(legend.position="bottom")

  return(p)

}

library(ggimage)

j=0
player_plot <- list()
for(i in equipos_d$Squad){
  j=j+1
  player_data<-dplyr::filter(equipos,Squad=="Atlanta Utd")
  player_data2<-dplyr::filter(equipos2,Squad=="Atlanta Utd")
  equipos_d1<-dplyr::filter(equipos_d,Squad=="Atlanta Utd")

  h<-OptaMAPcampofutbolblanco()

  p<-h +
    geom_image(data=equipos_d1,aes(x=5300,y=3520,image = badge), size = 0.4,alpha=.5) +
    geom_bar(data=player_data,aes(x=zona*106,y=7040,fill=as.factor(rango)),width=3500, stat="identity",alpha=0.5) +
    # scale_fill_gradient(low = "red", high = "green") +
    scale_fill_manual(
      values = c("Red" = "red","Green" = "green","Yellow" = "yellow")
    ) +
    annotate("text", x = player_data$zona*106, y = 1000, label = paste("(",player_data$value,"%)"),size=3) +
    annotate("text", x = player_data2$zona*106, y = 6500, label = paste(player_data2$value,"%"),size=4,fontface =2,parse=FALSE) +
    theme(legend.position="none")
p
  #a?adimos a la lista cada gr?fico
  player_plot[[j]] <- p

}


# ya podemos crear con grid.arrange el gr?fico en 2 columnas. si ponemos 1 saldr?n arriba y abajo
gridExtra::grid.arrange(grobs = c(player_plot), ncol = 7, as.table = FALSE)


#############################
# Ejercicio Facet jugadores
#############################

US_shots_Total <- readRDS("US_shots_Total.RData")

# por si aparecen nombres repetidos
dif<-US_shots_Total %>%
  distinct(player,player_id)

dif2<-dif %>%
  group_by(player) %>%
  summarise(conteo2=n()) %>%
  filter(conteo2>1)

US_shots_Total<-US_shots_Total %>%
  mutate(team_name=case_when(
    h_a=="h" ~ h_team,
    TRUE ~ a_team
  )) %>%
  left_join(dif2,by=c('player')) %>%
  mutate(player=case_when(
    conteo2>1 ~ paste(player,"-",team_name),
    TRUE ~ player
  ))

players<-US_shots_Total %>%
  distinct(player)


disparos_totales<-US_shots_Total %>%
  filter(situation!='OwnGoal') %>%
  group_by(player_id) %>%
  summarise(conteo=n()) %>%
  filter(conteo>=300) %>%
  ungroup() %>%
  arrange(-conteo) %>%
  top_n(8)



US_evo<-US_shots_Total %>%
  filter(situation!='OwnGoal') %>%
  inner_join(disparos_totales,by=c('player_id')) %>%
  mutate(gol=case_when(result=="Goal" ~ 1,
                       TRUE ~ 0))



library(zoo)

US_evo<-US_evo %>%
  select(player,xG,gol,date,situation,conteo) %>%
  group_by(player,conteo) %>%
  mutate(xGR = rollsum(xG, k = 50,align = "right", fill = NA),
         goalsR = rollsum(gol, k = 50, align = "right",fill = NA),
         shot=row_number()) %>%
  ungroup()



US_evo$dif<-US_evo$goalsR-US_evo$xGR

US_evo <- US_evo %>%
  arrange(-conteo)




ggplot(US_evo,aes(shot,dif,color = dif)) +
  geom_line(size=2) +
  scale_color_gradient2(low = ("darkblue"), high = ("red"),mid='gray',midpoint = 0, na.value = NA) +
  geom_hline(yintercept = 0,linetype='dashed')+
  ylim(-10, 10) +
  geom_text(
    data    = US_evo,
    mapping = aes(x = -Inf, y = -Inf, label = paste('Total shots: ',conteo,sep='')),
    hjust   = -0.1,
    vjust   = -1,
    color = 'black'
  ) +
  facet_wrap(~reorder(player,-conteo)) +
  xlab("Sequencies of 50 shots") +
  ylab("Goals above / below expectation, per 50 shots") +
  ggtitle("How finishing varies over time",
          subtitle = "Represented by Goals vs xG \n2014 to date. (Understat data)") +
  labs(caption = "Data source: UnderStat. Based on viz James Yorke @jair1970") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20, face = "bold"),
        strip.background = element_blank(),
        strip.placement = "inside",
        plot.subtitle = element_text(size = 16, face = "bold"),
        panel.border = element_rect(linetype=1, fill = NA),
        axis.title.x = element_text(color="black", size=10, face="italic"),
        axis.title.y = element_text(color="black", size=10, face="italic"),
        # strip.background = element_rect(
        #   color="white", fill="white", size=0, linetype="solid"),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold",hjust = 0

        ),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"
        ))

