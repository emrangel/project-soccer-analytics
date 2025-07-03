library(ggplot2)
library(dplyr)
library(plotly)



# Ejercicio 8.1 Plotly

#mainDir <- "C:/Users/jlagosmi/Desktop/MASTER BIG DATA/PAQUETE_R/UnderStatPlots/data/Understat"
#setwd(file.path(mainDir))


setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")

US_player_Total<-readRDS("US_player_Total.RData")


jugadores<- US_player_Total %>%
  filter(year==2019,games>30) %>%
  arrange(player_name)


jugadores <- jugadores %>%
  mutate(goals90=(goals/time)*90,
         xG90=(xG/time)*90,
         assists90=(assists/time)*90,
         xA90=(xA/time)*90,
         key_passes90=(key_passes/time)*90,
         npg90=(npg/time)*90,
         npxG90=(npxG/time)*90,
         xGChain90=(xGChain/time)*90,
         xGBuildup90=(xGBuildup/time)*90) %>%
  select(player_name,goals90,xG90,assists90,xA90,
         npg90,
         npxG90,xGChain90,xGBuildup90)


jugador1 <- jugadores %>%
  filter(player_name %in% c('Lionel Messi','Cristiano Ronaldo','Robert Lewandowski'))

datos <- jugador1 %>%
  select(-1)

datos[is.na(datos)] <- 0

nombre_campos<-colnames(datos)

n1<-as.character(jugador1[1,1])
n2<-as.character(jugador1[2,1])
n3<-as.character(jugador1[3,1])


plot_ly(
  type = 'scatterpolar',
  mode = "closest",
  fill = 'toself'
) %>%
  add_trace(
    r = as.matrix(datos[1,]),
    theta = nombre_campos,
    showlegend = TRUE,
    mode = "markers",
    name = n1
  ) %>%
  add_trace(
    r = as.matrix(datos[2,]),
    theta = nombre_campos,
    showlegend = TRUE,
    mode = "markers",
    name = n2
  ) %>%
  add_trace(
    r = as.matrix(datos[3,]),
    theta = nombre_campos,
    showlegend = TRUE,
    mode = "markers",
    name = n3
  ) %>%

  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,max(datos))
      )
    ),

    showlegend=TRUE


  )

# Ejercicio 8.2 Igraph + GGplot

library(igraph)

OptaMAPcampofutbol <- function(){

  #Creamos la plantilla del tema del gr?fico (esto es de soccermatics con algunos cambios)
  theme_blankPitch = function(size=12) {
    theme(
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.length=unit(0, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background=element_rect(fill="#538032", colour=NA),
      legend.key=element_rect(colour="#538032",fill="#538032"),
      legend.key.size=unit(1.2, "lines"),
      legend.text=element_text(size=size),
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = "#538032", fill = "#538032", size = .5),
      panel.background=element_rect(fill="#538032",colour="#538032"),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.spacing=element_blank(),
      plot.background=element_blank(),
      plot.margin=unit(c(0, 0, 0, 0), "lines"),
      plot.title=element_text(size=size*1.2),
      strip.text.y=element_text(colour="#538032",size=size,angle=270),
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

    xlim(c(-1000,10600+1000)) + ylim(c(-10,7040+10)) +
    theme_blankPitch() +
    geom_rect(aes(xmin=0, xmax=10600, ymin=0, ymax=7040), fill = "#538032", colour = "#ffffff") +
    geom_rect(aes(xmin=0, xmax=TheBoxHeight[1], ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = "#538032", colour = "#ffffff") +
    geom_rect(aes(xmin=TheBoxHeight[2], xmax=10600, ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = "#538032", colour = "#ffffff") +
    geom_rect(aes(xmin=0, xmax=box6yardHeight[1], ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = "#538032", colour = "#ffffff")  +
    geom_rect(aes(xmin=box6yardHeight[2], xmax=10600, ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = "#538032", colour = "#ffffff")  +
    geom_segment(aes(x = 10600/2, y = ymin, xend = 10600/2, yend = 7040),colour = "#ffffff") +
    geom_path(data=Dleft, aes(x=x,y=y), colour = "#ffffff") +
    geom_path(data=Dright, aes(x=x,y=y), colour = "#ffffff") +
    geom_path(data=center_circle, aes(x=x,y=y), colour = "#ffffff") +
    geom_point(aes(x = penspot , y = 7040/2), colour = "#ffffff") +
    geom_point(aes(x = (10600-(penspot)) , y = 7040/2), colour = "#ffffff") +
    geom_point(aes(x = (10600/2) , y = 7040/2), colour = "#ffffff") +
    geom_segment(aes(x = xmin, y = GoalPosts[1], xend = xmin, yend = GoalPosts[2]),colour = "#000000", size = 1) +
    geom_segment(aes(x = 10600, y = GoalPosts[1], xend = 10600, yend = GoalPosts[2]),colour = "#000000", size = 1)+
    theme(legend.position="bottom")

  return(p)

}

OptaMAPcampofutbol()

segmentsDf <- function(data, shorten.start, shorten.end, offset){

  data$dx = data$x - data$avg_x
  data$dy = data$y - data$avg_y
  data$dist = sqrt( data$dx^2 + data$dy^2 )
  data$px = data$dx/data$dist
  data$py = data$dy/data$dist


  data$x2 = data$avg_x + data$px * shorten.start
  data$y2 = data$avg_y + data$py * shorten.start
  data$xend = data$x - data$px * shorten.end
  data$yend = data$y - data$py * shorten.end
  data$x2 = data$x2 - data$py * offset
  data$xend = data$xend - data$py * offset
  data$y2 = data$y2 + data$px * offset
  data$yend = data$yend + data$px * offset

  data<-data.frame(data)

  return(data)
}

player_data<-readRDS('redes.rds')

str(player_data)

player_data$names<-as.integer(player_data$names)

player_data<-player_data %>%
  filter(team_name=='Barcelona')

# Creamos los nodos
knodes<-player_data %>%
  group_by(player_name,x,y) %>%
  summarise(total=sum((names)))

pasestotales<-sum((player_data$names))

# Saco la matriz sin portero para calcular el convex hull y sacar profundidad y amplitud
player_data_gk<-player_data %>%
  filter(position!="Goalkeeper")

# Me quedo con duplas de m?s de 2 o m?s pases
player_data_gk<-player_data_gk %>%
  filter(names>=2)

# valores para profundidad y amplitud
x_max_prof<-max(player_data_gk$x)*106
x_min_prof<-min(player_data_gk$x)*106
y_max_ampl<-max(player_data_gk$y)*70.5
y_min_ampl<-min(player_data_gk$y)*70.5


hull_coor <- player_data_gk %>%
  slice(chull(x,y)) %>%
  mutate(hull1= 1)

centro_pol<-player_data_gk %>%
  summarise(xm=mean(x),ym=mean(y))

grafo<-player_data %>%
  select(player_name,player_name2,names,team_name)

names(grafo)<- c('to','from','pases','equipo')

grafo<-filter(grafo,pases>=2)

#creamos el grafo que permite sacar metricas
Colleague_Graph_kedges=igraph::graph.data.frame(grafo, directed=TRUE)

#simplificamos los looping (es raro que haya pero alguno he visto) y las multiplicidades
Colleague_Graph_kedges_d<-igraph::simplify(Colleague_Graph_kedges, remove.multiple=TRUE, remove.loops=TRUE)

#Calculamos la densidad del grafo

densidad<-igraph::graph.density(Colleague_Graph_kedges_d,loop=FALSE)
media<-igraph::mean_distance(Colleague_Graph_kedges_d,directed=FALSE,unconnected=TRUE)
parejas_combinadas<-dyad_census(Colleague_Graph_kedges_d)$mut
parejas_1combinadas<-dyad_census(Colleague_Graph_kedges_d)$asym
parejas_sincombinadas<-dyad_census(Colleague_Graph_kedges_d)$null
triangulacion<-transitivity(Colleague_Graph_kedges_d, type="global")
grado=degree(Colleague_Graph_kedges_d, mode="all")
centralidad<-closeness(Colleague_Graph_kedges_d, mode="all")
eigenvector<-centr_eigen(Colleague_Graph_kedges_d, directed=T, normalized=T)$vector
hs <- hub_score(Colleague_Graph_kedges_d, weights=NA)$vector
as <- authority_score(Colleague_Graph_kedges_d, weights=NA)$vector

#Calculamos la centralidad de cada jugador
bet<-igraph::betweenness(Colleague_Graph_kedges_d,directed = TRUE,normalized = TRUE)
bet2 <- as.data.frame(bet)
bet3 <- cbind(Label = rownames(bet2), bet2)
bet3[,'Label'] <- as.character(bet3[,'Label'])

library(scales)

  #unimos a los nodos el valor de la centralidad
knodes$total<-as.numeric(knodes$total)
knodes10 <- knodes %>%
    ungroup() %>%
    mutate(size = scales::rescale(total, c(1,5), c(min(total), max(total)))) %>%
    left_join(bet3, by = c("player_name"="Label"))

curva<-player_data %>%
    group_by(player_name,player_name2) %>%
    summarise(avg_x=mean(x),avg_y=mean(y),to=sum(as.integer(names)))

curva<-curva %>%
      select(player_name,avg_x,avg_y,player_name2,to) %>%
      filter(to>=2) %>%
      inner_join(knodes,by=c('player_name2'='player_name'))  %>%
      mutate(size2 = scales::rescale(to, c(0.1, 2), c(min(to), max(to))))


  curva <- curva %>%
    segmentsDf(2,2, 1)

  arrow <- arrow(type = "closed", angle = 30, length = unit(0.1, "inches"))

  library(ggrepel)

  h <- OptaMAPcampofutbol()
  p <- h +
    geom_polygon(hull_coor, mapping=aes(x= x*106, y=y*70.5), alpha = 0.4,linetype = "dashed",size = 1,show.legend = FALSE) +
    geom_point(data = centro_pol,aes(x = xm*106,y =ym*70.4),shape = 21, colour = "black", size = 5, stroke = 5) +
    geom_segment(data=centro_pol,aes(x = xm*106, y = ym*70.5, xend = 0, yend = 7050/2),colour = "#000000", size = 1) +
    geom_text(data = centro_pol,aes(x = (xm*106)/2,y =(ym-5)*70.5,label=format(sqrt((xm*1.06-0)^2+(ym*0.7-35.2)),digits=2,nsmall=2)), colour="black") +

    #    geom_segment(data = curva, aes(x = xend*106, y =(yend*70.5), xend = x2*106, yend =( y2*70.5), size = size2,color=to), arrow = arrow, alpha = 1) +
    geom_segment(data = curva, aes(x = x2*106, y =(y2*70.5), xend = xend*106, yend =( yend*70.5), size = size2,color=to), arrow = arrow, alpha = 1) +

    scale_color_gradient(low = "gray", high = "red")+
    geom_point(data = knodes10,aes(x = x*106,y = (y*70.5),size = size,fill=bet),color='black',shape=21,stroke = 1) +

    ggrepel::geom_label_repel(
      data = knodes10,
      aes(x = x*106,y = (y*70.5),label=player_name),
      nudge_y = 400,
      size=3) +

    geom_segment(aes(x = x_min_prof, y = 6900, xend = x_max_prof, yend = 6900),colour = "#000000", size = 1) +
    geom_segment(aes(x = x_min_prof, y = 7025, xend = x_min_prof, yend = 6775),colour = "#000000", size = 1) +
    geom_segment(aes(x = x_max_prof, y = 7025, xend = x_max_prof, yend = 6775),colour = "#000000", size = 1) +
    geom_text(aes(x=(x_max_prof-x_min_prof)/2+x_min_prof,y=6725,label=format((x_max_prof-x_min_prof)/100,digits=2,nsmall=2)), colour="black") +


    geom_segment(aes(x = 9650, y = y_min_ampl, xend = 9650, yend = y_max_ampl),colour = "#000000", size = 1) +
    geom_segment(aes(x = 9525, y = y_min_ampl, xend = 9725, yend = y_min_ampl),colour = "#000000", size = 1) +
    geom_segment(aes(x = 9525, y = y_max_ampl, xend = 9725, yend = y_max_ampl),colour = "#000000", size = 1) +
    geom_text(aes(x=9450,y=(y_min_ampl-y_max_ampl)/2+y_max_ampl,label=format((y_max_ampl-y_min_ampl)/100,digits=2,nsmall=2)), colour="black") +

    scale_size_identity() +

    annotate("text", label = paste("Pases Totales en Matriz: ",pasestotales), x = 100, y = 200, size = 5, colour = "black",hjust = 0)+
    annotate("text", label = paste("Densidad del grafo: ",format(densidad, digits=2, nsmall=2)), x = 100, y = 500, size = 5, colour = "black",hjust = 0)+
    annotate("text", label = paste("Duplas combinadas: ",format(parejas_combinadas, digits=0, nsmall=2)), x = 100, y = 800, size = 5, colour = "black",hjust = 0)+
    annotate("text", label = paste("Duplas monodirigidas: ",format(parejas_1combinadas, digits=0, nsmall=2)), x = 100, y = 1100, size = 5, colour = "black",hjust = 0)+
    annotate("text", label = paste("Duplas sin combinación: ",format(parejas_sincombinadas, digits=0, nsmall=2)), x = 100, y = 1400, size = 5, colour = "black",hjust = 0)+
    annotate("text", label = paste("Triangulación: ",format(triangulacion, digits=0, nsmall=2)), x = 100, y = 1700, size = 5, colour = "black",hjust = 0)+
    theme(legend.position="none")


p

ggplotly(p)


# ejercicio 8.3 visnetwork

temp2<- readRDS("vis.rds")
nodes<- readRDS("nodes_manager.rds")

temp2 <- temp2 %>%
  left_join(nodes,by=c('from'='label')) %>%
  left_join(nodes,by=c('to'='label'))

temp2 <- temp2 %>%
  select(id.x,id.y)

names(temp2)<-c('from','to')

edges<-temp2

edges2<-edges %>%
  distinct(from)

nodes<-nodes %>%
  inner_join(edges2,by=c('id'='from')) %>%
  select(id,label,shape,image,group)

library(visNetwork)

visNetwork(nodes, edges) %>%
  visEdges(arrows = "to") %>%
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = list(enabled = TRUE)) %>%
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visLayout(randomSeed = 2)
