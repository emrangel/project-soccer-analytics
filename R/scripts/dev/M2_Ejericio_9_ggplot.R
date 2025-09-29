library(dplyr)
library(plotly)
library(ggimage)
library(ggplot2)
library(readxl)
library(ggrepel)
library(gridExtra)

setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")

US_player_Total <- readRDS("US_player_Total.RData")
US_season_Total <- readRDS("US_season_Total.RData")
US_shots_Total <- readRDS("US_shots_Total.RData")
Teams <- read_excel("Teams.xlsx")
seasons <- readRDS("seasons.RData")
leagues <- readRDS("leagues.RData")


# Vamos a construir una funci?n que cuando la llamemos dibuje el gráfico.
# Tiene 2 parámetros, un dataframe que tiene que llevar 3 columnas y el tamaño del escudo

Ejercicio9 <- function(df,tam){

  # Como usaremos esta función para varios gráficos vamos a transformar los nombres de los campos
  # a algo más genérico. F?jate que el nombre de la tabla es df. Aunque la tabla que le pasemos tenga otro
  # nombre para la función es df
  # con "names(df)" le podemos cambiar los nombres a los campos
  names(df)<-c('Teams','Xaxis1','Yaxis1')

  # Como vimos en ggimage tenemos que crear un campo donde venga la ruta relativa de la imagen. Para ello
  # Tendremos que tener los escudos en el mismo directorio de trabajo donde estemos trabajando.
  df$badge <- paste("./",df$Teams ,".png",sep="")


  # Aqu? empezamos a dibujar el gráfico. Como le hemos cambiado el nombre a los campos da igual la tabla que
  # venga, lo que tenemos que tener claro es que tiene que tener 3 campos, y el segundo será la coordenada X
  # y el tercero la coordenada Y
  p<-ggplot(data = df, aes(x = Xaxis1, y = Yaxis1)) +
    # Vimos que con labs() podiamos incluir el titulo y el subtitulo. Otra opción es ggtittle
    ggtitle(label = "An?lisis de xG" ,subtitle = "Ratio Goles Reales / Goles Esperados") +
    # Aqu?? le decimos en que campo está la ruta de la imagen y podéis ver el parámetro tam que hemos pasado
    geom_image(aes(image = badge), size = tam) +
    # Una cosa interesante al gráfico es poder delimitar lo que está por encima de la mediana y por debajo
    # Yo prefiero usar la mediana al promedio.
    # Para ello usamos geom_segment que como vimos dibuja un segmento
    # Pintamos dos segmentos: el primero es el vertical, que parte en 2 el eje X sobre la mediana.
    # Para eso le indicamos que el comienzo y final será la mediana de X y tendrá de longitud
    # el m?nimo y el máximo de Y.
    # F?jate también que ponemos como atributo la transparencia alpha, el color y el tipo dashed
    geom_segment(data = df,aes(x = median(Xaxis1), y = min(Yaxis1), xend = median(Xaxis1), yend = max(Yaxis1)), linetype="dashed",colour = "gray61",alpha=0.5) +
    geom_segment(data = df,aes(x = min(Xaxis1), y = median(Yaxis1), xend = max(Xaxis1), yend = median(Yaxis1)), linetype="dashed", colour = "gray61",alpha=0.5) +
    # Además de pintar la linea es interesante marcar el valor. En este caso lo hacemos con anotate(text)
    # Aqu? la clave es donde situar el texto que lo marcaremos en x y en y y la etiqueta en label.
    # El primero es el texto que irá sobre la mediana del Eje Y. Por eso decimos que lo situe
    # en el final del Eje X y para la Y es donde viene el truco. Si lo ponemos sobre la mediana de Y
    # Lo pintará sobre la linea que hemos dibujado.
    # Si metemos algo a mano para desplazar en función de los valores podrá ser mucho o poco.
    # Para ello lo que hacemos es calcular un desplazamiento basado en el máximo y mediana entre 4 y es lo
    # sumamos a la mediana
    # Como la división devuelve muchos decimales los dejamos en 2 con format()
    annotate(geom="text", x=max(df$Xaxis1), y=(max(df$Yaxis1)-median(df$Yaxis1))/4+median(df$Yaxis1), label=paste("Median Y =",format(round(median(df$Yaxis1), 2), nsmall = 2)),
             color="gray",size=3) +
    annotate(geom="text", x=(max(df$Xaxis1)-median(df$Xaxis1))/4+median(df$Xaxis1), y=max(df$Yaxis1), label=paste("Median X =",format(round(median(df$Xaxis1), 2), nsmall = 2)),
             color="gray",size=3) +
    # También vimos como con labs podias poner el eje x e Y, aquí tenéis otra opción:
    xlab("Ratio de Goles a Favor") +
    ylab("Ratio de Goles en Contra") +
    # Ponemos el texto inferior con nuestra fuente
    labs(caption = "Fuente: Understat  Elaborado por: Jes?s Lagos. @Vdot_spain") +
    # Quitamos la leyenda
    theme(legend.position="none") +
    # Ajustamos elementos del tema
    theme(plot.title = element_text(hjust = 0.5, face="bold.italic", size=14),
          plot.subtitle = element_text(hjust = 0.5, face="bold.italic", size=10),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(size = 0.5, linetype = "solid",
                                   colour = "gray"))
  
  return(p)
}

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


################
# Ejercicio 9.1
################

# Partimos de dos tablas. US_season_Total donde tenemos todos los datos y de Teams que es donde
# tenemos los códigos de todos los equipos para poder meterle luego el escudo

equipos<-US_season_Total %>%
  # Metemos un filtro de liga para quedarnos con una liga y una temporada En shiny esto será parametrizado
  # recordad que "," es como decir "y" (que cumplan ambas condiciones) y que "|" (altgr + 1) es como "o"
  # que cumpla alguna de las dos condiciones
  # En este filtro nos salen 166 registros(cuando yo hice esto el 26/11/2020)
  filter(league_name=="La_liga",year=='2020') %>%
  # Aquí unimos con Teams para meter los códigos para luego meter el escudo
  # importante. es un inner join, por lo que lo que no cruce se quitará. Hay que comprobar que tenemos
  # los mismos registros. Si tuvieramos menos es que en la tabla Teams falta algún equipo.
  inner_join(Teams, by=c('team_name'='Understat')) %>%
  # En la tabla tenemos un registro por cada partido del equipo pero queremos un solo registro por equipo
  # para ello agrupamos por los 4 campos que necesitamos
  group_by(league_name,team_name,Code,Country) %>%
  # y aquí calculamos los nuevos campos como suma. Como aquí necesitamos solo los xG, xG, goles a favor y en contra
  # nos quedamos con esos campos. Una buena estrategia es saber para que vamos a usar esta tabla y dejar los
  # campos necesarios para que nos sirva para cada cosa, y luego filtrar según necesidades.
  # en próximos ejercicios usaremos todos los campos.
  summarise(xG=sum(xG),scored=sum(scored),xGA=sum(xGA),missed=sum(missed),npxG=sum(npxG),npxGA=sum(npxGA),
            ratiof=sum(scored)/sum(xG),ratioc=sum(missed)/sum(xGA),ratiofp=sum(scored)/sum(npxG),
            ratiocp=sum(missed)/sum(npxGA),puntos=sum(pts),
            deep=sum(deep),ppda.att=sum(ppda.att),ppda.def=sum(ppda.def),
            PPDAC=sum(ppda.att)/sum(ppda.def),PPDAF=sum(ppda_allowed.att)/sum(ppda_allowed.def),DCC=sum(deep_allowed),DCF=sum(deep),xpts=sum(xpts)) %>%
  ungroup() %>%
  # Con mutate podemos construir nuevos campos. En este caso vamos a crear un campo con el codigo de liga.
  # se leeria así. "Creame un campo liga, cuando el campo league_name sea igual a "La_liga" le das el valor
  # 'ESP_"......
  mutate(liga = case_when(
    league_name == 'Bundesliga' ~ 'GER_',
    league_name == 'EPL' ~ 'ENG_',
    league_name == 'La_liga' ~ 'ESP_',
    league_name == 'Ligue_1' ~ 'FRA_',
    league_name == 'Serie_A' ~ 'ITA_'
  )
  )

# Ya tenemos la tabla, pero falta dejar construido el código del equipo que lo haremos
# concatenando el nuevo campo liga y el campo Code que viene del inner_join con Teams
equipos$code1 <- paste(equipos$liga,equipos$Code,sep='')



# Vamos a quedarnos con la tabla de 3 campos necesaria
# con select seleccionamos los 3 campos necesarios
# Si vamos al group_by podemos ver que campos he usado ratiof=sum(scored)/sum(xG),ratioc=sum(missed)/sum(xGA)
equipos91 <- equipos %>%
  select(code1,ratiof,ratioc)

# Ya podemos llamar a la función para que dibuje el gráfico
Ejercicio9(equipos91,0.04)


# ¿qué le añadirías al gráfico? Yo por ejemplo, le pondría 4 rectangulos, uno por cuadrante con colores,
# donde el cuadrante inferior derecha fuera verde ya que son los equipos que marcan más goles de los que se
# se le esperan (>1) y reciben menos goles de los se esperan (<1). Incluso pondría el texto!
# Vamos a modificar la función y parametrizar aun mucho más todo!
# Mira todos los parámetros que vamos a poner

Ejercicio9bis <- function(df,tam,titulo,subtitulo,cuadrante1,cuadrante2,cuadrante3,cuadrante4,ejeX,ejeY,color1,color2,color3,color4){
  
  # Lo importante son saber el orden de los cuadrantes:
  # cuadrante 1 = Inferior Izquierda
  # cuadrante 2 = Superior Izquierda
  # cuadrante 3 = Superior Derecha
  # cuadrante 4 = Inferior Derecha
  
  names(df)<-c('Teams','Xaxis1','Yaxis1')
  
  df$badge <- paste("./",df$Teams ,".png",sep="")
  
  # Vamos a crear un data frame con los valores que usaremos para los rectángulos
  minX <- min(df$Xaxis1)
  maxX <- max(df$Xaxis1)
  medianX <- median(df$Xaxis1)
  minY <- min(df$Yaxis1)
  maxY <- max(df$Yaxis1)
  medianY <- median(df$Yaxis1)
  
  p<-ggplot(df, aes(Xaxis1,Yaxis1)) +
    # El titulo y subtitulo vienen como parámetro
    ggtitle(label = titulo ,subtitle = subtitulo) +
    # Aqui ponemos los rectangulos. Los ponemos primero porque ggplot dibuja en orden.
    # Aqui hay que ir en orden vamos a parametrizar los colores tambien
    annotate("rect",xmin=minX, xmax=medianX, ymin=minY, ymax=medianY, fill=color1, alpha=0.3) +
    annotate("rect",xmin=minX, xmax=medianX, ymin=medianY, ymax=maxY, fill=color2, alpha=0.3) +
    annotate("rect",xmin=medianX, xmax=maxX, ymin=medianY, ymax=maxY, fill=color3, alpha=0.3) +
    annotate("rect",xmin=medianX, xmax=maxX, ymin=minY, ymax=medianY, fill=color4, alpha=0.3) +
    geom_image(aes(image = badge), size = tam) +
    geom_segment(data = df,aes(x = median(Xaxis1), y = min(Yaxis1), xend = median(Xaxis1), yend = max(Yaxis1)),
                 linetype="dashed",colour = "gray61",alpha=0.5) +
    geom_segment(data = df,aes(x = min(Xaxis1), y = median(Yaxis1), xend = max(Xaxis1), yend = median(Yaxis1)),
                 linetype="dashed", colour = "gray61",alpha=0.5) +
    annotate(geom="text", x=max(df$Xaxis1)-(max(df$Xaxis1)-median(df$Xaxis1))/10, y=(max(df$Yaxis1)-median(df$Yaxis1))/10+median(df$Yaxis1),
             label=paste("Median Y =",round(median(df$Yaxis1), 2)),
             color="gray",size=3) +
    annotate(geom="text", x=(max(df$Xaxis1)-median(df$Xaxis1))/10+median(df$Xaxis1), y=max(df$Yaxis1), label=paste("Median X =",round(median(df$Xaxis1), 2)),
             color="gray",size=3) +
    #Aquí ponemos los textos!
    annotate(geom="text", x=minX, y=minY, label=cuadrante1,color="black",size=3,hjust = 0,parse = FALSE) +
    annotate(geom="text", x=minX, y=maxY, label=cuadrante2,color="black",size=3,hjust = 0,parse = FALSE) +
    annotate(geom="text", x=maxX, y=maxY, label=cuadrante3,color="black",size=3,hjust = 1,parse = FALSE) +
    annotate(geom="text", x=maxX, y=minY, label=cuadrante4,color="black",size=3,hjust = 1,parse = FALSE) +
    labs(x=ejeX,
         y=ejeY,
         caption = "Fuente: Understat  Elaborado por: Jes?s Lagos. @Vdot_spain") +
    theme(legend.position="none") +
    theme(plot.title = element_text(hjust = 0.5, face="bold.italic", size=14),
          plot.subtitle = element_text(hjust = 0.5, face="bold.italic", size=10),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(size = 0.5, linetype = "solid",
                                   colour = "gray"))
  
  return(p)
}

Ejercicio9bis(df=equipos91,
              tam=0.04,
              titulo="An?lisis xG",
              subtitulo="Ratio Goles Reales / Goles Esperados",
              cuadrante1="Menos goles a favor y en contra de los esperados",
              cuadrante2="M?s goles en contra de lo esperado en contra y menos a favor",
              cuadrante3="M?s goles en contra pero m?s goles a favor",
              cuadrante4="Más goles a favor y menos en contra de lo esperado",
              ejeX="Ratio de Goles a Favor",
              ejeY="Ratio de Goles en Contra",
              color1="yellow",
              color2="red",
              color3="orange",
              color4="green")


################
# Ejercicio 9.2
################

Ejercicio92 <- function(df,tam,titulo,subtitulo,cuadrante1,cuadrante2,cuadrante3,cuadrante4,ejeX,ejeY,color1,color2,color3,color4){
  
  # Lo importante son saber el orden de los cuadrantes:
  # cuadrante 1 = Inferior Izquierda
  # cuadrante 2 = Superior Izquierda
  # cuadrante 3 = Superior Derecha
  # cuadrante 4 = Inferior Derecha
  
  names(df)<-c('Teams','Xaxis1','Yaxis1')
  
  df$badge <- paste("./",df$Teams ,".png",sep="")
  
  # Vamos a crear un data frame con los valores que usaremos para los rectángulos
  minX <- min(df$Xaxis1)
  maxX <- max(df$Xaxis1)
  medianX <- median(df$Xaxis1)
  minY <- min(df$Yaxis1)
  maxY <- max(df$Yaxis1)
  medianY <- median(df$Yaxis1)
  
  # la ubicación para el texto de R2
  x1<-mean(df$Xaxis1)
  y1<-mean(df$Yaxis1)
  
  
  p<-ggplot(df, aes(Xaxis1,Yaxis1)) +
    # El titulo y subtitulo vienen como parámetro
    ggtitle(label = titulo ,subtitle = subtitulo) +
    # Aqui ponemos los rectangulos. Los ponemos primero porque ggplot dibuja en orden.
    # Aqui hay que ir en orden vamos a parametrizar los colores tambien
    annotate("rect",xmin=minX, xmax=medianX, ymin=minY, ymax=medianY, fill=color1, alpha=0.3) +
    annotate("rect",xmin=minX, xmax=medianX, ymin=medianY, ymax=maxY, fill=color2, alpha=0.3) +
    annotate("rect",xmin=medianX, xmax=maxX, ymin=medianY, ymax=maxY, fill=color3, alpha=0.3) +
    annotate("rect",xmin=medianX, xmax=maxX, ymin=minY, ymax=medianY, fill=color4, alpha=0.3) +
    geom_image(aes(image = badge), size = tam) +
    # Aquí añadimos la recta de regresión con el método "lm" -> linear model.
    geom_smooth(method = "lm", se=FALSE, color="black") +
    # Aquí ponemos el texto del R2 pasandolo por la función lm_eqn
    geom_text(x = x1, y = y1, label = lm_eqn(df), parse = TRUE) +
    geom_segment(data = df,aes(x = median(Xaxis1), y = min(Yaxis1), xend = median(Xaxis1), yend = max(Yaxis1)), linetype="dashed",colour = "gray61",alpha=0.5) +
    geom_segment(data = df,aes(x = min(Xaxis1), y = median(Yaxis1), xend = max(Xaxis1), yend = median(Yaxis1)), linetype="dashed", colour = "gray61",alpha=0.5) +
    annotate(geom="text", x=max(df$Xaxis1)-(max(df$Xaxis1)-median(df$Xaxis1))/10, y=(max(df$Yaxis1)-median(df$Yaxis1))/10+median(df$Yaxis1), label=paste("Median Y =",round(median(df$Yaxis1), 2)),
             color="gray",size=3) +
    annotate(geom="text", x=(max(df$Xaxis1)-median(df$Xaxis1))/10+median(df$Xaxis1), y=max(df$Yaxis1), label=paste("Median X =",round(median(df$Xaxis1), 2)),
             color="gray",size=3) +
    #Aquí ponemos los textos!
    annotate(geom="text", x=minX, y=minY, label=cuadrante1,color="black",size=3,hjust = 0,parse = FALSE) +
    annotate(geom="text", x=minX, y=maxY, label=cuadrante2,color="black",size=3,hjust = 0,parse = FALSE) +
    annotate(geom="text", x=maxX, y=maxY, label=cuadrante3,color="black",size=3,hjust = 1,parse = FALSE) +
    annotate(geom="text", x=maxX, y=minY, label=cuadrante4,color="black",size=3,hjust = 1,parse = FALSE) +
    labs(x=ejeX,
         y=ejeY,
         caption = "Fuente: Understat  Elaborado por: Jesús Lagos. @Vdot_spain") +
    theme(legend.position="none") +
    theme(plot.title = element_text(hjust = 0.5, face="bold.italic", size=14),
          plot.subtitle = element_text(hjust = 0.5, face="bold.italic", size=10),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(size = 0.5, linetype = "solid",
                                   colour = "gray"))
  
  return(p)
}

# Ya sabemos crear funciones, veamos que hace esta
lm_eqn <- function(df1){
  # lanzamos un modelo simple, linear donde se relacione x con y
  m = lm(Xaxis1 ~ Yaxis1, df1);
  # en m se han guardado los parámetros del modelo. en summary(m)$r.squared tenemos el valor que necesitamos.
  eq <- substitute(italic(r)^2~"="~r2,
                   list(r2 = format(summary(m)$r.squared, digits = 2)))
  
  return(as.character(as.expression(eq)));
}

#Dijimos que ibamos a mezclar dos gráficos
# favor
equipos1 <- equipos %>%
  select(code1,xG,scored)
# en contra
equipos2 <- equipos %>%
  select(code1,xGA,missed)

p<-Ejercicio92(df=equipos1,
               tam=0.04,
               titulo="An?lisis xG y Goles a Favor",
               subtitulo="Liga Espa?ola temporada 2020",
               cuadrante1="Menos goles a favor con pocas ocasiones",
               cuadrante2="Muchos goles con pocas ocasiones",
               cuadrante3="Muchos goles con muchas ocaiones",
               cuadrante4="Pocos goles con muchas ocasiones",
               ejeX="Goles",
               ejeY="Expected Goal A Favor",
               color1="orange",
               color2="yellow",
               color3="green",
               color4="red")


w<-Ejercicio92(df=equipos2,
               tam=0.04,
               titulo="An?lisis xG y Goles en Contra",
               subtitulo="Liga Española temporada 2020",
               cuadrante1="Menos goles y ocasiones recibidas",
               cuadrante2="Muchos goles y pocas ocasiones recibidas",
               cuadrante3="Muchos goles y muchas ocasiones recibidas",
               cuadrante4="Pocos goles pero muchas ocasiones recibidas",
               ejeX="Goles",
               ejeY="Expected Goal en Contra",
               color1="green",
               color2="orange",
               color3="red",
               color4="yellow")


player_plot <- list()
player_plot[[1]] <- p
player_plot[[2]] <- w

library(gridExtra)
grid.arrange(grobs = c(player_plot), ncol = 2, as.table = FALSE)



################
# Ejercicio 9.3
################

# a favor
equipos1 <- equipos %>%
  select(code1,xG,puntos)
# en contra
equipos2 <- equipos %>%
  select(code1,xGA,puntos)

p<-Ejercicio92(df=equipos1,
               tam=0.04,
               titulo="An?lisis xG y Puntos",
               subtitulo="Liga Española temporada 2020",
               cuadrante1="Equipos con pocas ocasiones y pocos puntos",
               cuadrante2="Equipos con muchas ocasiones y pocos puntos",
               cuadrante3="Equipos con muchas ocasiones y muchos puntos",
               cuadrante4="Equipos con pocas ocasiones y muchos puntos",
               ejeX="Expected Goal A Favor",
               ejeY="Puntos",
               color1="orange",
               color2="yellow",
               color3="green",
               color4="red")


w<-Ejercicio92(df=equipos2,
               tam=0.04,
               titulo="An?lisis xGA y Puntos",
               subtitulo="Liga Española temporada 2020",
               cuadrante1="Equipos con pocas ocasiones y pocos puntos",
               cuadrante2="Equipos con pocas ocasiones y muchos puntos",
               cuadrante3="Equipos con muchas ocasiones y muchos puntos",
               cuadrante4="Equipos con muchas ocasiones y pocos puntos",
               ejeX="Expected Goal en Contra",
               ejeY="Puntos",
               color1="orange",
               color2="yellow",
               color3="green",
               color4="red")


player_plot <- list()
player_plot[[1]] <- p
player_plot[[2]] <- w


grid.arrange(grobs = c(player_plot), ncol = 2, as.table = FALSE)



################
# Ejercicio 9.4
################


# a favor
equipos1 <- equipos %>%
  select(code1,PPDAF,DCF)
# en contra
equipos2 <- equipos %>%
  select(code1,PPDAC,DCC)

p<-Ejercicio92(df=equipos1,
               tam=0.04,
               titulo="Análisis PPDA y DC a favor",
               subtitulo="Liga Espa?ola temporada 2020",
               cuadrante1="Equipos con poco PPDA en campo del rival y pocos pases zona finalización",
               cuadrante2="Equipos con mucho PPDA en campo del rival pero pocos pases zona finalización",
               cuadrante3="Equipos con mucho PPDA en campo del rival y muchos pases zona finalización",
               cuadrante4="Equipos con poco PPDA en campo del rival pero muchos pases zona finalización",
               ejeX="PPDA a Favor",
               ejeY="DC Pases a favor zona finalizaci?n",
               color1="orange",
               color2="yellow",
               color3="green",
               color4="red")


w<-Ejercicio92(df=equipos2,
               tam=0.04,
               titulo="Análisis PPDA y DC en contra",
               subtitulo="Liga Española temporada 2020",
               cuadrante1="Equipos con poco PPDA en campo propio y pocos pases zona finalización",
               cuadrante2="Equipos con mucho PPDA en campo del rival pero pocos pases zona finalización",
               cuadrante3="Equipos con mucho PPDA en campo propio y muchos pases zona finalización",
               cuadrante4="Equipos con poco PPDA en campo propio pero muchos pases zona finalización",
               ejeX="PPDA a Favor",
               ejeY="DC Pases a favor zona finalización",
               color1="green",
               color2="yellow",
               color3="red",
               color4="orange")


player_plot <- list()
player_plot[[1]] <- p
player_plot[[2]] <- w


grid.arrange(grobs = c(player_plot), ncol = 2, as.table = FALSE)




################
# Ejercicio 9.5
################

# a favor
equipos1 <- equipos %>%
  select(code1,PPDAC,xGA)
# en contra
equipos2 <- equipos %>%
  select(code1,DCC,xGA)

p<-Ejercicio92(df=equipos1,
               tam=0.04,
               titulo="Análisis PPDA y xGA",
               subtitulo="Liga Española temporada 2020",
               cuadrante1="Equipos con poco PPDA y poco xGA",
               cuadrante2="Equipos con mucho PPDA pero poco xGA",
               cuadrante3="Equipos con mucho PPDA y mucho xGA",
               cuadrante4="Equipos con poco PPDA pero  mucho xGA",
               ejeX="PPDA en Contra",
               ejeY="xGA",
               color1="green",
               color2="yellow",
               color3="red",
               color4="orange")


w<-Ejercicio92(df=equipos2,
               tam=0.04,
               titulo="Análisis DC y xGA",
               subtitulo="Liga Española temporada 2020",
               cuadrante1="Equipos con poco DC y poco xGA",
               cuadrante2="Equipos con mucho DC pero poco xGA",
               cuadrante3="Equipos con mucho DC y mucho xGA",
               cuadrante4="Equipos con poco DC pero mucho xGA",
               ejeX="DC",
               ejeY="xGA",
               color1="green",
               color2="yellow",
               color3="red",
               color4="orange")


player_plot <- list()
player_plot[[1]] <- p
player_plot[[2]] <- w


grid.arrange(grobs = c(player_plot), ncol = 2, as.table = FALSE)



################
# Ejercicio 9.6
################


US_shots_Total1 <- US_shots_Total %>%
  filter(year=='2020',result!='OwnGoal') %>%
  mutate(
    team_name=case_when(
      h_a=="h" ~ h_team,
      h_a=='a' ~ a_team)
  ) %>%
  inner_join(Teams,by=c('team_name'='Understat')) %>%
  filter(team_name=="Real Madrid")

US_shots_Total1 <- US_shots_Total1 %>%
  mutate(liga = case_when(
    Country == 'Germany' ~ 'GER_',
    Country == 'England' ~ 'ENG_',
    Country == 'Spain' ~ 'ESP_',
    Country == 'France' ~ 'FRA_',
    Country == 'Italy' ~ 'ITA_'
  ))

US_shots_Total1 <- US_shots_Total1 %>%
  mutate(
    badge= paste("./",US_shots_Total1$liga,US_shots_Total1$Code,".png",sep=''),
    Resultado=case_when(
      result == 'Goal' ~ "Gol",
      TRUE ~ "Disparo"
    )
  )

goles<-US_shots_Total1 %>%
  filter(Resultado=='Gol')

goles_np<-US_shots_Total1 %>%
  filter(situation!='Penalty')

goleador <- US_shots_Total1 %>%
  filter(Resultado=="Gol") %>%
  group_by(player) %>%
  summarise(conteo=n(),xG=sum(xG)) %>%
  arrange(-conteo)

disparador <- US_shots_Total1 %>%
  group_by(player) %>%
  summarise(conteo=n(),xG=sum(xG)) %>%
  arrange(-conteo)

colours<-c("Gol"="green","Disparo"="red")
formas<-c('MissedShots'=23,'BlockedShot'=25,'Goal'=21,'SavedShot'=24,'OwnGoal'=21,'ShotOnPost'=22)
borde<-c('MissedShots'=1,'BlockedShot'=1,'Goal'=2,'SavedShot'=1,'OwnGoal'=1,'ShotOnPost'=1)
equipos_d1 <- US_shots_Total1 %>%
  distinct(badge)



h<-createPitch_StatsBomb()

p <- h +
  geom_image(data=equipos_d1,aes(x=80,y=74,image = badge), size = 0.5,alpha=0.1) +
  geom_point(data=US_shots_Total1,aes(x=((1-Y)*80),y=(X*121),fill=xG,shape=result),size=3,alpha=0.5) +
  geom_point(data=goles,aes(x=((1-Y)*80),y=(X*121),fill=xG),color='green',shape=21,size=3,alpha=0.5,stroke=2) +
  scale_colour_manual(values = colours) +
  scale_shape_manual(values=formas) +
  scale_fill_gradient2(low=('blue'),mid=('yellow'),high=('red'),midpoint = 0.4) +
  labs(shape="Resultado") +
  #scale_stroke_manual(values=borde) +
  annotate("text", label = paste("Disparos: ",nrow(US_shots_Total1)), x = 1, y = 74, size = 5, colour = "black",hjust = 0,fontface=2)+
  annotate("text", label = paste("Goles: ",nrow(goles)), x = 1, y = 72, size = 5, colour = "black",hjust = 0,fontface=2)+
  annotate("text", label = paste("xG: ",format(sum(US_shots_Total1$xG), digits=2, nsmall=2)), x = 1, y = 70, size = 5, colour = "black",hjust = 0,fontface=2)+
  annotate("text", label = paste("NPxG: ",format(sum(goles_np$xG), digits=2, nsmall=2)), x = 1, y = 68, size = 5, colour = "black",hjust = 0,fontface=2)+
  annotate("text", label = paste("M?ximo Goleador: ",goleador$player[1]," - ","Goles: ",goleador$conteo[1]," xG: ",format(goleador$xG[1], digits=2, nsmall=2),sep=''), x = 1, y = 66, size = 5, colour = "black",hjust = 0,fontface=2)+
  annotate("text", label = paste("M?s Disparos - xG: ",disparador$player[1]," - Disparos:",disparador$conteo[1]," xG: ",format(disparador$xG[1], digits=2, nsmall=2),sep=''), x = 1, y = 64, size = 5, colour = "black",hjust = 0,fontface=2)+
  theme(legend.position="bottom",
        plot.background = element_rect(fill = "gray"))


p

################
# Ejercicio 9.7
################


p <- h +
  geom_image(data=equipos_d1,aes(x=80,y=74,image = badge), size = 0.5,alpha=0.1) +
  stat_binhex(data=US_shots_Total1,aes(x=((1-Y)*80),y=(X*121),label=..count..), geom="hex", bins=10, colour="grey",alpha=0.4) +
  ggplot2::stat_binhex(data=US_shots_Total1,aes(x=((1-Y)*80),y=(X*121),label=..count..), geom="text", bins=10, colour="dimgrey") +
  scale_fill_gradientn(colours =  c("yellow","orange","red"))+
  annotate("text", label = paste("Disparos: ",nrow(US_shots_Total1)), x = 1, y = 74, size = 5, colour = "black",hjust = 0,fontface=2)+
  annotate("text", label = paste("Goles: ",nrow(goles)), x = 1, y = 72, size = 5, colour = "black",hjust = 0,fontface=2)+
  annotate("text", label = paste("xG: ",format(sum(US_shots_Total1$xG), digits=2, nsmall=2)), x = 1, y = 70, size = 5, colour = "black",hjust = 0,fontface=2)+
  annotate("text", label = paste("NPxG: ",format(sum(goles_np$xG), digits=2, nsmall=2)), x = 1, y = 68, size = 5, colour = "black",hjust = 0,fontface=2)+
  annotate("text", label = paste("M?ximo Goleador: ",goleador$player[1]," - ","Goles: ",goleador$conteo[1]," xG: ",format(goleador$xG[1], digits=2, nsmall=2),sep=''), x = 1, y = 66, size = 5, colour = "black",hjust = 0,fontface=2)+
  annotate("text", label = paste("M?s Disparos - xG: ",disparador$player[1]," - Disparos:",disparador$conteo[1]," xG: ",format(disparador$xG[1], digits=2, nsmall=2),sep=''), x = 1, y = 64, size = 5, colour = "black",hjust = 0,fontface=2)+
  theme(legend.position="none")

p

################
# Ejercicio 9.8
################

# Aquí la clave es sacar los datos por equipo a favor y en contra


US_shots_TotalF <- US_shots_Total %>%
  filter(year=='2020',result!='OwnGoal') %>%
  mutate(
    team_name=case_when(
      h_a=="h" ~ h_team,
      h_a=='a' ~ a_team)
  ) %>%
  inner_join(Teams,by=c('team_name'='Understat'))

US_shots_TotalF <- US_shots_TotalF %>%
  filter(Country=='Spain') %>%
  mutate(liga = case_when(
    Country == 'Germany' ~ 'GER_',
    Country == 'England' ~ 'ENG_',
    Country == 'Spain' ~ 'ESP_',
    Country == 'France' ~ 'FRA_',
    Country == 'Italy' ~ 'ITA_'
  ))

US_shots_TotalF <- US_shots_TotalF %>%
  mutate(
    badge= paste(US_shots_TotalF$liga,US_shots_TotalF$Code,sep='')
  )


golesF<- US_shots_TotalF %>%
  group_by(badge) %>%
  summarise(AvgxG=mean(xG),SumxG=sum(xG),total=n())

golesF <- golesF %>%
  select(badge,SumxG,total)


US_shots_TotalC <- US_shots_Total %>%
  filter(year=='2020',result!='OwnGoal') %>%
  mutate(
    rival=case_when(
      h_a=="h" ~ a_team,
      h_a=='a' ~ h_team)
  ) %>%
  inner_join(Teams,by=c('rival'='Understat'))

US_shots_TotalC <- US_shots_TotalC %>%
  filter(Country=='Spain') %>%
  mutate(liga = case_when(
    Country == 'Germany' ~ 'GER_',
    Country == 'England' ~ 'ENG_',
    Country == 'Spain' ~ 'ESP_',
    Country == 'France' ~ 'FRA_',
    Country == 'Italy' ~ 'ITA_'
  ))

US_shots_TotalC <- US_shots_TotalC %>%
  mutate(
    badge= paste(US_shots_TotalC$liga,US_shots_TotalC$Code,sep='')
  )


golesC<- US_shots_TotalC %>%
  group_by(badge) %>%
  summarise(AvgxG=mean(xG),SumxG=sum(xG),total=n())

golesC <- golesC %>%
  select(badge,SumxG,total)




p<-Ejercicio92(df=golesF,
               tam=0.04,
               titulo="Análisis xG y Disparos realizados",
               subtitulo="Liga Española temporada 2020",
               cuadrante1="Equipos con poco xG y pocos disparos",
               cuadrante2="Equipos con poco xG y mucho disparos",
               cuadrante3="Equipos con mucho xG y muchos disparos",
               cuadrante4="Equipos con mucho xG y poco disparos",
               ejeX="xG",
               ejeY="Disparos Realizados",
               color1="red",
               color2="orange",
               color3="green",
               color4="yellow")


w<-Ejercicio92(df=golesC,
               tam=0.04,
               titulo="Análisis xG y Disparos recibidos",
               subtitulo="Liga Española temporada 2020",
               cuadrante1="Equipos con poco xGA y pocos disparos",
               cuadrante2="Equipos con poco xGA y mucho disparos",
               cuadrante3="Equipos con mucho xGA y muchos disparos",
               cuadrante4="Equipos con mucho xGA y pocos disparos",
               ejeX="xGA",
               ejeY="Disparos Recibidos",
               color1="green",
               color2="orange",
               color3="red",
               color4="yellow")


player_plot <- list()
player_plot[[1]] <- p
player_plot[[2]] <- w


grid.arrange(grobs = c(player_plot), ncol = 2, as.table = FALSE)


################
# Ejercicio 9.9
################


library(ggalt)


Ejercicio<-US_player_Total %>%
  filter(year=='2020') %>%
  inner_join(Teams,by=c('team_name'='Understat')) %>%
  filter(Country=="Spain") %>%
  arrange(desc(goals),desc(xG)) %>%
  top_n(n = 20,goals) %>%
  select(player_name,xG,goals,Country) %>%
  mutate(diferencia=round(goals-xG,1))

Ejercicio$xG<-round(Ejercicio$xG,1)

Ejercicio <- arrange(Ejercicio, desc(diferencia))
Ejercicio$player_name <- factor(Ejercicio$player_name, levels=rev(Ejercicio$player_name))


if(max(Ejercicio$xG)>max(Ejercicio$goals)){
  maximo<-max(Ejercicio$xG)
}else{
  maximo<-max(Ejercicio$goals)
}

if(min(Ejercicio$xG)<min(Ejercicio$goals)){
  minimo<-min(Ejercicio$xG)
}else{
  minimo<-min(Ejercicio$goals)
}


gg <- ggplot()

gg <- gg + geom_segment(data=Ejercicio, aes(y=player_name, yend=player_name, x=minimo-0.2, xend=maximo+0.2), linetype='dashed',color="#b2b2b2", size=0.15)

gg <- gg + geom_dumbbell(data=Ejercicio, aes(y=player_name, x=xG, xend=goals),
                         size=1.5, color="#b2b2b2", size_x=6, size_xend=6,
                         colour_x="#9fb059", colour_xend="#edae52")

gg <- gg + geom_text(data=filter(Ejercicio, player_name==Ejercicio$player_name[1]),
                     aes(x=xG, y=player_name, label="xG"),
                     color="#9fb059", size=5, vjust=-1, fontface="bold")
gg <- gg + geom_text(data=filter(Ejercicio, player_name==Ejercicio$player_name[1]),
                     aes(x=goals, y=player_name, label="Goals"),
                     color="#edae52", size=5, vjust=-1, fontface="bold")

gg <- gg + geom_text(data=Ejercicio, aes(x=xG, y=player_name, label=(xG)),
                     color="white", size=2.75, vjust=0,nudge_y = -0.1)
gg <- gg + geom_text(data=Ejercicio, color="white", size=2.75, vjust=0,nudge_y = -0.1,
                     aes(x=goals, y=player_name, label=(goals)))

gg <- gg + geom_rect(data=Ejercicio, aes(xmin=maximo+0.1, xmax=maximo+0.3, ymin=-Inf, ymax=Inf), fill="#efefe3")
gg <- gg + geom_text(data=Ejercicio, aes(label=diferencia, y=player_name, x=maximo+0.2), fontface="bold", size=3)
gg <- gg + geom_text(data=filter(Ejercicio, player_name==Ejercicio$player_name[1]), aes(x=maximo+0.2, y=player_name, label="DIFF"),
                     color="#7a7d7e", size=3.1, vjust=-2, fontface="bold")
gg <- gg + scale_x_continuous(expand=c(0,0), limits=c(minimo-0.2,maximo+0.5))
gg <- gg + scale_y_discrete(expand=c(0.075,0))
gg <- gg + labs(x=NULL, y=NULL, title="Diferencia entre xG y Goles",
                subtitle="Liga Espa?ola - Temporada 2020",
                caption="Fuente: Understat. Elaboraci?n: Jes?s Lagos. @Vdot_spain")
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

ggsave("ggxG.png", height = 10.5, width = 10.5)


################
# Ejercicio 9.10
################

totalplayer<-US_player_Total %>%
  inner_join(Teams,by=c('team_name'='Understat')) %>%
  filter(year=='2020',Country=='Spain',time>400) %>%
  mutate(Posicion=case_when(
    substr(position,1,1)==('F') ~ "Delantero",
    substr(position,1,1)==('S') ~ "Sustituto",
    substr(position,1,1)==('M') ~ "Centrocampista",
    substr(position,1,1)==('D') ~ "Defensa"
  )) %>%
  filter(Posicion=='Delantero')


totalplayer<-totalplayer %>%
  mutate(xg90=(xG/time)*90,
         sh90=(shots/time)*90,
         goals90=(goals/time)*90,
         npgoals90=(npg/time)*90,
         npxG90=(npxG/time)*90,
         npgoals90=(npg/time)*90,
         assits90=(assists/time)*90,
         xA90=(xA/time)*90,
         key_passes90=(key_passes/time)*90,
         xGChain90=(xGChain/time)*90,
         xGBuildup90=(xGBuildup/time)*90)  %>%
  arrange(desc(games)) %>%
  top_n(n = 50,games)

totalplayer <- totalplayer %>%
  select(player_name,xg90,sh90,goals90,npgoals90,npxG90,npgoals90,assits90,xA90,key_passes90,xGChain90,xGBuildup90,Country)


totalplayer<-totalplayer %>%
  select(player_name,xg90,xA90,xGChain90) %>%
  mutate(Cuadrante=case_when(
    xg90<median(xg90) & xA90<median(xA90) ~ "red",
    xg90<median(xg90) & xA90>median(xA90) ~ "orange",
    xg90>median(xg90) & xA90>median(xA90) ~ "green",
    xg90>median(xg90) & xA90<median(xA90) ~ "orange",
    
  ))

ggplot(data = totalplayer, aes(x = xg90, y = xA90,size=xGChain90,color=Cuadrante,fill=Cuadrante)) +
  
  ggtitle(label = "An?lisis de m?tricas cada 90'",
          subtitle = "Liga Espa?ola - 2020") +
  geom_label_repel(aes(label = player_name),
                   size = 2.5, hjust  = 0,nudge_x=0.01,
                   segment.color = 'grey80',fill="white") +
  geom_point(aes(alpha=xg90/1.3)) +
  scale_alpha_identity() +
  scale_color_identity() +
  #scale_size_identity() +
  xlab("xG 90'") +
  ylab("xA 90'") +
  labs(caption = "Fuente: Understat  Elaborado por: Jes?s Lagos. @Vdot_spain") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face="bold.italic", size=14),
    plot.subtitle = element_text(hjust = 0.5, face="bold.italic", size=10),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5, linetype = "solid",colour = "gray")
  )


################
# Ejercicio 9.11
################

equipos<-US_season_Total %>%
  mutate(GolesFavor=(scored),GolesContra=(missed),xPTS=(xpts),Puntos=(pts),
         DC=(deep),ODC=(deep_allowed),PPDA=(ppda.att/ppda.def),
         OPPDA=(ppda_allowed.att/ppda_allowed.def),
         Ratio_GF_xG=(scored/xG),Ratio_GC_xGA=(missed/xGA),
  ) %>%
  filter(team_name=="Real Madrid")

equipos <- equipos %>%
  select(team_name,year,xG,result)

colores<-c("w"="green","d"="orange","l"="red")

p<-ggplot(data = equipos, aes(x = as.factor(year), y = xG,text= row.names(equipos))) +
  ggtitle(label = 'Evoluci?n de xG del Real Madrid',
          subtitle = "Temporadas de 2014 a 2020") +
  geom_boxplot(outlier.colour="red", outlier.shape=NA,
               outlier.size=2,colour='gray') +
  geom_jitter(shape=16, position=position_jitter(0.2),aes(color=result)) +
  scale_color_manual(values=colores) +
  xlab("Temporada") +
  ylab('xG') +
  labs(caption = "Fuente: Understat  Elaborado por: Jesús Lagos. @Vdot_spain") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5, face="bold.italic", size=14),
        plot.subtitle = element_text(hjust = 0.5, face="bold.italic", size=10),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "gray"))
ggplotly(p)


################
# Ejercicio 9.12
################


disparos <- US_shots_Total %>%
  filter(year=='2020',result!='OwnGoal') %>%
  mutate(
    team_name=case_when(
      h_a=="h" ~ h_team,
      h_a=='a' ~ a_team)
  ) %>%
  inner_join(Teams,by=c('team_name'='Understat'))

disparos <- disparos %>%
  filter(Country=='Spain') %>%
  mutate(liga = case_when(
    Country == 'Germany' ~ 'GER_',
    Country == 'England' ~ 'ENG_',
    Country == 'Spain' ~ 'ESP_',
    Country == 'France' ~ 'FRA_',
    Country == 'Italy' ~ 'ITA_'
  ))

disparos <- disparos %>%
  mutate(
    badge= paste('./',disparos$liga,disparos$Code,".png",sep='')
  )



disparos<-disparos %>%
  mutate(Resultado=case_when(
    result == 'Goal' ~ "Gol",
    TRUE ~ "Disparo"
  ))

colours<-c("Gol"="green","Disparo"="red")

team_disparos<-disparos %>%
  distinct(team_name,badge) %>%
  arrange(team_name)

tipo_grafico<-"Hex?gonos"

plot=0
player_plot <- list()
for(x in team_disparos$team_name){
  
  equipos_d1 <- team_disparos %>%
    filter(team_name==x) %>%
    select(badge)
  
  disparos_team<-disparos %>%
    filter(team_name==x)
  
  goles<-disparos %>%
    filter(team_name==x,Resultado=='Gol')
  
  h<-createPitch_StatsBomb()
  
  # Vamos a hacerlo con hexágonos
  if(tipo_grafico=="Hex?gonos"){
    
    p<- h +
      ggtitle(label = x) +
      ggplot2::stat_binhex(data=disparos_team,aes(x=((1-Y)*80),y=(X*121),label=..count..), geom="hex", bins=10, colour="grey",alpha=0.4) +
      #ggplot2::stat_binhex(data=disparos_team,aes(x=((1-Y)*80),y=(X*121),label=..count..), geom="text", bins=10, colour="black") +
      scale_fill_gradientn(colours =  c("yellow","orange","red"))+
      geom_image(data=equipos_d1,aes(x=1,y=74,image = badge), size = 0.5,alpha=.2) +
      theme(legend.position="none") +
      theme(plot.title = element_text(hjust = 0.5))}
  
  else {
    
    
    
    colours<-c("Gol"="green","Disparo"="red")
    formas<-c('MissedShots'=23,'BlockedShot'=25,'Goal'=21,'SavedShot'=24,'OwnGoal'=21,'ShotOnPost'=22)
    borde<-c('MissedShots'=1,'BlockedShot'=1,'Goal'=2,'SavedShot'=1,'OwnGoal'=1,'ShotOnPost'=1)
    
    p <- h +
      geom_image(data=equipos_d1,aes(x=80,y=74,image = badge), size = 0.5,alpha=0.1) +
      geom_point(data=disparos_team,aes(x=((1-Y)*80),y=(X*121),fill=xG,shape=result),size=3,alpha=0.5) +
      geom_point(data=goles,aes(x=((1-Y)*80),y=(X*121),fill=xG),color='green',shape=21,size=3,alpha=0.5,stroke=2) +
      scale_colour_manual(values = colours) +
      scale_shape_manual(values=formas) +
      scale_fill_gradient2(low=('blue'),mid=('yellow'),high=('red'),midpoint = 0.4) +
      labs(shape="Resultado") +
      #scale_stroke_manual(values=borde) +
      theme(legend.position="none",
            plot.background = element_rect(fill = "gray"))
    
    
    
  }
  
  plot=plot+1
  player_plot[[plot]] <- p
  
}

grid.arrange(grobs = c(player_plot), ncol = 5, as.table = FALSE)




################
# Ejercicio 9.13
################


disparos <- US_shots_Total %>%
  filter(year=='2020') %>%
  mutate(
    team_name=case_when(
      h_a=="h" ~ h_team,
      h_a=='a' ~ a_team)
  ) %>%
  inner_join(Teams,by=c('team_name'='Understat'))

disparos <- disparos %>%
  filter(Country=='Spain') %>%
  mutate(liga = case_when(
    Country == 'Germany' ~ 'GER_',
    Country == 'England' ~ 'ENG_',
    Country == 'Spain' ~ 'ESP_',
    Country == 'France' ~ 'FRA_',
    Country == 'Italy' ~ 'ITA_'
  ))

disparos <- disparos %>%
  mutate(
    badge= paste('./',disparos$liga,disparos$Code,".png",sep='')
  )


disparos <- disparos %>%
  filter(h_team=='Atletico Madrid' | a_team=='Atletico Madrid')


disparos<-disparos %>%
  mutate(team_name2=case_when(
    team_name=='Atletico Madrid' ~ 'Atletico Madrid',
    team_name!='Atletico Madrid' ~ "Resto Equipos",
  ))


exes<-disparos %>%
  distinct(team_name2)

disparos<-disparos %>%
  mutate(team_name3=case_when(
    result=="OwnGoal" & team_name2==exes$team_name2[1] ~ exes$team_name2[2],
    result=="OwnGoal" & team_name2==exes$team_name2[2] ~ exes$team_name2[1],
    TRUE ~ team_name2
  ))

disparos<-disparos %>%
  mutate(Target = case_when(
    result == 'Goal' ~ 1,
    result == 'OwnGoal' ~ 1,
    TRUE ~ 0
  )
  )


Mapa_disparos1<-disparos %>%
  filter(team_name3==exes$team_name2[1])
Mapa_disparos2<-disparos %>%
  filter(team_name3==exes$team_name2[2])

equipo1<-first(Mapa_disparos1$team_name3)
equipo2<-first(Mapa_disparos2$team_name3)


Mapa_disparos1$Target<-as.numeric(Mapa_disparos1$Target)
Mapa_disparos2$Target<-as.numeric(Mapa_disparos2$Target)

Goles_totales1<-sum(Mapa_disparos1$xG)
Goles_reales1<-sum(Mapa_disparos1$Target)
Goles_totales2<-sum(Mapa_disparos2$xG)
Goles_reales2<-sum(Mapa_disparos2$Target)

Mapa_disparos1<-Mapa_disparos1 %>%
  mutate(resultado=case_when(
    result=='MissedShots' ~ "Fuera",
    result=='ShotOnPost' ~ "Al Palo",
    result=='SavedShot' ~ "Parado",
    result=='Goal' ~ "Gol",
    result=='BlockedShot' ~ "Bloqueado",
    result=='OwnGoal' ~ "Propia Puerta"
  ))

Mapa_disparos2<-Mapa_disparos2 %>%
  mutate(resultado=case_when(
    result=='MissedShots' ~ "Fuera",
    result=='ShotOnPost' ~ "Al Palo",
    result=='SavedShot' ~ "Parado",
    result=='Goal' ~ "Gol",
    result=='BlockedShot' ~ "Bloqueado",
    result=='OwnGoal' ~ "Propia Puerta"
  ))

Mapa_disparos2$minuto <- as.numeric(Mapa_disparos2$minute)
Mapa_disparos1$minuto <- as.numeric(Mapa_disparos1$minute)

Mapa_disparos11<-Mapa_disparos1 %>%
  select(Target,minuto,xG,player)
Mapa_disparos22<-Mapa_disparos2 %>%
  select(Target,minuto,xG,player)

anadir<-data.frame(Target=c(0,0),minuto=c(0,90),xG=c(0,0),player="")

Mapa_disparos11<-rbind(Mapa_disparos11,anadir)
Mapa_disparos22<-rbind(Mapa_disparos22,anadir)

Mapa_disparos11<-Mapa_disparos11[order(Mapa_disparos11$minuto),]
Mapa_disparos11$nuevo <- cumsum(Mapa_disparos11$xG)
Mapa_disparos11$color <- c('black')
Mapa_disparos11$equipo<-equipo1
Mapa_disparos22<-Mapa_disparos22[order(Mapa_disparos22$minuto),]
Mapa_disparos22$nuevo <- cumsum(Mapa_disparos22$xG)
Mapa_disparos22$equipo<-equipo2
Mapa_disparos22$color <- 'red'

Mapa_disparos33<-rbind(Mapa_disparos11,Mapa_disparos22)
Mapa_disparos33$label<-paste(Mapa_disparos33$player," - xG: ",format(Mapa_disparos33$xG,digits=0,nsmall=2),sep='')

OptaMAPcampofutbol <- function(){
  
  #Creamos la plantilla del tema del gráfico (esto es de soccermatics con algunos cambios)
  ##538032
  theme_blankPitch = function(size=12) {
    theme(
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.length=unit(0, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background=element_rect(fill="gray", colour=NA),
      legend.key=element_rect(colour="gray",fill="gray"),
      legend.key.size=unit(1.2, "lines"),
      legend.text=element_text(size=size),
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = "gray", fill = "gray", size = .5),
      panel.background=element_rect(fill="gray",colour="gray"),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.spacing=element_blank(),
      plot.background=element_blank(),
      plot.margin=unit(c(0, 0, 0, 0), "lines"),
      plot.title=element_text(size=size*1.2),
      strip.text.y=element_text(colour="gray",size=size,angle=270),
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
    geom_rect(aes(xmin=0, xmax=10600, ymin=0, ymax=7040), fill = "gray", colour = "#ffffff") +
    geom_rect(aes(xmin=0, xmax=TheBoxHeight[1], ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = "gray", colour = "#ffffff") +
    geom_rect(aes(xmin=TheBoxHeight[2], xmax=10600, ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = "gray", colour = "#ffffff") +
    geom_rect(aes(xmin=0, xmax=box6yardHeight[1], ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = "gray", colour = "#ffffff")  +
    geom_rect(aes(xmin=box6yardHeight[2], xmax=10600, ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = "gray", colour = "#ffffff")  +
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

h <- OptaMAPcampofutbol()
p <- h +
  geom_point(data=Mapa_disparos1,aes(x = X*10600, y=Y*7040,size=xG,color=resultado),stroke = 0) + #,color="red") +
  geom_point(data=Mapa_disparos2,aes(x = 10600-X*10600, y=7040-Y*7040,size=xG,color=resultado),stroke = 0) + #,color="blue") +
  annotate(geom="text", x=7950, y=3500, label=format(Goles_totales1,digits=2,nsmall=2), color="white",size=30,alpha=0.5) +
  annotate(geom="text", x=7950, y=6500, label=paste(equipo1," (",Goles_reales1,")"), color="white",size=8) +
  annotate(geom="text", x=2650, y=3500, label=format(Goles_totales2,digits=2,nsmall=2), color="white",size=30,alpha=0.5) +
  annotate(geom="text", x=2650, y=6500, label=paste(equipo2," (",Goles_reales2,")"), color="white",size=8)+
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(legend.title = element_blank(),
        legend.position=c(0.05,0.5),
        legend.box = "vertical",
        legend.text = element_text( size=8,
                                    face="bold"))


w <- ggplot() +
  geom_step(data=Mapa_disparos33, aes(x=(minuto), y=nuevo,color=color,group=color),size=2)+
  geom_point(data=subset(Mapa_disparos33,Target=='1'),aes(x = minuto, y=nuevo,color=color),show.legend = FALSE,
             size = 5, shape = 21, fill = "white", stroke = 1.25) +
  scale_color_identity(labels = c(equipo1,equipo2),
                       guide = "legend")+
  geom_label_repel(data = Mapa_disparos33 %>% filter(Target == 1),
                   aes(x = minuto, y = nuevo,
                       color = color, label = label),
                   nudge_x = 6, nudge_y = 0.15,
                   show.legend = FALSE,size=3) +
  labs(title = paste("Evoluci?n xG del ",'Atl?tico de Madrid'," y sus rivales",sep=''),
       #subtitle = "Acumulado de xG",
       x = "Minutos",
       y = "Expected Goals") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 26),
    plot.subtitle = element_text(size = 18,
                                 color = "grey20"),
    axis.title = element_text(size = 18, color = "grey20"),
    axis.text = element_text(size = 16, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = c(0.2, 0.95),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    plot.background = element_rect(colour = "gray"))

player_plot <- list()
player_plot[[1]] <- p
player_plot[[2]] <- w

k<-grid.arrange(grobs = c(player_plot), ncol = 2, as.table = FALSE)
ggsave("atleti.png",k, width = 50, height = 20, units = "cm")

w
################
# Ejercicio 9.14
################

Faltas_T<-readRDS('US_faltas_Total.RData')

Faltas_jugadores_total <- Faltas_T %>%
  group_by(player) %>%
  summarise(conteo=n())  %>%
  filter(conteo>= 100) %>%
  ungroup()

Faltas_T1 <- Faltas_T %>%
  inner_join(Faltas_jugadores_total,by=c('player')) %>%
  filter(player=='Lionel Messi')

library(plotly)

h<-OptaMAPcampofutbol()

p<-h + geom_point(data=Faltas_T1,aes(x=(X*10800),y=(Y*7040),color=result,shape=result,
                                     text = paste(
                                       "Jugador: ", player,
                                       "\nxG: ", xG,
                                       "\nPartido: ", paste(h_team," ",h_goals,"-",a_goals," ",a_team,sep=""),
                                       "\nFecha: ", date,
                                       "\nResultado: ", result)
),size=3)

k<-ggplotly(p,tooltip = "text") %>%
  layout(legend = list( x = 0.13, y =0.9,fontsize=2))
k

# tabla

Faltas12<-Faltas_T %>%
  mutate(resultado=case_when(
    result=='Goal'~ 1,
    TRUE ~ 0
  )) %>%
  group_by(player) %>%
  summarise(Goles=sum(resultado),Lanzamientos=n())

Faltas12 <- Faltas12 %>%
  mutate(Porcentaje=format(round((Goles/Lanzamientos)*100,2))) %>%
  arrange(desc(Porcentaje))

library(DT)

datatable(Faltas12)


# Ejercicio 9.15 Media M?vil

equipos<- US_shots_Total %>%
  filter(result!='OwnGoal',player=='Lionel Messi') %>%
  mutate(Resultado=case_when(
    result == 'Goal' ~ "Gol",
    TRUE ~ "Disparo"
  )) %>%
  group_by(player,date,Resultado) %>%
  summarise(xG=sum(xG),conteo=n()) %>%
  ungroup()

equipos$conteo<-as.numeric(equipos$conteo)

equipos<-equipos %>%
  mutate(conteo=case_when(
    Resultado=="Disparo" ~ 0,
    TRUE ~ conteo
  ))

equipos<-equipos %>%
  group_by(player,date) %>%
  summarise(xG=sum(xG),goles=sum(conteo))

metrica1<-'Goles a Favor'
metrica2<-'xG a Favor'


names(equipos) <- c('team_name','fecha','m2','m1')

library(zoo)

savings <- equipos %>%
  mutate(m5partidosm1 = rollmean(m1, k = 5, fill = NA),
         m5partidosm2 = rollmean(m2, k = 5, fill = NA),
         m5partidosm3 = rollmean(m1, k = 10, fill = NA),
         m5partidosm4 = rollmean(m2, k = 10, fill = NA))


partidosj=10

if(partidosj==5){
  savings1<-savings %>%
    ungroup() %>%
    select(fecha,m5partidosm1,m5partidosm2)
  savings<-savings %>%
    ungroup() %>%
    select(fecha,m5partidosm1,m5partidosm2)
  
}else{
  savings1<-savings %>%
    ungroup() %>%
    select(fecha,m5partidosm3,m5partidosm4)
  savings<-savings %>%
    ungroup() %>%
    select(fecha,m5partidosm3,m5partidosm4)
  
}

names(savings1)<-c('fecha','m1','m2')
names(savings)<-c('fecha','m1','m2')


covid<-data.frame(league_name=c('RFPL',
                                'Bundesliga',
                                'La_liga',
                                'EPL',
                                'Serie_A',
                                'Ligue_1'),
                  covid_start=c('16/03/2020',
                                '11/03/2020',
                                '10/03/2020',
                                '09/03/2020',
                                '09/03/2020',
                                '08/03/2020'),
                  covid_end=c('19/06/2020',
                              '16/05/2020',
                              '11/06/2020',
                              '17/06/2020',
                              '20/06/2020',
                              '21/08/2020'),
                  label=c('02/05/2020',
                          '13/04/2020',
                          '25/04/2020',
                          '28/04/2020',
                          '29/04/2020',
                          '30/05/2020'
                  ))

covid$covid_start<-as.Date(covid$covid_start,"%d/%m/%Y")
covid$covid_end<-as.Date(covid$covid_end,"%d/%m/%Y")
covid$label<-as.Date(covid$label,"%d/%m/%Y")

covid2<-US_season_Total %>%
  filter(team_name=='Barcelona') %>%
  distinct(league_name) %>%
  inner_join(covid,by=c('league_name'))

savings1 <- cbind(savings1,min_line=pmin(savings1$m1,savings1$m2))

savings1 <- reshape2::melt(savings1, id.vars=c("fecha","min_line"), variable.name="Metricas", value.name="valores")

subtitulo= paste("<b>Comparaci?n entre  <span style = 'color:darkgreen;'>**",metrica1,"**</span> y  <span style = 'color:red;'>**",metrica2,"** </span>",sep="")

library(ggtext)
ggplot() +
  geom_ribbon(data=savings1,aes(ymax=valores, ymin=min_line,x=fecha, fill=Metricas),alpha=0.5) +
  scale_fill_manual(values=c(m1="green", m2="red"),name = "M?tricas", labels = c("En Positivo", "En Negativo")) +
  geom_line(data=savings,aes(x=fecha,y=m1),color='darkgreen',size=1) +
  geom_smooth(data=savings,aes(x=fecha,y=m1),color='darkgreen',size=1,se = FALSE,alpha=0.5,linetype='dashed') +
  geom_line(data=savings,aes(x=fecha,y=m2),color='red',size=1) +
  geom_smooth(data=savings,aes(x=fecha,y=m2),color='red',size=1,se = FALSE,alpha=0.5,linetype='dashed') +
  geom_point(data=savings,aes(x=fecha,y=m1),color='darkgreen',size=4,fill='white',shape=21,stroke=1.2) +
  geom_smooth(data=savings,aes(x=fecha,y=m1),color='darkgreen',size=1,se = FALSE,alpha=0.5,linetype='dashed') +
  geom_point(data=savings,aes(x=fecha,y=m2),color='red',size=4,fill='white',shape=21,stroke=1.2) +
  geom_rect(data=covid2, aes(xmin=covid_start, xmax=covid_end, ymin=-Inf, ymax=Inf),alpha=0.7, fill="#efefe3")+
  geom_text(mapping = aes(x = covid_start,
                          y = Inf,
                          
                          label = 'Covid-19',
                          hjust = 0,
                          vjust = 1,
                          angle = 0),
            data = covid2,size = 3,color='black')+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  labs(x='Fecha de partidos',y='Evoluci?n de m?trica',
       title=paste("Evoluci?n comparativa de la media m?vil ",partidosj," partidos de Lionel Messi",sep=''),
       # subtitle = paste('Comparaci?n entre ',metrica1,' y ',metrica2,sep=''),
       subtitle=subtitulo,
       caption = "Fuente: Understat.com  Elaborado por Jes?s Lagos @Vdot_Spain") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.subtitle = element_markdown())

