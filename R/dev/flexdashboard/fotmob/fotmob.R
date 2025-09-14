library('rvest')
library('dplyr')
library(ggplot2)
require("ggrepel")
require("jsonlite")

#V Vamos a bajarnos los datos estadísticos de los partidos de fotmob. Pero necesitamos cada id de partido primero. Vamos para allá.

# Declaramos nuestro directorio de trabajo
mainDir <- "C:/Users/jlagosmi/Desktop/MASTER BIG DATA/PAQUETE_R/UnderStatPlots/data/wyscout/Scout_Tool"
setwd(file.path(mainDir))

# Lo primero es saber las urls de las peticiones que usa la web para cargar la información
# En herramientas para el desarrollador de Chrome nos ponemos en la pestaña de Network
# Cuando le damos en la web a Leagues y seleccionamos una vemos como en las herramientas nos aparecen muchas cosas
# pero una de esllas son estas urls cada vez que apretamos en cada liga
# yo las meto en un tibble
# Si copiais una dirección de estas en el navegador veréis que se abre un json con toda la info!
ligas<-tibble(liga=
                c(
                  'https://www.fotmob.com/leagues?id=54&tab=overview&type=league&ccode3=ESP&timeZone=Europe%2FMadrid&seo=1.-bundesliga',
                  'https://www.fotmob.com/leagues?id=47&tab=overview&type=league&ccode3=ESP&timeZone=Europe%2FMadrid&seo=premier-league',
                  'https://www.fotmob.com/leagues?id=87&tab=overview&type=league&ccode3=ESP&timeZone=Europe%2FMadrid&seo=laliga',
                  'https://www.fotmob.com/leagues?id=55&tab=overview&type=league&ccode3=ESP&timeZone=Europe%2FMadrid&seo=serie-a',
                  'https://www.fotmob.com/leagues?id=53&tab=overview&type=league&ccode3=ESP&timeZone=Europe%2FMadrid&seo=ligue-1'))

# Vamos a hacer una prueba con una y luego metemos un bucle para todas.
# como se descarga un json podemos usar la función fromJSON

rm(list = ls(all = T))

tt= "https://www.fotmob.com/_next/data/4276/en/leagues/274/overview/primera-apertura.json?id=274&tab=overview&slug=primera-apertura"
# test<-fromJSON('https://www.fotmob.com/api/leagues?id=47&ccode3=COL')
# test<-fromJSON('https://www.fotmob.com/api/matches?')
# test<-fromJSON(paste0('https://www.fotmob.com/api/leagues?id=47'))
test<-fromJSON(tt)

# obj1<-test[[1]]
# dd = as.data.frame(obj1)
# dd[dd$id=="47",]
# test<-fromJSON(paste0('https://www.fotmob.com/api/leagues?id=,',dd %>% filter(id=="47") %>% select(id)))
# Obtenemos una lista con 11 objetos
length(test)

# El primer objeto lo podemos llamar así por su posición o por su nombre
# Este objeto tiene simplemente los nombres de las pestañas. Nada importante.
obj1<-test[[1]]
obj1 %>% view()
# dd = as.data.frame(obj1)
# obj1 %>% view()
obj1<-obj1[["matches"]]
obj1<-test["matches"]

# El segundo objeto nos genera otra lista de 5 objetos. Los vamos a pasar a un dataframe. Veremos que tiene elnombre la liga, el id, etc.
obj2<-test[["details"]]
obj2<-do.call(data.frame,obj2)

# Nos vamos al último objeto que es el que nos interesa. El listado de partidos con su id que usaremos luego para bajarnos los datos
obj11<-test[["fixtures"]]
obj11<-do.call(data.frame,obj11)
# Si os fijais ha creado muchas columnas raras. Nos quedamos con las 11 primeras que nos interesan
obj11<-obj11 %>%
  select(1:11)
# Y finalmente tenemos el listado de partidos de la busndesliga con lo más importante, el id!

#Ahora vamos a crear el bucle para recorrer las 5 grandes ligas.
# Creamos un contador 
k=1
# empezamos el bucle con un for y vamos a meter en la variable h cada url de cada liga en cada pasada.
for(h in ligas$liga){
  
  # siempre pongo un print para ver por donde va
  print(h)
  # descargamos la url
  gg<-fromJSON(h)
  # apuntamos a los partdos
  fixtures<-gg[["fixtures"]]
  fixtures<-do.call(data.frame,fixtures)
  fix<-fixtures %>%
    select(1:11)
  # Quitamos los partidos que no se han jugado
  fix<-fix %>%
    filter(!is.na(home.score))

  # Vamos agregando los resultados de cada pasada. 
  if(k==1){
    fix_total<-fix}
  else{
    fix_total<-rbind(fix_total,fix)
    }
  k=k+1
  }

# Ahora toca sacar los datos de cada partido
# La url donde vienen los datos es como sigue:
# https://www.fotmob.com/matchDetails?matchId= y le ponemos el id que tenemos del bucle anterior.
# Vamos a ver que hay dentro antes de lanzar el bucle
test2<-fromJSON('https://www.fotmob.com/matchDetails?matchId=3424065')
# Hay un objeto content que tiene toda la info y que genera una lista de 7 elementos. Podéis bucear para ver que contiene que son muchas cosas
# de content vamos a quedarnos con varios elementos
# Aquí tenemos el nombre de los equipos del partido. Luego veremos para que lo usamos
test2_content_equipos<-test2[["content"]][["liveticker"]][["teams"]]
# Aquí tenemos las estadísticas del partido que genera una nueva lista con 7 elementos
test2_content_stats<-test2[["content"]][["stats"]][["stats"]][["stats"]]
# Vamos a pegarlos todos juntos
test2_content_stats<-do.call(rbind,test2_content_stats)
# Vemos que hay 51 estadísticas por partido
# Para este ejercicio nos quedamos con 2, EXPECTED GOALS (xG)	y 	xG on target (xGOT) que están incluidas en el elemento 3
# Así podemos hacer
test2_content_stats_xg<-test2[["content"]][["stats"]][["stats"]][["stats"]][[3]]
# Si nos fijamos los datos están en este formato 	c("0.58", "0.93")
# tenemos que separarlos en dos columnas. Podemos limpiarlo así:
test2_content_stats_xg$stats <- gsub("c\\(","", test2_content_stats_xg$stats)
test2_content_stats_xg$stats <- gsub("\\)","", test2_content_stats_xg$stats)
test2_content_stats_xg$stats <- gsub(", ",",", test2_content_stats_xg$stats)
test2_content_stats_xg$stats <- gsub('\\"',"", test2_content_stats_xg$stats)
# ahora está en este formato 0.58,0.93	
# ya podemos separarlo en dos columnas a partir de la coma. 
# Pero debemos dar un nombre a las columnas. Para eso vamos a usar el nombre de los equipos que vimos antes
equipos<-tibble(test2_content_equipos)
# Se nos ha quedado un tibble de 2 registros, donde el primero es el local y el segundo el visitante
local<-as.character(equipos[1,1])
visitante<-as.character(equipos[2,1])
# Y ahora separamos
test2_content_stats_xg <- test2_content_stats_xg %>%
  tidyr::separate(stats, c(local[1], visitante[1]), sep = ",")
# Ya tenemos cada equipo en formato wide pero ya sabéis que para pintarlo en ggplot necesitamos tenerlo en long
# si ademas vemos la estructura están formato character. Los pasamos a numeric
# Como luego en el bucle los equipos cambian podemos apuntar a las columnas por su posición no por su nombre
test2_content_stats_xg[,2]<-as.numeric(test2_content_stats_xg[,2])
test2_content_stats_xg[,3]<-as.numeric(test2_content_stats_xg[,3])
# y convertimos a formato long
test2_content_stats_xg<-reshape2::melt(test2_content_stats_xg)
# Ahora ya podriamos meter debajo de cada uno cada elemento del bucle


# Ahora tenemos que sacar un bucle para recorrer todos los partidos. LLevará lo suyo
h=1
for(x in fix_total$id){
  url<-paste('https://www.fotmob.com/matchDetails?matchId=',x,sep="")
  print(url)
  gg<-fromJSON(url)
  if(length(gg[["content"]][["liveticker"]])>1){
    print(length(gg[["content"]][["liveticker"]]))
  equipos<-tibble(gg[["content"]][["liveticker"]][["teams"]])
  local<-as.character(equipos[1,1])
  visitante<-as.character(equipos[2,1])
  stats<-(gg[["content"]][["stats"]][["stats"]][["stats"]])
  xg<-gg[["content"]][["stats"]][["stats"]][["stats"]][[3]]
  xg$stats <- gsub("c\\(","", xg$stats)
  xg$stats <- gsub("\\)","", xg$stats)
  xg$stats <- gsub(", ",",", xg$stats)
  xg$stats <- gsub('\\"',"", xg$stats)
  xg <- xg %>%
    tidyr::separate(stats, c(local[1], visitante[1]), sep = ",")
  xg[,2]<-as.numeric(xg[,2])
  xg[,3]<-as.numeric(xg[,3])
  xg<-reshape2::melt(xg)
  
  if(h==1){
    xg_total<-xg
  }else{
    xg_total<-rbind(xg_total,xg)
  }
  h=h+1
  }
}

# Ya tenemos el fichero final! Ahora vamos a quedarnos con lo que nos hace falta
# Nos quedamos con los campos que queremos (podemos poner el nombre o apuntar con su posición en el select)
# filtramos los valores que nos interesan
# y creamos el agrupado y calculamos el total de ambos valores

xG_filtrado<-xg_total %>%
  select(1,4,5) %>%
  filter(title %in% c('EXPECTED GOALS (xG)','xG on target (xGOT)')) %>%
  group_by(title,variable) %>%
  summarise(total=sum(value)) %>%
  ungroup()


# Ahora tenemos que pasar a formato wide. para tener ambas variables de xG en cada columna
# Recordad que se hace con cast. le estamos diciendo
# ponme en wide a partir xG_filtrado, dejando como identificador title (el equipo) y metiendo variable en cada campo sumando el valor numérico 
# de la tabla, en este caso total.
xG_wide<-reshape::cast(xG_filtrado,variable~title,sum)
#le cambiamos los nombres
names(xG_wide)<-c("team","xG","xGOT")
#xG_wide<-do.call(data.frame, xG_wide)
# El campo team nos lo deja como factor. Lo podemos pasar a character
xG_wide$team<-as.character(xG_wide$team)
# Creamos un campo que sea el ratio entre xGOT/xG. Si es >1 será que tienen más xGOT que xG
xG_wide$DifxG<-xG_wide[,3]/xG_wide[,2]



# A pintar



ggplot(xG_wide,aes(x=xG,y=xGOT)) +
  ggtitle("Análisis de xG y xGOT",
          subtitle = "5 Grandes Ligas Europeas hasta el 18/5/2021") +
  # Vamos a pintar los puntos a partir del ratio. 
  geom_point(aes(color=DifxG),size=3) + # color="chartreuse4"
  # Con esta escala le decimos que los pinte de rojo a verde pasando por el nananja, siendo el naranja el valor 0.95 
  scale_color_gradient2(midpoint=0.95, low="red", mid="orange",high="green" ) +
  # Así pintamos segmentos que indiquen diferentes ratios
  geom_segment(aes(x = 0, y = 0, xend = 100, yend = 60),linetype="dashed",size=0.5,color="gray50") +
  geom_segment(aes(x = 0, y = 0, xend = 100, yend = 70),linetype="dashed",size=0.5,color="gray50") +
  geom_segment(aes(x = 0, y = 0, xend = 100, yend = 80),linetype="dashed",size=0.5,color="gray50") +
  geom_segment(aes(x = 0, y = 0, xend = 100, yend = 90),linetype="dashed",size=0.5,color="gray50") +
  geom_segment(aes(x = 0, y = 0, xend = 100, yend = 100),linetype="dashed",size=0.5,color="gray50") +
  geom_segment(aes(x = 0, y = 0, xend = 100, yend = 110),linetype="dashed",size=0.5,color="gray50") +
  geom_segment(aes(x = 0, y = 0, xend = 100, yend = 120),linetype="dashed",size=0.5,color="gray50") +
  # No vamos a etiquetar todos. Solo vamos a hacerlo con los equipos que tengan un ratio por encima de 1.07 y por debajo de 0.88
  # Los pinto por separado para indicar que el nudge_y sea positivo (hacía arriba) y el otro hacia abajo con  valor negativo
  geom_text_repel(
    data=subset(x=xG_wide,subset = DifxG>1.07),
    aes(label=team),
    fontface="bold",
    size=3,
    box.padding = 0.5,
    point.padding = 1,
    segment.color = "black",
    max.overlaps = Inf,
    segment.curvature = -0.5,
    segment.ncp = 3,
    segment.angle = 40,
    segment.color="black",
    segment.size=1,
    segment.alpha=0.7,
    nudge_y      = 10
    
  )+
  
  
  geom_text_repel(
    data=subset(x=xG_wide,subset =  DifxG<0.88),
    aes(label=team),
    fontface="bold",
    point.padding = 1,
    size=3,
    box.padding = 0.5,
    segment.color = "black",
    max.overlaps = Inf,
    segment.curvature = -0.5,
    segment.ncp = 8,
    segment.angle = 20,
    segment.color="black",
    segment.size=0.6,
    segment.alpha=0.7,
    nudge_y      = -15,
    nudge_x      = 5
    
  )+
  # escribimos las etiquetas de las lineas de los ratios. Le pongo y=61...para que esté un poco más arriba 
  annotate(geom="text",x=Inf,y=61,label="60%",size=3,hjust=1,fontface="bold")+
  annotate(geom="text",x=Inf,y=71,label="70%",size=3,hjust=1,fontface="bold")+
  annotate(geom="text",x=Inf,y=81,label="80%",size=3,hjust=1,fontface="bold")+
  annotate(geom="text",x=Inf,y=91,label="90%",size=3,hjust=1,fontface="bold")+
  annotate(geom="text",x=Inf,y=101,label="100%",size=3,hjust=1,fontface="bold")+
  annotate(geom="text",x=Inf,y=111,label="110%",size=3,hjust=1,fontface="bold")+
  annotate(geom="text",x=Inf,y=121,label="120%",size=3,hjust=1,fontface="bold")+
  xlim(0,100)+
  xlab("xG Totales") +
  ylab("xGOT Totales") +
  labs(caption = "Elaborado por Jesús Lagos. Fuente: @Fotmob. Estilo: @Besoccer") +
  theme_classic(base_family = "Ubuntu Condensed") +
  theme(legend.position = "none") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_blank(),
        axis.title = element_text(face = "bold", size = 12, margin = margin(b = 10),color="chartreuse4"),
        plot.title = element_text(face = "bold", size = 18, margin = margin(b = 5),color="chartreuse4"),
        plot.subtitle = element_text(face = "bold",size = 12, margin = margin(b = 2)),
        plot.caption = element_text(size = 8, margin = margin(b = 5, t = 5), color = "#5D646F"),
        axis.text.x = element_text(size = 8, color = "#5D646F"),
        axis.text.y = element_text(size = 8, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 18, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(family = "Ubuntu Condensed", size = 12),
        legend.text.align = 0,
        panel.border = element_rect(colour = "black", fill=NA, size=1))
  
