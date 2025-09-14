# Este código ha sido elaborado por Alfonso, alumno del curso.
# Yo luego he hecho algunos retoques y añadido algunas cosas que os pueden ser útiles
# Vamos a usar los datos de fbref para su cálculo.


# librerias
library(tidyverse)
library(ggplot2)
library(ggrepel)

# Directorio de trabajo y carga de información
setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Fbref")

# Vamos a usar los datos del scrapeo de jugadores de fbref.
# si recordáis bajabamos 10 ficheros de datos de jugadores
# ¿podemos evitar tener que abrir los 10 ficheros uno a uno? Si.
# Vamos a meter en una lista los nombres de los ficheros que queremos
# para eso usamos list.files.
# la clave es hacer el pattern bien. Los ficheros deben estar en la ruta del directorio de trabajo.
# si estan en otra ruta hay que añadir un parámetro path="la ruta" indicando la ruta
# para hacer el pattern bien podéis usar esta función
# en windows podemos poner algo sencillo como "Fbref_Players*.rds" pero eso R no lo entiende.
# usando esta función glob2rx("Fbref_Players*.rds") lo traduce a  ^Fbref_Players.*\\.rds$" que es lo que entiende R
all.files <- list.files(pattern="^Fbref_Players.*\\.rds$", full.names=TRUE)

# ahora vamos a juntarlos
for (file in all.files){
  # leemos el fichero
  datos<-readRDS(file)
  # primero le decimos si existe un df, como no existe lo creamos, cuando exista ya hacemos el left_join
  if (!exists("df_fbref")){
    df_fbref <- datos
  } else {
    df_fbref <- df_fbref %>%
      left_join(datos,by=c(".1Player",".4Squad","season"))
  }
}

# Ya tenemos en df_fbref los datos de 12122 jugadores (temporada y equipo) y 303 columnas...de locos...;)
# Vamos a seleccionar las variables a representar. Esto ya cada uno que pinte las que quiera por ejemplo en función
# del jugador.

df_pintar<-df_fbref %>%
  select(2,3,4,5,7,33,35,8,9,11,15,16,18,21,28,128,131,140,141,143,153,157,158,161,164,170,171,172,218,220,223,230,
         233,234,251,252,259,264)

# Cambiamos los nombres
# para saber los nombres actuales podéis poner colnames(df_pintar) en la consola y se os pintaran los nombres de los campos
names(df_pintar)<-c("Jugador","Nacionalidad","Posicion","Equipo","Nacimiento","Liga","Temporada","Partidos","Tackles","Tackles Primer Tercio",
                    "Intento Regateado",
                    "% Regateado","Presiones","Presiones Primer Tercio","Intercepciones","Tarjetas Amarillas","Faltas",
                    "Recuperaciones","Duelos Aéreos Ganados","% Duelos Aéreos Ganados","Pases Completados","Distancia Pases Progresivos",
                    "Pases Cortos (<14m)","Pases Cercanos (<27m)","Pases Largos (>27m)","Pases Clave","Pases Último Tercio",
                    "Pases al Area","Contactos balón","Contactos balón Primer Tercio","Contactos Área Rival",
                    "Conducciones","Conducciones Progresivas","Conducciones Último Tercio","Goles","Tiros","Distancia Media Tiro","npxG")

# Siempre os digo que reviseis la tabla, su estructura
str(df_pintar)
# ¿qué ocurre? los campos están en formato chr. Tenemos que convertirlo a numerico.
# Para no tener que copiar todas las columnas vamos a crear una función que mira que toda la columna tenga valores convertibles
# a numericos. si es posible la convierte.

is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

# Al escrapear a veces los nulos los trae en blanco
# vamos a pasarlos a 0

df_pintar[df_pintar==""]<-0

df_pintar<- df_pintar %>%
  mutate_if(is_all_numeric,as.numeric)

# Vamos a seleccionar una temporada, la 2020/2021 y al menos 15 partidos y posición defensa (468 jugadores)
df_pintar <- df_pintar %>%
  filter(Temporada=='2020/2021',Partidos>15)


# Ya tenemos el df pero ahora tenemos que convertir las métricas a 90'. Recordad que también podéis normalizar
# por posesión ajustada. Si cargais la tabla de equipos viene un campo posesión. Podéis cruzar por equipo y temporada y
# tendriais la posesión para cada jugador para los calculos.
# así podemos pasarle un mutate con una función a las columnas que queramos. He quitado las columnas % que no tiene sentido
# normalizarlas por partido

df_pintar2  <- df_pintar %>%
  mutate_at(.funs = list(~./Partidos), .vars = c(9:11,13:19,21:38))

# Ahora voy a crear un par de métricas, para que tengáis de todo un poco
df_pintar2$`Presiones 2 y 3 tercio`<-df_pintar2$`Presiones`-df_pintar2$`Presiones Primer Tercio`
df_pintar2$`Contactos balón 2 y 3 Tercio`<-df_pintar2$`Contactos balón`-df_pintar2$`Contactos balón Primer Tercio`

# Seleccionamos a un jugador

Jugador_Seleccionado <-df_pintar2 %>%
  filter(Jugador=="Mohamed Salah")
Jugador_Seleccionado <- reshape2::melt(Jugador_Seleccionado,id.vars=c(1:8))


Jugadores_Todos <- reshape2::melt(df_pintar2,id.vars=c(1:8))


# Creamos los percentiles

Jugadores_Todos <- Jugadores_Todos %>%
  group_by(variable)%>%
  mutate(Quartile=ntile(value,100),
         RangoQuartil=case_when(Quartile>=0 & Quartile<=19~"0-19%",
                                Quartile>=20 & Quartile<=39~"20-39%",
                                Quartile>=40& Quartile<= 59~"40-59%",
                                Quartile>=60&Quartile<=79~"60-79%",
                                TRUE~"80-100%"),
         Ranking=min_rank(-value)) %>%
  ungroup()

nTotal_Jugadores<-Jugadores_Todos %>%
  distinct(Jugador) %>%
  nrow()

Pintar_Jugador <-Jugadores_Todos %>%
  filter(Jugador=="Mohamed Salah")

Pintar_Jugador$value<-round(Pintar_Jugador$value,2)

# Vamos a crear un df auxiliar para categorizar las métricas
categoria=data.frame(variable=c('Tackles',
                                'Tackles Primer Tercio',
                                'Intento Regateado',
                                '% Regateado',
                                'Presiones',
                                'Presiones Primer Tercio',
                                'Intercepciones',
                                'Tarjetas Amarillas',
                                'Faltas',
                                'Recuperaciones',
                                'Duelos Aéreos Ganados',
                                '% Duelos Aéreos Ganados',
                                'Pases Completados',
                                'Distancia Pases Progresivos',
                                'Pases Cortos (<14m)',
                                'Pases Cercanos (<27m)',
                                'Pases Largos (>27m)',
                                'Pases Clave',
                                'Pases Último Tercio',
                                'Pases al Area',
                                'Contactos balón',
                                'Contactos balón Primer Tercio',
                                'Contactos Área Rival',
                                'Conducciones',
                                'Conducciones Progresivas',
                                'Conducciones Último Tercio',
                                'Goles',
                                'Tiros',
                                'Distancia Media Tiro',
                                'npxG',
                                'Presiones 2 y 3 tercio',
                                'Contactos balón 2 y 3 Tercio'),
                     tipo=c('Defensa',
                            'Defensa',
                            'Defensa',
                            'Defensa',
                            'Defensa',
                            'Defensa',
                            'Defensa',
                            'Defensa',
                            'Defensa',
                            'Defensa',
                            'Defensa',
                            'Defensa',
                            'Creación',
                            'Creación',
                            'Creación',
                            'Creación',
                            'Creación',
                            'Creación',
                            'Creación',
                            'Creación',
                            'Posesión',
                            'Posesión',
                            'Posesión',
                            'Posesión',
                            'Posesión',
                            'Posesión',
                            'Finalización',
                            'Finalización',
                            'Finalización',
                            'Finalización',
                            'Defensa',
                            'Posesión'))


Pintar_Jugador<-Pintar_Jugador %>%
  inner_join(categoria,by=c('variable'))


coloresrango<-c("0-19%"="red3","20-39%"="orange","40-59%"="yellow2","60-79%"="yellowgreen","80-100%"="green3")
titulo <-paste("**",Pintar_Jugador$Jugador[1], "** (",Pintar_Jugador$Nacimiento[1],")",sep="")
subtitulo <- paste(Pintar_Jugador$Equipo[1]," - ","Premier League (",Pintar_Jugador$Temporada[1],") -  min",sep="")


p<-ggplot(Pintar_Jugador, aes(x=Quartile, y=variable,group=tipo, color=RangoQuartil))+
  geom_segment(aes(yend=variable),xend=0, size=2)+
  geom_point(size=3.5)+
  scale_colour_manual(values = coloresrango)+
  ggrepel::geom_text_repel(
    aes(label=value),
    nudge_x = 0.3,
    size=3,
    color="black",
    fontface="bold",
    hjust = -1, vjust = 0.5,
    min.segment.length = 3,
    max.overlaps=Inf
    ) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks=c(0,10,20,30,40,50,60,70,80,90,100),
    labels=c("p0","p10","p20","p30","p40","p50","p60","p70","p80","p90","p100")
    )+
  scale_y_discrete(expand=c(0.09,0)) +
  annotate("rect", fill = "white", alpha = 0.5, color="white",
          xmin = 110, xmax = 119,
          ymin = -Inf, ymax =Inf )+
  geom_text(aes(label=paste("#",Ranking," ","(","P",Quartile,")", sep = ""), y=variable,x=114),
           fontface="bold.italic", size=3, color="black")+
  geom_text(aes(label=paste("\n",nTotal_Jugadores, "Atacantes Big 5 Europa"), y=Inf,x=114), fontface="italic", color="grey",
            vjust=1,size=3,hjust=0.8) +
  labs(title =titulo,
       subtitle = subtitulo,
       x="",
       y="",
       caption = c("Datos por 90'", "Estilo @driblab_es elaborado por @emrangel tomado de @Vdot_Spain")) +
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = ggtext::element_markdown(size = 16, hjust = 0.5, colour = "black"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "black"),
        panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = "bottom",
        plot.margin = unit(c(2,2,2,2), "cm"),
        plot.caption = element_text(hjust=c(0, 1),size=8))

player_plot=list()
player_plot[[1]]<-p



g3<-gridExtra::grid.arrange(grobs = c(player_plot), ncol = 1, as.table = FALSE,
                            top = grid::textGrob("Mohamed Salah \nRendimiento 2020/2021",
                                                 gp=grid::gpar(fontsize=20,fontface="bold",col="black")))


h<-cowplot::ggdraw(g3) +
  cowplot::draw_image("Salah.jfif",
                      x = -0.17, y = 0.45,
                      scale=0.1
                      ) +
  cowplot::draw_image("Liverpool.png",
                      x = -0.15, y = 0.4,
                      scale=0.05
  ) +
  theme(plot.background = element_rect(fill="white", color = NA))

ggsave("grafico_cooper1.png", plot = h, width = 35, height = 40, units = "cm")
