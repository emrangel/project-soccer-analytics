# librerias
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(readxl)
library(dplyr)
library(writexl)

.folder_data_input_r <- 'R/Modulo 2/data/input/'
.folder_data_out_r <- 'R/Modulo 2/data/out/'
.folder_data_input_py <- 'Python/data/input/'
.folder_data_out_py <- 'Python/data/output/'
.folder_img_out_r <- 'R/Modulo 2/img/output/'
.folder_img_r <- 'R/Modulo 2/img/'
.ligue <- 'Ecuador. Liga Pro'
.ligue_short <- 'ECU_'
.country <- 'Ecuador'
.minutes = 500
getwd()

{

  categoria_metricas <- tribble(
    ~variable, ~nombre, ~tipo,
    "jugador", "Jugador", "General",
    "equipo", "Equipo", "General",
    "equipo_durante_el_periodo_seleccionado", "Equipo Durante El Periodo Seleccionado", "General",
    "posicion_especifica", "Posición Específica", "General",
    "edad", "Edad", "General",
    "valor_de_mercado_transfermarkt", "Valor de mercado", "General",
    "vencimiento_contrato", "Vencimiento contrato", "General",
    "partidos_jugados", "Partidos jugados", "General",
    "minutos", "Minutos", "General",
    "goles", "Goles", "Attack",
    "x_g", "xG", "Attack",
    "asistencias", "Asistencias", "Creativity",
    "x_a", "xA", "Creativity",
    "duelos_90", "Duelos / 90", "Physical",
    "duelos_ganados_percent", "Duelos ganados (%)", "Physical",
    "pais_de_nacimiento", "País de nacimiento", "General",
    "pasaporte", "Pasaporte", "General",
    "pie", "Pie dominante", "General",
    "altura", "Altura", "General",
    "peso", "Peso", "General",
    "en_prestamo", "En préstamo", "General",
    "acciones_defensivas_realizadas_90", "Acciones defensivas / 90", "Defense",
    "duelos_defensivos_90", "Duelos defensivos / 90", "Defense",
    "duelos_defensivos_ganados_percent", "Duelos defensivos ganados (%)", "Defense",
    "duelos_aereos_en_los_90", "Duelos aéreos / 90", "Defense",
    "duelos_aereos_ganados_percent", "Duelos aéreos ganados (%)", "Defense",
    "entradas_90", "Entradas / 90", "Defense",
    "posesion_conquistada_despues_de_una_entrada", "Posesión tras entrada", "Defense",
    "tiros_interceptados_90", "Tiros interceptados / 90", "Defense",
    "interceptaciones_90", "Intercepciones / 90", "Defense",
    "posesion_conquistada_despues_de_una_interceptacion", "Posesión tras intercepción", "Defense",
    "faltas_90", "Faltas / 90", "Discipline",
    "tarjetas_amarillas", "Tarjetas amarillas", "Discipline",
    "tarjetas_amarillas_90", "Tarjetas amarillas / 90", "Discipline",
    "tarjetas_rojas", "Tarjetas rojas", "Discipline",
    "tarjetas_rojas_90", "Tarjetas rojas / 90", "Discipline",
    "acciones_de_ataque_exitosas_90", "Acciones ofensivas exitosas / 90", "Attack",
    "goles_90", "Goles / 90", "Attack",
    "goles_excepto_los_penaltis", "Goles (sin penaltis)", "Attack",
    "goles_excepto_los_penaltis_90", "Goles (sin penaltis) / 90", "Attack",
    "x_g_90", "xG / 90", "Attack",
    "goles_de_cabeza", "Goles de cabeza", "Attack",
    "goles_de_cabeza_90", "Goles de cabeza / 90", "Attack",
    "remates", "Remates", "Attack",
    "remates_90", "Remates / 90", "Attack",
    "tiros_a_la_porteria_percent", "Precisión de remates (%)", "Attack",
    "goles_hechos_percent", "Tasa de conversión (%)", "Attack",
    "asistencias_90", "Asistencias / 90", "Creativity",
    "centros_90", "Centros / 90", "Attack",
    "precision_centros_percent", "Precisión centros (%)", "Attack",
    "centros_desde_la_banda_izquierda_90", "Centros izquierda / 90", "Attack",
    "precision_centros_desde_la_banda_izquierda_percent", "Precisión izquierda (%)", "Attack",
    "centros_desde_la_banda_derecha_90", "Centros derecha / 90", "Attack",
    "precision_centros_desde_la_banda_derecha_percent", "Precisión derecha (%)", "Attack",
    "centros_al_area_pequena_90", "Centros al área pequeña / 90", "Attack",
    "regates_90", "Regates / 90", "Physical",
    "regates_realizados_percent", "Éxito en regates (%)", "Physical",
    "duelos_atacantes_90", "Duelos ofensivos / 90", "Attack",
    "duelos_atacantes_ganados_percent", "Duelos ofensivos ganados (%)", "Attack",
    "toques_en_el_area_de_penalti_90", "Toques en área / 90", "Attack",
    "carreras_en_progresion_90", "Carreras progresivas / 90", "Attack",
    "aceleraciones_90", "Aceleraciones / 90", "Attack",
    "pases_recibidos_90", "Pases recibidos / 90", "Physical",
    "pases_largos_recibidos_90", "Pases largos recibidos / 90", "Physical",
    "faltas_recibidas_90", "Faltas recibidas / 90", "Discipline",
    "pases_90", "Pases / 90", "Passing",
    "precision_pases_percent", "Precisión total pases (%)", "Passing",
    "pases_hacia_adelante_90", "Pases hacia adelante / 90", "Passing",
    "precision_pases_hacia_adelante_percent", "Precisión adelante (%)", "Passing",
    "pases_hacia_atras_90", "Pases hacia atrás / 90", "Passing",
    "precision_pases_hacia_atras_percent", "Precisión atrás (%)", "Passing",
    "pases_laterales_90", "Pases laterales / 90", "Passing",
    "precision_pases_laterales_percent", "Precisión lateral (%)", "Passing",
    "pases_cortos_medios_90", "Pases cortos/medios / 90", "Passing",
    "precision_pases_cortos_medios_percent", "Precisión cortos/medios (%)", "Passing",
    "pases_largos_90", "Pases largos / 90", "Passing",
    "precision_pases_largos_percent", "Precisión pases largos (%)", "Passing",
    "longitud_media_pases_m", "Longitud media de pases (m)", "Passing",
    "longitud_media_pases_largos_m", "Longitud media de pases largos (m)", "Passing",
    "x_a_90", "xA / 90", "Creativity",
    "second_assists_90", "Segunda asistencia / 90", "Creativity",
    "third_assists_90", "Tercera asistencia / 90", "Creativity",
    "desmarques_90", "Desmarques / 90", "Attack",
    "precision_desmarques_percent", "Precisión desmarques (%)", "Attack",
    "jugadas_claves_90", "Pases clave / 90", "Creativity",
    "pases_en_el_ultimo_tercio_90", "Pases en tercio final / 90", "Creativity",
    "precision_pases_en_el_ultimo_tercio_percent", "Precisión tercio final (%)", "Creativity",
    "pases_al_area_de_penalti_90", "Pases al área / 90", "Creativity",
    "pases_hacia_el_area_pequena_percent", "Precisión área pequeña (%)", "Creativity",
    "pases_en_profundidad_90", "Pases en profundidad / 90", "Creativity",
    "precision_pases_en_profundidad_percent", "Precisión profundidad (%)", "Creativity",
    "ataque_en_profundidad_90", "Ataques en profundidad / 90", "Attack",
    "centros_desde_el_ultimo_tercio_90", "Centros desde último tercio / 90", "Attack",
    "pases_progresivos_90", "Pases progresivos / 90", "Passing",
    "precision_pases_progresivos_percent", "Precisión progresivos (%)", "Passing",
    "goles_recibidos", "Goles recibidos", "Goalkeeping",
    "goles_recibidos_90", "Goles recibidos / 90", "Goalkeeping",
    "remates_en_contra", "Remates en contra", "Goalkeeping",
    "remates_en_contra_90", "Remates en contra / 90", "Goalkeeping",
    "porterias_imbatidas_en_los_90", "Porterías imbatidas / 90", "Goalkeeping",
    "paradas_percent", "Porcentaje de paradas", "Goalkeeping",
    "x_g_en_contra", "xG en contra", "Goalkeeping",
    "x_g_en_contra_90", "xG en contra / 90", "Goalkeeping",
    "goles_evitados", "Goles evitados", "Goalkeeping",
    "goles_evitados_90", "Goles evitados / 90", "Goalkeeping",
    "pases_hacia_atras_recibidos_del_arquero_90", "Pases del arquero / 90", "Goalkeeping",
    "salidas_90", "Salidas / 90", "Goalkeeping",
    "tiros_libres_90", "Tiros libres / 90", "Attack",
    "tiros_libres_directos_90", "Tiros libres directos / 90", "Attack",
    "tiros_libres_directos_percent", "Precisión tiros libres (%)", "Attack",
    "corneres_90", "Corneres / 90", "Attack",
    "penaltis_a_favor", "Penaltis a favor", "Attack",
    "penaltis_realizados_percent", "Penaltis convertidos (%)", "Attack"
  )
}


#betplay
# df_fbref <- read_excel(paste0(.folder_data_input_r,"Betplay2021.xlsx"))
# df_fbref <- read_excel(paste0(.folder_data_input_py,"LigaEcu/players/players_ecu.xlsx"))
df_fbref <- read_excel(paste0(.folder_data_input_py,"Argentina_B/players/players.xlsx")) %>%
  select(-c(81,109)) %>%
  filter(!is.na(Equipo))

df_fbref$Equipo %>% unique()

df_fbref <- df_fbref %>%
  rename('Minutos' = `Minutos jugados`) %>%
  janitor::clean_names()

colnames(df_fbref)

df_fbref$posicion_especifica %>% unique()

#Delanteros
df_delanteros <- df_fbref %>%
  filter(df_fbref$posicion_especifica %in% c('CF', 'SS', 'AMF')) %>%
  select(1, 2, 5, 9, 14, 25, 37, 38, 41, 43, 45, 48, 56, 58, 63, 66, 80, 86) %>%
  filter(minutos > .minutes)

##Proceso de filtrado
df_pintar <- df_delanteros

df_pintar[df_delanteros==""]<-0

df_pintar2<- distinct(df_pintar)

Jugador_Seleccionado <- df_pintar2 %>%
  filter(jugador=="R. Monti", equipo=="Vinotinto de Ecuador" )

Jugador_Seleccionado <- reshape2::melt(Jugador_Seleccionado,id.vars=c(1:4))

Jugadores_Todos <- reshape2::melt(df_pintar2,id.vars=c(1:4))

Jugadores_Todos <- Jugadores_Todos %>%
  filter(minutos >= .minutes) %>%
  group_by(variable) %>%
  mutate(
    Percentil = percent_rank(value),
    Decil = pmin(floor(Percentil * 10), 9),
    RangoPercentil = case_when(
      Decil == 9 ~ "90-100%",
      Decil == 8 ~ "80-89%",
      Decil == 7 ~ "70-79%",
      Decil == 6 ~ "60-69%",
      Decil == 5 ~ "50-59%",
      Decil == 4 ~ "40-49%",
      Decil == 3 ~ "30-39%",
      Decil == 2 ~ "20-29%",
      Decil == 1 ~ "10-19%",
      TRUE       ~ "0-9%"
    ),
    Ranking = min_rank(-value)
  ) %>%
  ungroup()

nTotal_Jugadores<-Jugadores_Todos %>%
  distinct(jugador) %>%
  nrow()

Pintar_Jugador <- Jugadores_Todos %>%
  filter(jugador=="R. Monti", equipo=="Vinotinto de Ecuador" )

Pintar_Jugador$value<-round(Pintar_Jugador$value,2)

names = colnames(df_pintar)[5:ncol(df_pintar)]

categoria_delanteos <- categoria_metricas %>%
  filter(tipo == 'Attack')

Pintar_Jugador <- Pintar_Jugador %>%
  inner_join(categoria_metricas %>% select(variable, nombre_variable = nombre, tipo),
             by = "variable")

Pintar_Jugador <- Pintar_Jugador %>%
  distinct()

Pintar_Jugador <- Pintar_Jugador %>%
  mutate(nombre_variable = factor(nombre_variable, levels = nombre_variable[order(Ranking, decreasing = TRUE)]))

coloresrango <- c(
  "0-9%" = "red3",
  "10-19%" = "orange",
  "20-29%" = "gold",
  "30-39%" = "yellow2",
  "40-49%" = "khaki3",
  "50-59%" = "yellowgreen",
  "60-69%" = "olivedrab",
  "70-79%" = "darkolivegreen3",
  "80-89%" = "seagreen3",
  "90-100%" = "green4"
)

titulo <-paste("**",Pintar_Jugador$jugador[1], "** (",Pintar_Jugador$edad[1],")",sep="")
subtitulo <- paste(Pintar_Jugador$equipo[1]," - ",.ligue," (",Pintar_Jugador$minutos[1],") -  min","\n",
                   "Players ",nTotal_Jugadores, " Strikers"
                   ,sep="")

p <- ggplot(Pintar_Jugador, aes(x = Decil * 10, y = nombre_variable, group = tipo, color = RangoPercentil)) +
  geom_segment(aes(yend = nombre_variable), xend = 0, size = 2) +
  geom_point(size = 3.5) +
  scale_colour_manual(values = coloresrango) +
  ggrepel::geom_text_repel(
    aes(label = value),
    nudge_x = 0.3,
    size = 3,
    color = "black",
    fontface = "bold",
    hjust = -1,
    vjust = 0.5,
    min.segment.length = 3,
    max.overlaps = Inf
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(0, 90, 10),
    labels = paste0("p", seq(0, 90, 10))
  ) +
  annotate("rect", fill = "white", alpha = 0.5, color = "white",
           xmin = 100, xmax = 109,
           ymin = -Inf, ymax = Inf) +
  geom_text(aes(label = paste0("#", Ranking, " (P", Decil * 10, ")"), y = nombre_variable, x = 104),
            fontface = "bold.italic", size = 4, color = "black")+
  labs(title =titulo,
       subtitle = subtitulo,
       x="",
       y="",
       caption = c(paste0("Source Wyscout ",.minutes, "by: Erick Rangel"))
       ) +
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = ggtext::element_markdown(size = 16, hjust = 0.5, colour = "black"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "black"),
        panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(),
        axis.text.y = element_text(size=12),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = "bottom",
        plot.margin = unit(c(2,2,2,2), "cm"),
        plot.caption = element_text(hjust=c(0, 1),size=8))

player_plot=list()
player_plot[[1]]<-p

p

h<-cowplot::ggdraw(player_plot[[1]]) +
  cowplot::draw_image(paste0(.folder_img_r,"ARGENTINA_RAFAEL_MONTI.png"),
                      x = -0.04, y = 0.38,
                      scale=0.14
  ) +
  cowplot::draw_image(paste0(.folder_img_r,"ECU_VIE.png"),
                      x = -0.095, y = 0.38,
                      scale=0.14
  ) +
  theme(plot.background = element_rect(fill="white", color = NA))

h

ggsave(paste0(.folder_img_r,"final.png"), plot = h, width = 34, height = 15, units = "cm")
