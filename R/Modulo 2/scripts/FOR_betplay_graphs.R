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
.minutes = 10
.team = 'Vinotinto de Ecuador'

.ligue <- 'Argentina. Primera Nacional'
.ligue_short <- 'ARG_'
.country <- 'Argentina_B'
.team = 'Gimnasia Mendoza'

getwd()

{

  categoria_metricas <- tribble(
    ~variable, ~nombre, ~tipo, ~name,
    "jugador", "Jugador", "General", "Player",
    "equipo", "Equipo", "General", "Equipment",
    "equipo_durante_el_periodo_seleccionado", "Equipo Durante El Periodo Seleccionado", "General", "Team During The Selected Period",
    "posicion_especifica", "Posición Específica", "General", "Specific Position",
    "edad", "Edad", "General", "Age",
    "valor_de_mercado_transfermarkt", "Valor de mercado", "General", "Market value",
    "vencimiento_contrato", "Vencimiento contrato", "General", "Contract expiration",
    "partidos_jugados", "Partidos jugados", "General", "Matches played",
    "minutos", "Minutos", "General", "Minutes",
    "goles", "Goles", "Attack", "Goals",
    "x_g", "xG", "Attack", "xG",
    "asistencias", "Asistencias", "Creativity", "Assists",
    "x_a", "xA", "Creativity", "xA",
    "duelos_90", "Duelos / 90", "Physical", "Duels / 90",
    "duelos_ganados_percent", "Duelos ganados (%)", "Physical", "Duels won (%)",
    "pais_de_nacimiento", "País de nacimiento", "General", "Country of birth",
    "pasaporte", "Pasaporte", "General", "Passport",
    "pie", "Pie dominante", "General", "Dominant foot",
    "altura", "Altura", "General", "Height",
    "peso", "Peso", "General", "Weight",
    "en_prestamo", "En préstamo", "General", "On loan",
    "acciones_defensivas_realizadas_90", "Acciones defensivas / 90", "Defense", "Defensive actions / 90",
    "duelos_defensivos_90", "Duelos defensivos / 90", "Defense", "Defensive duels / 90",
    "duelos_defensivos_ganados_percent", "Duelos defensivos ganados (%)", "Defense", "Defensive duels won (%)",
    "duelos_aereos_en_los_90", "Duelos aéreos / 90", "Defense", "Aerial Duels / 90",
    "duelos_aereos_ganados_percent", "Duelos aéreos ganados (%)", "Defense", "Aerial duels won (%)",
    "entradas_90", "Entradas / 90", "Defense", "Tickets / 90",
    "posesion_conquistada_despues_de_una_entrada", "Posesión tras entrada", "Defense", "Possession after entry",
    "tiros_interceptados_90", "Tiros interceptados / 90", "Defense", "Intercepted shots / 90",
    "interceptaciones_90", "Intercepciones / 90", "Defense", "Interceptions / 90",
    "posesion_conquistada_despues_de_una_interceptacion", "Posesión tras intercepción", "Defense", "Possession after interception",
    "faltas_90", "Faltas / 90", "Discipline", "Fouls / 90",
    "tarjetas_amarillas", "Tarjetas amarillas", "Discipline", "Yellow cards",
    "tarjetas_amarillas_90", "Tarjetas amarillas / 90", "Discipline", "Yellow cards / 90",
    "tarjetas_rojas", "Tarjetas rojas", "Discipline", "Red cards",
    "tarjetas_rojas_90", "Tarjetas rojas / 90", "Discipline", "Red cards / 90",
    "acciones_de_ataque_exitosas_90", "Acciones ofensivas exitosas / 90", "Attack", "Successful offensive actions / 90",
    "goles_90", "Goles / 90", "Attack", "Goals / 90",
    "goles_excepto_los_penaltis", "Goles (sin penaltis)", "Attack", "Goals (without penalties)",
    "goles_excepto_los_penaltis_90", "Goles (sin penaltis) / 90", "Attack", "Goals (without penalties) / 90",
    "x_g_90", "xG / 90", "Attack", "xG / 90",
    "goles_de_cabeza", "Goles de cabeza", "Attack", "Headed goals",
    "goles_de_cabeza_90", "Goles de cabeza / 90", "Attack", "Headed goals / 90",
    "remates", "Remates", "Attack", "Auctions",
    "remates_90", "Remates / 90", "Attack", "Auctions / 90",
    "tiros_a_la_porteria_percent", "Precisión de remates (%)", "Attack", "Shot accuracy (%)",
    "goles_hechos_percent", "Tasa de conversión (%)", "Attack", "Conversion rate (%)",
    "asistencias_90", "Asistencias / 90", "Creativity", "Assists / 90",
    "centros_90", "Centros / 90", "Attack", "Centers / 90",
    "precision_centros_percent", "Precisión centros (%)", "Attack", "Accuracy centers (%)",
    "centros_desde_la_banda_izquierda_90", "Centros izquierda / 90", "Attack", "Left centers / 90",
    "precision_centros_desde_la_banda_izquierda_percent", "Precisión izquierda (%)", "Attack", "Left accuracy (%)",
    "centros_desde_la_banda_derecha_90", "Centros derecha / 90", "Attack", "Centers right / 90",
    "precision_centros_desde_la_banda_derecha_percent", "Precisión derecha (%)", "Attack", "Right accuracy (%)",
    "centros_al_area_pequena_90", "Centros al área pequeña / 90", "Attack", "Crosses into the small area / 90",
    "regates_90", "Regates / 90", "Physical", "Dribbling / 90",
    "regates_realizados_percent", "Éxito en regates (%)", "Physical", "Dribbling success (%)",
    "duelos_atacantes_90", "Duelos ofensivos / 90", "Attack", "Offensive duels / 90",
    "duelos_atacantes_ganados_percent", "Duelos ofensivos ganados (%)", "Attack", "Offensive duels won (%)",
    "toques_en_el_area_de_penalti_90", "Toques en área / 90", "Attack", "Touches in the area / 90",
    "carreras_en_progresion_90", "Carreras progresivas / 90", "Attack", "Progressive Races / 90",
    "aceleraciones_90", "Aceleraciones / 90", "Attack", "Accelerations / 90",
    "pases_recibidos_90", "Pases recibidos / 90", "Physical", "Passes received / 90",
    "pases_largos_recibidos_90", "Pases largos recibidos / 90", "Physical", "Long passes received / 90",
    "faltas_recibidas_90", "Faltas recibidas / 90", "Discipline", "Fouls received / 90",
    "pases_90", "Pases / 90", "Passing", "Passes / 90",
    "precision_pases_percent", "Precisión total pases (%)", "Passing", "Total pass accuracy (%)",
    "pases_hacia_adelante_90", "Pases hacia adelante / 90", "Passing", "Forward passes / 90",
    "precision_pases_hacia_adelante_percent", "Precisión adelante (%)", "Passing", "Forward Accuracy (%)",
    "pases_hacia_atras_90", "Pases hacia atrás / 90", "Passing", "Back passes / 90",
    "precision_pases_hacia_atras_percent", "Precisión atrás (%)", "Passing", "Backward Accuracy (%)",
    "pases_laterales_90", "Pases laterales / 90", "Passing", "Lateral passes / 90",
    "precision_pases_laterales_percent", "Precisión lateral (%)", "Passing", "Lateral accuracy (%)",
    "pases_cortos_medios_90", "Pases cortos/medios / 90", "Passing", "Short/medium passes / 90",
    "precision_pases_cortos_medios_percent", "Precisión cortos/medios (%)", "Passing", "Short/medium accuracy (%)",
    "pases_largos_90", "Pases largos / 90", "Passing", "Long passes / 90",
    "precision_pases_largos_percent", "Precisión pases largos (%)", "Passing", "Long pass accuracy (%)",
    "longitud_media_pases_m", "Longitud media de pases (m)", "Passing", "Average pass length (m)",
    "longitud_media_pases_largos_m", "Longitud media de pases largos (m)", "Passing", "Average length of long passes (m)",
    "x_a_90", "xA / 90", "Creativity", "xA / 90",
    "second_assists_90", "Segunda asistencia / 90", "Creativity", "Second assist / 90",
    "third_assists_90", "Tercera asistencia / 90", "Creativity", "Third assist / 90",
    "desmarques_90", "Desmarques / 90", "Attack", "Unmarks / 90",
    "precision_desmarques_percent", "Precisión desmarques (%)", "Attack", "Accuracy of runs (%)",
    "jugadas_claves_90", "Pases clave / 90", "Creativity", "Key passes / 90",
    "pases_en_el_ultimo_tercio_90", "Pases en tercio final / 90", "Creativity", "Passes in the final third / 90",
    "precision_pases_en_el_ultimo_tercio_percent", "Precisión tercio final (%)", "Creativity", "Final third accuracy (%)",
    "pases_al_area_de_penalti_90", "Pases al área / 90", "Creativity", "Passes to the area / 90",
    "pases_hacia_el_area_pequena_percent", "Precisión área pequeña (%)", "Creativity", "Small area accuracy (%)",
    "pases_en_profundidad_90", "Pases en profundidad / 90", "Creativity", "Deep passes / 90",
    "precision_pases_en_profundidad_percent", "Precisión profundidad (%)", "Creativity", "Depth accuracy (%)",
    "ataque_en_profundidad_90", "Ataques en profundidad / 90", "Attack", "Deep Attacks / 90",
    "centros_desde_el_ultimo_tercio_90", "Centros desde último tercio / 90", "Attack", "Centers from the final third / 90",
    "pases_progresivos_90", "Pases progresivos / 90", "Passing", "Progressive passes / 90",
    "precision_pases_progresivos_percent", "Precisión progresivos (%)", "Passing", "Progressive accuracy (%)",
    "goles_recibidos", "Goles recibidos", "Goalkeeping", "Goals received",
    "goles_recibidos_90", "Goles recibidos / 90", "Goalkeeping", "Goals received / 90",
    "remates_en_contra", "Remates en contra", "Goalkeeping", "Counterattacks",
    "remates_en_contra_90", "Remates en contra / 90", "Goalkeeping", "Shots against / 90",
    "porterias_imbatidas_en_los_90", "Porterías imbatidas / 90", "Goalkeeping", "Clean sheets / 90",
    "paradas_percent", "Porcentaje de paradas", "Goalkeeping", "Percentage of stops",
    "x_g_en_contra", "xG en contra", "Goalkeeping", "xG against",
    "x_g_en_contra_90", "xG en contra / 90", "Goalkeeping", "xG against / 90",
    "goles_evitados", "Goles evitados", "Goalkeeping", "Goals avoided",
    "goles_evitados_90", "Goles evitados / 90", "Goalkeeping", "Goals avoided / 90",
    "pases_hacia_atras_recibidos_del_arquero_90", "Pases del arquero / 90", "Goalkeeping", "Goalkeeper passes / 90",
    "salidas_90", "Salidas / 90", "Goalkeeping", "Exits / 90",
    "tiros_libres_90", "Tiros libres / 90", "Attack", "Free throws / 90",
    "tiros_libres_directos_90", "Tiros libres directos / 90", "Attack", "Direct free kicks / 90",
    "tiros_libres_directos_percent", "Precisión tiros libres (%)", "Attack", "Free throw accuracy (%)",
    "corneres_90", "Corneres / 90", "Attack", "Corners / 90",
    "penaltis_a_favor", "Penaltis a favor", "Attack", "Penalties in favor",
    "penaltis_realizados_percent", "Penaltis convertidos (%)", "Attack", "Penalties converted (%)"
  )
}


{
# Posiciones por tipo de jugador
posiciones_por_categoria <- list(
  Delanteros = c("CF"),
  Extremos = c("RW", "RWF", "LW", "LWF"),
  Mediocampo_Ofensivo = c("AMF", "RAMF", "LAMF"),
  Mediocampo = c("CMF", "RCMF", "LCMF"),
  Mediocampo_Defensivo = c("DMF", "RDMF", "LDMF"),
  Defensas = c("CB", "RCB", "RB", "RWB", "LCB", "LB", "LWB"),
  Arqueros = c("GK")
)

# Métricas más relevantes por tipo
metricas_por_tipo <- list(
  Delanteros = c(
    "goles_90", "x_g_90", "x_a_90","goles_excepto_los_penaltis_90", "asistencias_90",
    "remates_90", "tiros_a_la_porteria_percent", "toques_en_el_area_de_penalti_90",
    "duelos_atacantes_90", "duelos_atacantes_ganados_percent", "desmarques_90",
    "precision_desmarques_percent", "carreras_en_progresion_90", "pases_al_area_de_penalti_90"
  ),
  Extremos = c(
    "regates_90", "regates_realizados_percent", "asistencias_90", "x_g_90", "x_a_90",
    "centros_90", "precision_centros_percent", "pases_en_el_ultimo_tercio_90",
    "precision_pases_en_el_ultimo_tercio_percent", "duelos_atacantes_90",
    "duelos_atacantes_ganados_percent", "toques_en_el_area_de_penalti_90",
    "carreras_en_progresion_90", "desmarques_90", "pases_al_area_de_penalti_90"
  ),
  Mediocampo_Ofensivo = c(
    "asistencias_90","x_g_90", "x_a_90", "jugadas_claves_90", "pases_en_el_ultimo_tercio_90",
    "precision_pases_en_el_ultimo_tercio_percent", "remates_90",
    "goles_90", "pases_progresivos_90", "pases_al_area_de_penalti_90",
    "carreras_en_progresion_90", "toques_en_el_area_de_penalti_90",
    "second_assists_90", "pases_en_profundidad_90"
  ),
  Mediocampo = c(
    "pases_90","x_g_90", "x_a_90", "precision_pases_percent", "pases_hacia_adelante_90",
    "precision_pases_hacia_adelante_percent", "pases_largos_90",
    "precision_pases_largos_percent", "pases_progresivos_90",
    "jugadas_claves_90", "duelos_90", "duelos_ganados_percent",
    "interceptaciones_90", "entradas_90", "posesion_conquistada_despues_de_una_interceptacion",
    "faltas_90"
  ),
  Mediocampo_Defensivo = c(
    "duelos_defensivos_90","x_g_90", "x_a_90", "duelos_defensivos_ganados_percent", "interceptaciones_90",
    "entradas_90", "posesion_conquistada_despues_de_una_entrada", "pases_90",
    "precision_pases_percent", "pases_hacia_atras_90", "precision_pases_hacia_atras_percent",
    "faltas_90", "tarjetas_amarillas_90", "posesion_conquistada_despues_de_una_interceptacion",
    "pases_laterales_90", "precision_pases_laterales_percent"
  ),
  Defensas = c(
    "duelos_defensivos_90","x_g_90", "x_a_90", "duelos_defensivos_ganados_percent", "interceptaciones_90",
    "entradas_90", "duelos_aereos_en_los_90", "duelos_aereos_ganados_percent",
    "posesion_conquistada_despues_de_una_entrada", "tiros_interceptados_90",
    "pases_90", "precision_pases_percent", "pases_largos_90",
    "precision_pases_largos_percent", "faltas_90", "tarjetas_amarillas_90"
  ),
  Arqueros = c(
    "goles_recibidos_90", "x_g_en_contra_90", "goles_evitados_90",
    "paradas_percent", "porterias_imbatidas_en_los_90",
    "remates_en_contra_90", "salidas_90", "pases_hacia_atras_recibidos_del_arquero_90",
    "pases_largos_90", "precision_pases_largos_percent"
  )
)
}

# Convertir lista a data.frame con dos columnas: categoria y posicion
posiciones_df <- posiciones_por_categoria %>%
  enframe(name = "categoria", value = "posiciones") %>%
  unnest(posiciones) %>%
  rename(posicion_especifica = posiciones)


filtrar_por_tipo <- function(df, posiciones, variables, minutos = .minutes) {

  columnas_base <- c("jugador", "equipo","logo_team", "equipo_durante_el_periodo_seleccionado",
                     "posicion_especifica", "categoria","minutos", "edad", "pais_de_nacimiento")
  columnas_finales <- intersect(c(columnas_base, variables), colnames(df))

  patron <- paste0("\\b(", paste0(posiciones, collapse = "|"), ")\\b")

  df %>%
    filter(str_detect(posicion_especifica, patron), minutos >= minutos) %>%
    select(all_of(columnas_finales))
}

lista_dfs_posiciones_filtrados <- list()

# df_fbref <- read_excel(paste0(.folder_data_input_r,"Betplay2021.xlsx"))
# df_fbref <- read_excel(paste0(.folder_data_input_py,"LigaEcu/players/players_ecu.xlsx"))

df_teams <- read_excel(paste0(.folder_data_input_r,"Teams.xlsx"), sheet = .country)

df_teams <- df_teams %>%
  mutate(
    logo_team = paste0(toupper(substring(Country,1, 3)), "_", Code)
  )

df_fbref <- read_excel(paste0(.folder_data_input_py,"Argentina_B/players/players.xlsx")) %>%
  select(-c(48,109)) %>%
  filter(!is.na(Equipo))

names(df_fbref) <- gsub("\\.\\.\\.[0-9]+$", "", names(df_fbref))

df_fbref <- df_fbref %>%
  rename('Minutos' = `Minutos jugados`) %>%
  janitor::clean_names() %>%
  mutate(
    posicion_especifica = gsub(",", "", posicion_especifica),             # elimina comas
    posicion_especifica = trimws(posicion_especifica),                    # elimina espacios
    posicion_especifica = toupper(substr(posicion_especifica, 1, 3))      # asegura mayúsculas y 3 letras
  )


df_fbref <- df_fbref %>%
  inner_join(df_teams, by=c("equipo"="Understat"))

# Normalizar las posiciones (quitar espacios y homologar nombres)
df_fbref <- df_fbref %>%
  mutate(
    posicion_especifica = str_trim(posicion_especifica),
    posicion_especifica = recode(
      posicion_especifica,
      "LCM" = "LCMF",
      "RCM" = "RCMF",
      "LAM" = "LAMF",
      "RAM" = "RAMF",
      "RDM" = "RDMF",
      "LDM" = "LDMF",
      "CF " = "CF",
      "LW " = "LW",
      "RW " = "RW",
      "LB " = "LB",
      "RB " = "RB",
      "CB " = "CB"
    )
  ) %>%
  left_join(posiciones_df, by = "posicion_especifica")

for (rol in names(posiciones_por_categoria)) {
  posiciones <- posiciones_por_categoria[[rol]]
  metricas <- metricas_por_tipo[[rol]]

  lista_dfs_posiciones_filtrados[[rol]] <- filtrar_por_tipo(
    df_fbref,
    posiciones,
    metricas
  )
}

# FUNCIÓN DE FILTRADO
filtrar_por_tipo <- function(df, posiciones, variables, minutos = .minutes) {
  columnas_base <- c("jugador", "equipo","logo_team", "equipo_durante_el_periodo_seleccionado",
                     "posicion_especifica","categoria", "minutos", "edad", "pais_de_nacimiento")
  columnas_finales <- intersect(c(columnas_base, variables), colnames(df))

  df %>%
    filter(posicion_especifica %in% posiciones, minutos >= minutos) %>%
    select(all_of(columnas_finales))
}

# CREAR LISTA DE DATASETS FILTRADOS
lista_dfs_posiciones_filtrados <- list()

for (rol in names(posiciones_por_categoria)) {
  posiciones <- posiciones_por_categoria[[rol]]
  metricas <- metricas_por_tipo[[rol]]
  lista_dfs_posiciones_filtrados[[rol]] <- filtrar_por_tipo(df_fbref, posiciones, metricas)
}

# --- PALETA PARA RANGOS ---
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

# --- FUNCIONES DE PROCESO Y GRÁFICO ---

procesar_para_grafico <- function(df) {
  df[df == ""] <- 0
  df <- distinct(df)

  melted <- reshape2::melt(df, id.vars = c("jugador", "equipo","logo_team", "equipo_durante_el_periodo_seleccionado",
                                           "posicion_especifica","categoria", "minutos", "edad", "pais_de_nacimiento"))

  melted <- melted %>%
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

  return(melted)
}

graficar_jugador <- function(data_jugadores, categoria_metricas, rol = "", equipos = c()) {
  # browser()
  df_exportados <- data.frame(
    jugador = character(),
    equipo = character(),
    categoria = character(),
    archivo_png = character(),
    stringsAsFactors = FALSE
  )

  jugadores <- data_jugadores %>%
    filter(equipo %in% equipos) %>%
    pull(jugador) %>%
    unique()

  for (jugador_actual in jugadores) {
    Pintar_Jugador <- data_jugadores %>%
      filter(jugador == jugador_actual, equipo %in% equipos) %>%
      mutate(value = round(value, 2),
             pais_de_nacimiento = toupper(pais_de_nacimiento)) %>%
      inner_join(categoria_metricas %>% select(variable, nombre_variable = name, tipo),
                 by = "variable") %>%
      distinct() %>%
      mutate(nombre_variable = factor(nombre_variable, levels = nombre_variable[order(Ranking, decreasing = TRUE)]))

    if (nrow(Pintar_Jugador) == 0) next

    titulo <- paste("**", jugador_actual, "** (", Pintar_Jugador$edad[1], ") - ", Pintar_Jugador$categoria[1], sep = "")
    subtitulo <- paste(Pintar_Jugador$equipo[1], " - ", .ligue, " (", Pintar_Jugador$minutos[1], ") - min\n",
                       "Total jugadores: ", n_distinct(data_jugadores$jugador))

    # Gráfico...
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
                fontface = "bold.italic", size = 4, color = "black") +
      labs(
        title = titulo,
        subtitle = subtitulo,
        x = "",
        y = "",
        caption = paste("Source Wyscout - Minutes (", .minutes, ") by: Erick Rangel")
      ) +
      theme_bw() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = ggtext::element_markdown(size = 16, hjust = 0.5, colour = "black"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "black"),
        panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(),
        axis.text.y = element_text(size = 12),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = "bottom",
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.caption = element_text(hjust = c(1, 1), size = 8)
      )

    # nombre de archivo
    nombre_archivo <- toupper(gsub("\\.", "", jugador_actual))
    nombre_archivo <- gsub(" ", "_", nombre_archivo)

    nombre_archivo2 <- paste0(
      gsub(" ", "_", toupper(Pintar_Jugador$categoria[1])), "_",
      gsub(" ", "_", toupper(Pintar_Jugador$equipo[1])), "_",
      nombre_archivo,
      ".png"
    )

    name_player <- paste0(Pintar_Jugador$pais_de_nacimiento[1], "_", nombre_archivo, ".png")


    # Ruta imagen, render y guardado
    ruta_imagen_jugador <- file.path(.folder_img_r, "imagenes_jugadores", name_player)
    ruta_imagen_default <- file.path(.folder_img_r, "default_sombra.png")
    imagen_a_usar <- if (file.exists(ruta_imagen_jugador)) ruta_imagen_jugador else ruta_imagen_default

    h <- cowplot::ggdraw(p) +
      cowplot::draw_image(imagen_a_usar,
                          x = -0.045, y = 0.41, scale = 0.12) +
      # cowplot::draw_image(file.path(.folder_img_r, "ARG_GIM.png"),
      cowplot::draw_image(file.path(paste0(.folder_img_r, Pintar_Jugador$logo_team, ".png")),
                          x = -0.095, y = 0.41, scale = 0.12) +
      theme(plot.background = element_rect(fill = "white", color = NA))

    ruta_salida <- file.path(.folder_img_r, "output/", rol, nombre_archivo2)
    ggsave(ruta_salida, plot = h, width = 34, height = 15, units = "cm")

    # Registrar exportado
    df_exportados <- rbind(df_exportados, data.frame(
      jugador = jugador_actual,
      equipo = Pintar_Jugador$equipo[1],
      categoria = Pintar_Jugador$categoria[1],
      archivo_png = nombre_archivo2,
      stringsAsFactors = FALSE
    ))
  }

  return(df_exportados)
}


df_exportados_global <- data.frame()

for (rol in names(lista_dfs_posiciones_filtrados)) {
  cat("Procesando: ", rol, "\n")
  df_filtrado <- lista_dfs_posiciones_filtrados[[rol]]
  df_procesado <- procesar_para_grafico(df_filtrado)
  df_exportados <- graficar_jugador(df_procesado, categoria_metricas, rol = "", equipos = "Gimnasia Mendoza")
  # df_exportados <- graficar_jugador(df_procesado, categoria_metricas, rol = "", equipos = unique(df_filtrado$equipo))
  # df_exportados <- graficar_jugador(df_procesado, categoria_metricas, rol = rol, equipos = unique(df_filtrado$equipo))
  df_exportados_global <- bind_rows(df_exportados_global, df_exportados)
}

# Unir con df_fbref
df_fbref <- df_fbref %>%
  left_join(df_exportados_global, by = c("jugador", "equipo", "categoria"))


write.csv(df_fbref, file = file.path(.folder_data_out_r,"players_liga_argentina_B.csv"), row.names = FALSE)


df_fbref %>% select(c(1:10),archivo_png) %>% filter(equipo == 'Gimnasia Mendoza') %>% view()

ss














graficar_jugador2 <- function(data_jugadores, categoria_metricas, rol = "", nombre_equipo = "Gimnasia Mendoza") {
  # browser()
  jugadores <- data_jugadores %>%
    filter(equipo == nombre_equipo) %>%
    pull(jugador) %>%
    unique()

  for (jugador_actual in jugadores) {
    Pintar_Jugador <- data_jugadores %>%
      filter(jugador == jugador_actual, equipo == nombre_equipo) %>%
      mutate(value = round(value, 2),
             pais_de_nacimiento = toupper(pais_de_nacimiento)) %>%
      inner_join(categoria_metricas %>% select(variable, nombre_variable = nombre, tipo),
                 by = "variable") %>%
      distinct() %>%
      mutate(nombre_variable = factor(nombre_variable, levels = nombre_variable[order(Ranking, decreasing = TRUE)]))

    if (nrow(Pintar_Jugador) == 0) next

    titulo <- paste("**", jugador_actual, "** (", Pintar_Jugador$edad[1], ") - ", Pintar_Jugador$categoria[1], sep = "")
    subtitulo <- paste(Pintar_Jugador$equipo[1], " - ", .ligue, " (", Pintar_Jugador$minutos[1], ") - min\n",
                       "Total jugadores: ", n_distinct(data_jugadores$jugador))

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
                fontface = "bold.italic", size = 4, color = "black") +
      labs(
        title = titulo,
        subtitle = subtitulo,
        x = "",
        y = "",
        caption = paste("Source Wyscout - Minutes (", .minutes, ") by: Erick Rangel")
      ) +
      theme_bw() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = ggtext::element_markdown(size = 16, hjust = 0.5, colour = "black"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "black"),
        panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(),
        axis.text.y = element_text(size = 12),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = "bottom",
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.caption = element_text(hjust = c(1, 1), size = 8)
      )

    nombre_archivo <- toupper(gsub("\\.", "", jugador_actual))
    nombre_archivo <- gsub(" ", "_", nombre_archivo)
    # nombre_archivo <- paste0(Pintar_Jugador$pais_de_nacimiento[1], "_", nombre_archivo, ".png")
    nombre_archivo2 <- paste0(Pintar_Jugador$equipo[1], "_", nombre_archivo,Pintar_Jugador$categoria[1],".png")
    # nombre_archivo2 <- paste0(Pintar_Jugador$categoria[1],"_",Pintar_Jugador$pais_de_nacimiento[1], "_", nombre_archivo, ".png")
    nombre_archivo <- paste0(Pintar_Jugador$pais_de_nacimiento[1], "_", nombre_archivo, ".png")

    # Ruta de la imagen del jugador
    ruta_imagen_jugador <- file.path(.folder_img_r, "imagenes_jugadores", nombre_archivo)

    # Imagen por defecto (sombra)
    ruta_imagen_default <- file.path(.folder_img_r, "default_sombra.png")  # Asegúrate de que exista

    # Verifica si existe la imagen del jugador, si no, usa la sombra
    imagen_a_usar <- if (file.exists(ruta_imagen_jugador)) ruta_imagen_jugador else ruta_imagen_default

    # Construcción del gráfico con imagen
    h <- cowplot::ggdraw(p) +
      cowplot::draw_image(imagen_a_usar,
                          x = -0.045, y = 0.41, scale = 0.12) +
      cowplot::draw_image(file.path(.folder_img_r, "ARG_GIM.png"),
                          x = -0.095, y = 0.41, scale = 0.12) +
      theme(plot.background = element_rect(fill = "white", color = NA))

    # Guardar en carpeta por tipo de rol
    ruta_salida <- file.path(.folder_img_r,"output/", rol, paste0(nombre_archivo2))
    ggsave(ruta_salida, plot = h, width = 34, height = 15, units = "cm")
  }
}

# Crear subcarpetas por tipo de jugador (si no existen)
# for (rol in names(lista_dfs_posiciones_filtrados)) {
#   dir_path <- file.path(.folder_img_r,'output/', rol)
#   if (!dir.exists(dir_path)) {
#     dir.create(dir_path, recursive = TRUE)
#   }
# }
#
# for (rol in names(lista_dfs_posiciones_filtrados)) {
#   cat("Procesando: ", rol, "\n")
#   df_filtrado <- lista_dfs_posiciones_filtrados[[rol]]
#   df_procesado <- procesar_para_grafico(df_filtrado)
#   # graficar_jugador(df_procesado, categoria_metricas, rol = '')
#   graficar_jugador(df_procesado, categoria_metricas, rol = 'Total_players')
# }




graficar_jugador <- function(data_jugadores, categoria_metricas, nombre_equipo = .team) {
  jugadores <- data_jugadores %>%
    filter(equipo == nombre_equipo) %>%
    pull(jugador) %>%
    unique()

  for (jugador_actual in jugadores) {
    Pintar_Jugador <- data_jugadores %>%
      filter(jugador == jugador_actual, equipo == nombre_equipo) %>%
      mutate(value = round(value, 2),
             pais_de_nacimiento = toupper(pais_de_nacimiento)) %>%
      inner_join(categoria_metricas %>% select(variable, nombre_variable = nombre, tipo),
                 by = "variable") %>%
      distinct() %>%
      mutate(nombre_variable = factor(nombre_variable, levels = nombre_variable[order(Ranking, decreasing = TRUE)]))

    if (nrow(Pintar_Jugador) == 0) next

    titulo <- paste("**", jugador_actual, "** (", Pintar_Jugador$edad[1], ")", sep = "")
    subtitulo <- paste(Pintar_Jugador$equipo[1], " - ", .ligue, " (", Pintar_Jugador$minutos[1], ") - min\n",
                       "Total jugadores: ", n_distinct(data_jugadores$jugador))

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
                fontface = "bold.italic", size = 4, color = "black") +
      labs(
        title = titulo,
        subtitle = subtitulo,
        x = "",
        y = "",
        caption = paste("Source Wyscout - Minutes (", .minutes, ") by: Erick Rangel")
      ) +
      theme_bw() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = ggtext::element_markdown(size = 16, hjust = 0.5, colour = "black"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "black"),
        panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(),
        axis.text.y = element_text(size = 12),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = "bottom",
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.caption = element_text(hjust = c(1, 1), size = 8)
      )

    # Imagen del jugador
    nombre_archivo <- toupper(gsub("\\.", "", jugador_actual))
    nombre_archivo <- gsub(" ", "_", nombre_archivo)
    nombre_archivo <- paste0(Pintar_Jugador$pais_de_nacimiento[1], "_", nombre_archivo, ".png")

    h <- cowplot::ggdraw(p) +
      cowplot::draw_image(paste0(.folder_img_r,'imagenes_jugadores/', nombre_archivo),
                          x = -0.04, y = 0.38, scale = 0.14) +
      cowplot::draw_image(paste0(.folder_img_r, "ECU_VIE.png"),
                          x = -0.095, y = 0.38, scale = 0.14) +
      theme(plot.background = element_rect(fill = "white", color = NA))

    ggsave(paste0(.folder_img_r,'output/', "plot_", nombre_archivo), plot = h, width = 34, height = 15, units = "cm")
  }
}


df_pintar <- lista_dfs_posiciones_filtrados$Defensas
data_jugadores <- procesar_para_grafico(df_pintar)
graficar_jugador(data_jugadores, categoria_metricas)
























colnames(df_fbref)

df_fbref$posicion_especifica %>% unique()

#Delanteros
df_delanteros <- df_fbref %>%
  filter(df_fbref$posicion_especifica %in% c('CF',)) %>%
  select(1, 2, 5,16, 9, 14, 25, 37, 38, 41, 43, 45, 48, 56, 58, 63, 66, 80, 86) %>%
  filter(minutos > .minutes)

#Arquero
df_arquero <- df_fbref %>%
  filter(df_fbref$posicion_especifica %in% c('CF', 'SS', 'AMF')) %>%
  select(1, 2, 5,16, 9, 14, 25, 37, 38, 41, 43, 45, 48, 56, 58, 63, 66, 80, 86) %>%
  filter(minutos > .minutes)

#Mediocampistas
df_mediocampistas <- df_fbref %>%
  filter(df_fbref$posicion_especifica %in% c('CF', 'SS', 'AMF')) %>%
  select(1, 2, 5,16, 9, 14, 25, 37, 38, 41, 43, 45, 48, 56, 58, 63, 66, 80, 86) %>%
  filter(minutos > .minutes)

#Defensas
df_defensas <- df_fbref %>%
  filter(df_fbref$posicion_especifica %in% c('CF', 'SS', 'AMF')) %>%
  select(1, 2, 5,16, 9, 14, 25, 37, 38, 41, 43, 45, 48, 56, 58, 63, 66, 80, 86) %>%
  filter(minutos > .minutes)

##Proceso de filtrado
df_pintar <- df_delanteros
df_pintar <- df_delanteros
df_pintar <- df_delanteros
df_pintar <- df_delanteros

df_pintar[df_delanteros==""]<-0

df_pintar2<- distinct(df_pintar)

Jugadores_Todos <- reshape2::melt(df_pintar2,id.vars=c(1:5))

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

# Lista de jugadores únicos
jugadores_vinotinto <- df_pintar2 %>%
  filter(equipo == "Vinotinto de Ecuador") %>%
  pull(jugador) %>%
  unique()

# Inicializar lista de plots
player_plot <- list()

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

# Iterar sobre cada jugador
for (i in seq_along(jugadores_vinotinto)) {

  jugador_actual <- jugadores_vinotinto[i]

  Pintar_Jugador <- Jugadores_Todos %>%
    filter(jugador == jugador_actual, equipo == "Vinotinto de Ecuador") %>%
    mutate(pais_de_nacimiento = toupper(pais_de_nacimiento) ) %>%
    mutate(value = round(value, 2)) %>%
    inner_join(categoria_metricas %>% select(variable, nombre_variable = nombre, tipo),
               by = "variable") %>%
    distinct() %>%
    mutate(nombre_variable = factor(nombre_variable, levels = nombre_variable[order(Ranking, decreasing = TRUE)]))

  titulo <- paste("**", jugador_actual, "** (", Pintar_Jugador$edad[1], ")", sep = "")
  subtitulo <- paste(Pintar_Jugador$equipo[1], " - ", .ligue, " (", Pintar_Jugador$minutos[1], ") -  min\n",
                     "Players ", nTotal_Jugadores, " Strikers", sep = "")

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
              fontface = "bold.italic", size = 4, color = "black") +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "",
      y = "",
      caption = paste("Source Wyscout - Minutes (", .minutes, ") by: Erick Rangel")
      ) +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.title = ggtext::element_markdown(size = 16, hjust = 0.5, colour = "black"),
      plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "black"),
      panel.border = element_blank(),
      axis.line.y = element_blank(),
      axis.line.x = element_line(),
      axis.text.y = element_text(size = 12),
      legend.title = element_blank(),
      legend.direction = "horizontal",
      legend.position = "bottom",
      plot.margin = unit(c(2, 2, 2, 2), "cm"),
      plot.caption = element_text(
        hjust = c(1, 1),
        size = 8)
    )

  # Nombre del archivo de imagen del jugador
  nombre_archivo <- toupper(gsub("\\.", "", jugador_actual))
  nombre_archivo <- gsub(" ", "_", nombre_archivo) # Asegura que sea compatible con archivo
  nombre_archivo <- paste0(Pintar_Jugador$pais_de_nacimiento,"_", nombre_archivo, ".png")

  h <- cowplot::ggdraw(p) +
    cowplot::draw_image(paste0(.folder_img_r, nombre_archivo),
                        x = -0.04, y = 0.38, scale = 0.14) +
    cowplot::draw_image(paste0(.folder_img_r, "ECU_VIE.png"),
                        x = -0.095, y = 0.38, scale = 0.14) +
    theme(plot.background = element_rect(fill = "white", color = NA))

  h

  ggsave(paste0(.folder_img_r, "plot_", nombre_archivo), plot = h, width = 34, height = 15, units = "cm")

  player_plot[[i]] <- h
}
