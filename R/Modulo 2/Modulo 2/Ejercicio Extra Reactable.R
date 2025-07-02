# libraries --------------------

library(reactable)
library(htmltools)
library(dplyr)
library(readxl)
library(readxl)


setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")

setwd(file.path(mainDir))

.path = "Modulo 2/%s"

# ruta/a/imagenes/%s

# Equipos ------------------
# Estad?sticas globales por temporada de cada equipo
US_season_Total <- readRDS("Modulo 2/US_season_Total.RData")
Teams <- read_excel("Modulo 2/Teams.xlsx")


# Procesamiento -------------------
equipos<-US_season_Total %>%
  filter(year=='2020') %>%
  group_by(team_name) %>%
  summarise(partidos=n(),
            puntos=sum(pts),
            xG=sum(xG),
            xGA=sum(xGA),
            GF=sum(scored),
            GC=sum(missed),
            xpts=sum(xpts)) %>%
  mutate(ppp=puntos/partidos,
         xG=xG/partidos,
         xGA=xGA/partidos,
         GF=GF/partidos,
         GC=GC/partidos,
         xpts=xpts/partidos) %>%
  inner_join(Teams, by=c('team_name'='Understat')) %>%
  mutate(badge = case_when(
    Country == 'Germany' ~ paste('GER_',Code,".png",sep=""),
    Country == 'England' ~ paste('ENG_',Code,".png",sep=""),
    Country == 'Spain' ~ paste('ESP_',Code,".png",sep=""),
    Country == 'France' ~ paste('FRA_',Code,".png",sep=""),
    Country == 'Italy' ~ paste('ITA_',Code,".png",sep="")
  ),
  flag = case_when(
    Country == 'Germany' ~ paste('GER.png',sep=""),
    Country == 'England' ~ paste('ENG.png',sep=""),
    Country == 'Spain' ~ paste('ESP.png',sep=""),
    Country == 'France' ~ paste('FRA.png',sep=""),
    Country == 'Italy' ~ paste('ITA.png',sep="")
  )
  ) %>%
  ungroup() %>%
  select(badge,team_name,flag,puntos,ppp,xpts,xG,xGA,GF,GC,partidos) %>%
  arrange(-ppp) %>%
  mutate(Difpts=ppp-xpts,
         Ranking=row_number()) %>%
  select(Ranking,badge,team_name,flag,puntos,ppp,xpts,Difpts,xG,xGA,GF,GC,partidos)


# tabla ---------------------------

reactable(equipos[,1:12],
          fullWidth = FALSE,
          searchable = TRUE,
          resizable = TRUE,
          defaultPageSize = 25,
          highlight = TRUE,
          style = list(fontFamily = "Helvetica", fontSize = "14px"),
          columns = list(
            badge = colDef(align = "center",cell = function(value) {
              img_src <- knitr::image_uri(sprintf(.path, value))
              image <- img(src = img_src, height = "24px")
              tagList(
                div(style = list(display = "inline-block", width = "2px"), image)
              )
            },name=""),
            flag = colDef(align = "center",cell = function(value) {
              img_src <- knitr::image_uri(sprintf(.path,value))
              image <- img(src = img_src, height = "16px")
              tagList(
                div(style = list(display = "inline-block", width = "2px"), image)
              )
            },name="Pais"),
            team_name = colDef(align = "center",
                               # Show species under character names
                               cell = function(value, index) {
                                 species <- equipos$partidos[index]
                                 species <- if (!is.na(species)) species else "Unknown"
                                 tagList(
                                   div(style = list(fontWeight = 600), value),
                                   div(style = list(fontSize = 12), paste(species," partidos",sep="")
                                   ))
                               }
                               ,name = "Equipo"),
            xG = colDef(align = "center",name = "xG p/p",
                        cell = function(value) {
                          label <- paste0(round(value,2))
                        }),
            xGA = colDef(align = "center",name = "xGA p/p",
                         cell = function(value) {
                           label <- paste0(round(value,2))
                         }),
            GF = colDef(align = "center",name = "GF p/p",
                        cell = function(value) {
                          label <- paste0(round(value,2))
                        }),
            GC = colDef(align = "center",name = "GC p/p",
                        cell = function(value) {
                          label <- paste0(round(value,2))
                        }),
            puntos = colDef(align = "center",name = "Puntos Totales",
                            cell = function(value) {
                              label <- paste0(round(value))
                            }),
            xpts = colDef(align = "center",name = "xPTS p/p",
                          cell = function(value) {
                            label <- paste0(round(value,2))
                          }),
            ppp = colDef(align = "center",style = list(background = "rgba(0, 0, 0, 0.03)"),
                         name = "Puntos p/p",
                         cell = function(value) {
                           label <- paste0(round(value,2))
                         }),
            Difpts=colDef(align = "center",name = "Dif Pts-xPTs p/p",
                          cell = function(value) {
                            label <- paste0(round(value,2))
                          },
                          style = function(value) {
                            if (value > 0) {
                              color <- "#008000"
                            } else if (value < 0) {
                              color <- "#e00000"
                            } else {
                              color <- "#777"
                            }
                            list(color = color, fontWeight = "bold")
                          }),
            Ranking = colDef(align = "center",name = "Posición",
                             cell = function(value) {
                               label <- paste0(round(value))
                             })
          ))




equipos<-US_season_Total %>%
  filter(year=='2020') %>%
  group_by(team_name) %>%
  summarise(xpts2 = list(xpts),
            pts2 = list(pts),
            xG2 = list(xG),
            xGA2 = list(xGA),
            scored2 = list(scored),
            missed2 = list(missed),
            xpts1 = mean(xpts),
            pts1 = mean(pts),
            xG1 = mean(xG),
            xGA1 = mean(xGA),
            scored1 = mean(scored),
            missed1 = mean(missed),
            partidos=n()) %>%
  inner_join(Teams, by=c('team_name'='Understat')) %>%
  mutate(badge = case_when(
    Country == 'Germany' ~ paste('GER_',Code,".png",sep=""),
    Country == 'England' ~ paste('ENG_',Code,".png",sep=""),
    Country == 'Spain' ~ paste('ESP_',Code,".png",sep=""),
    Country == 'France' ~ paste('FRA_',Code,".png",sep=""),
    Country == 'Italy' ~ paste('ITA_',Code,".png",sep="")
  ),
  flag = case_when(
    Country == 'Germany' ~ paste('GER.png',sep=""),
    Country == 'England' ~ paste('ENG.png',sep=""),
    Country == 'Spain' ~ paste('ESP.png',sep=""),
    Country == 'France' ~ paste('FRA.png',sep=""),
    Country == 'Italy' ~ paste('ITA.png',sep="")
  )
  ) %>%
  ungroup() %>%
  select(badge,team_name,flag,xpts1,pts1,xG1,xGA1,scored1,missed1,
         xpts2,pts2,xG2,xGA2,scored2,missed2,partidos) %>%
  mutate(g1 = NA, g2 = NA,g3 = NA,
         g4 = NA,g5 = NA,g6 = NA) %>%
  select(badge,team_name,flag,xpts1,g1,pts1,g2,xG1,g3,xGA1,g4,scored1,g5,
         missed1,g6,
         xpts2,pts2,xG2,xGA2,scored2,missed2,partidos)

library(sparkline)

reactable(equipos[,1:15],
          fullWidth = TRUE,
          searchable = TRUE,
          resizable = TRUE,
          defaultPageSize = 25,
          highlight = TRUE,
          style = list(fontFamily = "Helvetica", fontSize = "14px"),
          columns = list(
            badge = colDef(align = "center",minWidth = 50,cell = function(value) {
              img_src <- knitr::image_uri(sprintf(value))
              image <- img(src = img_src, height = "24px")
              tagList(
                div(style = list(display = "inline-block", width = "2px"), image)
              )
            },name=""),
            flag = colDef(align = "center",minWidth = 50,cell = function(value) {
              img_src <- knitr::image_uri(sprintf(value))
              image <- img(src = img_src, height = "16px")
              tagList(
                div(style = list(display = "inline-block", width = "2px"), image)
              )
            },name="Pais"),
            team_name = colDef(align = "center",
                               # Show species under character names
                               cell = function(value, index) {
                                 species <- equipos$partidos[index]
                                 species <- if (!is.na(species)) species else "Unknown"
                                 tagList(
                                   div(style = list(fontWeight = 600), value),
                                   div(style = list(fontSize = 12), paste(species," partidos",sep="")
                                   ))
                               }
                               ,name = "Equipo"),
            xG1 = colDef(align = "center",minWidth = 50,name = "xG",
                        cell = function(value) {
                          label <- paste0(round(value,2))
                        }),
            g3 = colDef(align = "center",name = "",cell = function(value, index) {
              sparkline(equipos$xG2[[index]], type = "line"
                        )
            }),
            xGA1 = colDef(align = "center",minWidth = 50,name = "xGA",aggregate = "mean",
                         cell = function(value) {
                           label <- paste0(round(value,2))
                         }),
            g4 = colDef(align = "center",name = "",cell = function(value, index) {
              sparkline(equipos$xGA2[[index]], type = "line")
            }),
            scored1 = colDef(align = "center",minWidth = 50,name = "GF",aggregate = "mean",
                        cell = function(value) {
                          label <- paste0(round(value,2))
                        }),
            g5 = colDef(align = "center",name = "",cell = function(value, index) {
              sparkline(equipos$scored2[[index]], type = "box")
            }),
            missed1 = colDef(align = "center",minWidth = 50,name = "GC",aggregate = "mean",
                        cell = function(value) {
                          label <- paste0(round(value,2))
                        }),
            g6 = colDef(align = "center",name = "",cell = function(value, index) {
              sparkline(equipos$missed2[[index]], type = "box")
            }),
            pts1 = colDef(align = "center",minWidth = 50,name = "Puntos",
                            cell = function(value) {
                              label <- paste0(round(value,2))
                            }),
            g2 = colDef(align = "center",name = "",cell = function(value, index) {
              sparkline(equipos$pts2[[index]], type = "bar")
            }),
            xpts1 = colDef(align = "center",minWidth = 50,name = "xPTS",aggregate = "mean",
                          cell = function(value) {
                            label <- paste0(round(value,2))
                          }),
            g1 = colDef(align = "center",name = "",cell = function(value, index) {
              sparkline(equipos$xpts2[[index]], type = "box")
            })
          ))
