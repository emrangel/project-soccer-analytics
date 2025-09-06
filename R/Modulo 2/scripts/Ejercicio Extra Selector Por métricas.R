library(htmlwidgets)
library(parcoords)
library(dplyr)
library(d3r)

US_player_Total <- readRDS("US_player_Total.RData")

selec<-US_player_Total %>%
  filter(year=='2020')

selec1<-selec %>%
  arrange(goals) %>%
  top_n(40,goals)

parcoords(selec1[c(2,5:10,15:18)], 
          rownames = FALSE,
          reorderable  = FALSE,
          queue = T,
          axisDots = T,
          bundleDimension = "xG",
          smoothness=0.2,
          autoresize = T,
          brushMode="2D-strums",
          color= list(
            colorBy="player_name"
            , colorScale = "scaleOrdinal"
            , colorScheme = "schemeCategory10"
          )
          ,withD3 = TRUE
            )
