
partidos_1<-readRDS("partidos_valencia_bruto.rds")

partidos_1$Fecha<- gsub('\\.', '-', partidos_1$Fecha)
partidos_1$Fecha<-as.Date(partidos_1$Fecha,"%d-%m-%Y")
partidos_1<-partidos_1 %>%
  separate(Res, c("Home_team","Away_team"), "-")
partidos_1$Home_team<-as.numeric(partidos_1$Home_team)
partidos_1$Away_team<-as.numeric(partidos_1$Away_team)

partidos_1<-partidos_1 %>%
  mutate(puntos=case_when(
    Local=='Valencia CF' & Home_team>Away_team ~ 3,
    Home_team==Away_team ~ 1,
    Visitante=='Valencia CF' & Home_team<Away_team ~ 3,
    TRUE ~ 0
  ))


partidos_final<-partidos_1 %>%
  group_by(season) %>%
  arrange(Fecha) %>%
  mutate(jornada=row_number(),
         puntos_total=cumsum(puntos))

library(readxl)
Temporadas <- read_excel("Temporadas_id.xlsx")

partidos_final<-partidos_final %>%
  inner_join(Temporadas,by=c('season'='id_season'))



my_colors <- c(
  "green"      = rgb(103,180,75, maxColorValue = 256),
  "green2"      = rgb(147,198,44, maxColorValue = 256),
  "lightblue"  =  rgb(9, 177,240, maxColorValue = 256),
  "lightblue2" = rgb(173,216,230, maxColorValue = 256),
  'blue'       = "#00aedb",
  'red'        = "#d11141",
  'orange'     = "#f37735",
  'yellow'     = "#ffc425",
  'gold'       = "#FFD700",
  'light grey' = "#cccccc",
  'purple'     = "#551A8B",
  'dark grey'  = "#8c8c8c")


my_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (my_colors)
  my_colors[cols]
}


my_palettes <- list(
  `main`  = my_cols("blue", "green", "yellow"),
  `cool`  = my_cols("blue", "green"),
  `cool2hot` = my_cols("lightblue2","lightblue", "blue","green", "green2","yellow","gold", "orange", "red"),
  `hot`   = my_cols("yellow", "orange", "red"),
  `mixed` = my_cols("lightblue", "green", "yellow", "orange", "red"),
  `mixed2` = my_cols("lightblue2","lightblue", "green", "green2","yellow","gold", "orange", "red"),
  `mixed3` = my_cols("lightblue2","lightblue", "green", "yellow","gold", "orange", "red"),
  `mixed4` = my_cols("lightblue2","lightblue", "green", "green2","yellow","gold", "orange", "red","purple"),
  `mixed5` = my_cols("lightblue","green", "green2","yellow","gold", "orange", "red","purple","blue"),
  `mixed6` = my_cols("green", "gold", "orange", "red","purple","blue"),
  `grey`  = my_cols("light grey", "dark grey")
)


my_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- my_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}


scale_color_mycol <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- my_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("my_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}



scale_fill_mycol <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- my_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("my_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# dark theme ----
theme_dark2 = function(base_size = 12, base_family = "Courier New") {

  theme_grey(base_size = base_size, base_family = base_family) %+replace%

    theme(
      # Specify axis options
      axis.line = element_blank(),
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
      axis.ticks = element_line(color = "white", size  =  0.2),
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),
      axis.ticks.length = unit(0.3, "lines"),
      # Specify legend options
      legend.background = element_rect(color = NA, fill = " gray10"),
      legend.key = element_rect(color = "white",  fill = " gray10"),
      legend.key.size = unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = base_size*0.8, color = "white"),
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),
      legend.position = "right",
      legend.text.align = NULL,
      legend.title.align = NULL,
      legend.direction = "vertical",
      legend.box = NULL,
      # Specify panel options
      panel.background = element_rect(fill = " gray10", color  =  NA),
      #panel.border = element_rect(fill = NA, color = "white"),
      panel.border=element_blank(),
      panel.grid.major = element_line(color = "grey35"),
      panel.grid.minor = element_line(color = "grey20"),
      panel.spacing = unit(0.5, "lines"),
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),
      strip.text.x = element_text(size = base_size*0.8, color = "white"),
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),
      # Specify plot options
      plot.background = element_rect(color = " gray10", fill = " gray10"),
      plot.title = element_text(size = base_size*1.2, color = "white",hjust=0,lineheight=1.25,
                                margin=margin(2,2,2,2)),
      plot.subtitle = element_text(size = base_size*1, color = "white",hjust=0,  margin=margin(2,2,2,2)),
      plot.caption = element_text(size = base_size*0.8, color = "white",hjust=0),
      plot.margin = unit(rep(1, 4), "lines")

    )

}

# Plot ----
ggplot(data=filter(partidos_final,year>=1996), aes(y=as.factor(year),x=as.factor(jornada),fill=puntos_total, label=puntos_total))+
  scale_fill_mycol(palette="cool2hot",discrete=FALSE,reverse=FALSE,limits=c(0,84),
                   name="")+
  geom_tile()+
  geom_tile(color="white",size=0.5)+
  geom_tile(color="black",size=0.5,fill=NA)+
  geom_text(color="black",size=3)+
  theme_dark2(base_family="Courier New")+
  theme(legend.position="top",
        legend.direction="horizontal",
        panel.border = element_rect(fill = NA, color = NA),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        panel.grid.major.y=element_blank(),
        legend.key.width=unit(1,"cm"))+
  labs(x="",y="",title="Puntos Acumulados por jornadas del Valencia CF en 1ª División",
       caption = "Elaborado por Jesús Lagos @Vdot_Spain a partir de @lenkiefer)Datos: Ciberche")






# contra los grandes

partidos_1<-readRDS("partidos_valencia_bruto.rds")

partidos_1$Fecha<- gsub('\\.', '-', partidos_1$Fecha)
partidos_1$Fecha<-as.Date(partidos_1$Fecha,"%d-%m-%Y")
partidos_1<-partidos_1 %>%
  separate(Res, c("Home_team","Away_team"), "-")
partidos_1$Home_team<-as.numeric(partidos_1$Home_team)
partidos_1$Away_team<-as.numeric(partidos_1$Away_team)

rivales<-c('Atlético de Madrid','Real Madrid','Athletic de Bilbao','FC Barcelona','Real Sociedad',
           'Sevilla FC','Real Betis')

partidos_1<-partidos_1 %>%
  filter(Local %in% rivales | Visitante %in% rivales )


partidos_1<-partidos_1 %>%
  mutate(
    puntos=case_when(
      Local=='Valencia CF' & Home_team>Away_team ~ 3,
      Home_team==Away_team ~ 1,
      Visitante=='Valencia CF' & Home_team<Away_team ~ 3,
      TRUE ~ 0),
    rival=case_when(
      Local=='Valencia CF' ~ Visitante,
      TRUE ~ Local)
    )

partidos_final <- partidos_1 %>%
  group_by(rival,season) %>%
  summarise(ppp=mean(puntos))


library(readxl)
Temporadas <- read_excel("Temporadas_id.xlsx")

partidos_final<-partidos_final %>%
  inner_join(Temporadas,by=c('season'='id_season'))


# Plot ----
ggplot(data=filter(partidos_final,year>=1900), aes(y=as.factor(year),x=as.factor(rival),fill=ppp, label=ppp))+
  scale_fill_mycol(palette="cool",discrete=FALSE,reverse=FALSE,limits=c(0,3),
                   name="")+
  geom_tile()+
  geom_tile(color="white",size=0.5)+
  geom_tile(color="black",size=0.5,fill=NA)+
  geom_text(color="black",size=1)+
  theme_dark2(base_family="Courier New")+
  theme(legend.position="top",
        legend.direction="horizontal",
        panel.border = element_rect(fill = NA, color = NA),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 8),
        panel.grid.major.y=element_blank(),
        legend.key.width=unit(1,"cm"))+
  labs(x="",y="",title="Puntos Por Partido principales rivales del Valencia CF en 1ª División",
       caption = "Elaborado por Jesús Lagos @Vdot_Spain a partir de @lenkiefer)Datos: Ciberche")



