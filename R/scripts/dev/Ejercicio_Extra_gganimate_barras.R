library(dplyr)
library(stringr)
library(ggplot2)
library(gganimate)



setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")



partidos<-readRDS('Total_jugadores.rds')
Todo_goles1<-readRDS('player_goals_competition_season.rds')

Grafico_Valencia<-filter(Todo_goles1,(grupo_compe=="Nacional" | grupo_compe=='Europa') & Partidos>0)


pts_df <- Grafico_Valencia %>%
  select(Nombre, Goles, Partidos, year) %>%
  group_by(Nombre,year) %>%
  summarise(Partidos=sum(Partidos),Goles=sum(Goles))

pts_df_sum <- pts_df %>%
  mutate(pts = as.numeric(Goles), year = as.numeric(year),pj=as.numeric(Partidos)) %>%
  group_by(Nombre) %>%
  mutate(cum_sum_pts = cumsum(pts),cumsum_pj=cumsum(pj)) %>%
  ungroup()

pts_df_sum <- pts_df_sum %>%
  mutate(decada=case_when(
    year<1950 ~ "A?os 20-49",
    year<1970 ~ "A?os 50-69",
    year<1990 ~ "A?os 70-89",
    year<2010 ~ "A?os 90-09",
    year<2030 ~ "A?os 10-30"
  )
  )


pts_df_sum<-filter(pts_df_sum,cum_sum_pts<0)

pts_df_sum2 <- pts_df_sum %>%
  filter(year==1923) %>%
  top_n(-10, wt=cum_sum_pts) %>%
  arrange(desc(cum_sum_pts)) %>%
  mutate(curr_year = 1923,
         ordering = as.double(row_number()) * 1.0)



yrs <- sort(unique(pts_df_sum$year))

for (i in 2:length(yrs)) {
  tmp_df <- pts_df_sum %>%
    filter(year <= yrs[i]) %>%
    group_by(Nombre) %>%
    filter(cum_sum_pts==min(cum_sum_pts) & cumsum_pj==max(cumsum_pj)) %>%
    ungroup() %>%
    top_n(-10, wt=cum_sum_pts) %>%
    arrange((cum_sum_pts)) %>%
    mutate(curr_year = yrs[i],
           ordering = as.double(row_number()) * 1.0)

  pts_df_sum2 <- pts_df_sum2 %>%
    bind_rows(tmp_df)
  print(yrs[i])
}

pts_df_sum2 <- pts_df_sum2 %>%
  mutate(yr_suffix = curr_year - 1,
         yr_label = paste0(yr_suffix, '-', curr_year))


my_theme <- theme(text = element_text(face = 'bold'),
                  plot.background = element_rect(color = NA),
                  panel.background = element_rect(color = NA),
                  panel.border = element_blank(),
                  plot.title = element_text(face = 'bold', size = 20),
                  plot.subtitle = element_text(size = 14),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_line(color = 'grey75'),
                  panel.grid.minor.x = element_line(color = 'grey75'),
                  legend.position = 'none',
                  plot.caption = element_text(size = 8),
                  axis.ticks = element_blank(),
                  axis.text.y =  element_blank())



theme_set(theme_light() + my_theme)

pts_df_sum2 <- pts_df_sum2 %>% filter(ordering < 21)


pts_df_sum2$cum_sum_pts<-pts_df_sum2$cum_sum_pts*(-1)

valenciacf_goles <- ggplot(aes(ordering, group = Nombre), data = pts_df_sum2)+
  geom_tile(aes(y = cum_sum_pts / 2,
                 height = cum_sum_pts,
                width = 0.9, fill=decada), alpha = 0.9) +
  geom_text(aes(y = cum_sum_pts, label = paste(Nombre,' - PJ ',cumsum_pj,' - Gol/pp ',format(round(cum_sum_pts/cumsum_pj, 2), nsmall = 2),sep='')), fontface = "bold",nudge_y = -75, size = 3) +
  geom_text(aes(y = cum_sum_pts, label = as.character(cum_sum_pts)), fontface = "bold",nudge_y = 20) +
  geom_text(aes(x=9,y=300, label=paste0(yr_label)), size=8,fontface = "bold", color = 'gray45') +
  coord_cartesian(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  coord_flip() +
  labs(title = 'Goles en Contra Porteros (PJ y Gpp) All-Time Valencia CF',
       caption = 'data source: ciberche.net | plot by @vdot_spain | Competiciones Nacionales y Europeas',
       x = '',
       y = '') +
  transition_states(curr_year,
                    transition_length = 4, state_length = 3) +
  ease_aes('cubic-in-out')

animate(valenciacf_goles, nframes = 100, fps = 100, width = 800, height = 600, res=80, detail = 3)

anim_save("valenciacf_goles.gif")


