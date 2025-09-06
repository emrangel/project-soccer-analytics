if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, mainly, dplyr, purrr, tidyr, StatsBombR, SBpitch, soccermatics,
               extrafont, ggupset, tibbletime,
               ggtext, ggrepel, glue,
               patchwork, cowplot, gtable, grid,
               magick)

library(dplyr)
library(StatsBombR)
comps <- FreeCompetitions()

glimpse(comps)

messi_matches_raw <- comps %>%
  filter(competition_id == 11) %>%
  FreeMatches()

messi_data_raw <- StatsBombFreeEvents(MatchesDF = messi_matches_raw)


messi_data_clean <- messi_data_raw %>%
  allclean() %>%
  left_join(comps %>% select(season_id, season_name), by = "season_id")


{messi_data_clean <- messi_data_clean %>%
  ## player name
  mutate(player.name = case_when(
    player.name == "Oleguer Presas Renom" ~ "Oleguer",
    player.name == "Xavier Hernández Creus" ~ "Xavi",
    player.name == "Carles Puyol i Saforcada" ~ "Carles Puyol",
    player.name == "Anderson Luís de Souza" ~ "Deco",
    player.name == "Rafael Márquez Álvarez" ~ "Rafa Márquez",
    player.name == "Giovanni van Bronckhorst" ~ "Gio v.Bronckhorst",
    player.name == "Samuel Eto'o Fils" ~ "Samuel Eto'o",
    player.name == "Víctor Valdés Arribas" ~ "Víctor Valdés",
    player.name == "Juliano Haus Belletti" ~ "Juliano Belletti",
    player.name == "Ludovic Giuly" ~ "Ludovic Giuly",
    player.name == "Andrés Iniesta Luján" ~ "Andrés Iniesta",
    player.name == "Ronaldo de Assis Moreira" ~ "Ronaldinho",
    player.name == "Lionel Andrés Messi Cuccittini" ~ "Lionel Messi",
    player.name == "Fernando Navarro i Corbacho" ~ "Fernando Navarro",
    player.name == "Sylvio Mendes Campos Junior" ~ "Sylvinho",
    player.name == "Damià Abella Pérez" ~ "Damià",
    player.name == "Rubén Iván Martínez Andrade" ~ "Ronaldinho",
    player.name == "Ronaldo de Assis Moreira" ~ "Rubén",
    player.name == "Thiago Motta" ~ "Thiago Motta",
    player.name == "Mark van Bommel" ~ "Mark van Bommel",
    player.name == "Henrik Larsson" ~ "Henrik Larsson",
    player.name == "José Edmílson Gomes de Moraes" ~ "Edmílson",
    player.name == "Gabriel Francisco García de la Torre" ~ "Gabri",
    player.name == "Santiago Ezquerro Marín" ~ "Santi Ezquerro",
    player.name == "Maximiliano Gastón López" ~ "Maxi López",
    player.name == "Gianluca Zambrotta" ~ "Gianluca Zambrotta",
    player.name == "Eiður Smári Guðjohnsen" ~ "Eiður Guðjohnsen",
    player.name == "Lilian Thuram" ~ "Lilian Thuram",
    player.name == "Javier Pedro Saviola Fernández" ~ "Javier Saviola",
    player.name == "Gnégnéri Yaya Touré" ~ "Yaya Touré",
    player.name == "Bojan Krkíc Pérez" ~ "Bojan",
    player.name == "Eric-Sylvain Bilal Abidal" ~ "Eric Abidal",
    player.name == "Gabriel Alejandro Milito" ~ "Gabriel Milito",
    player.name == "Giovani dos Santos Ramírez" ~ "Giovani dos Santos",
    player.name == "Víctor Vázquez Solsona" ~ "Víctor Vázquez",
    player.name == "Thierry Henry" ~ "Thierry Henry",
    player.name == "José Manuel Pinto Colorado" ~ "José Manuel Pinto",
    player.name == "Daniel Alves da Silva" ~ "Dani Alves",
    player.name == "Sergio Busquets i Burgos" ~ "Sergio Busquets",
    player.name == "Seydou Kéita" ~ "Seydou Kéita",
    player.name == "José Martín Cáceres Silva" ~ "Martín Cáceres",
    player.name == "Gerard Piqué Bernabéu" ~ "Gerard Piqué",
    player.name == "Aliaksandr Hleb" ~ "Aliaksandr Hleb",
    player.name == "Pedro Eliezer Rodríguez Ledesma" ~ "Pedro",
    player.name == "Sergio Rodríguez García" ~ "Rodri",
    player.name == "Rafael Romero Serrano" ~ "Fali",
    player.name == "José Manuel Rueda Sampedro" ~ "José Manuel Rueda",
    player.name == "Zlatan Ibrahimovic" ~ "Zlatan Ibrahimovic",
    player.name == "Dmytro Chygrynskiy" ~ "Dmytro Chygrynskiy",
    player.name == "Maxwell Scherrer Cabelino Andrade" ~ "Maxwell",
    player.name == "Jeffren Isaac Suárez Bermúdez" ~ "Jeffren",
    player.name == "Víctor Sánchez Mata" ~ "Víctor Sánchez",
    player.name == "Thiago Alcântara do Nascimento" ~ "Thiago Alcântara",
    player.name == "David Villa Sánchez" ~ "David Villa",
    player.name == "Javier Alejandro Mascherano" ~ "Javier Mascherano",
    player.name == "Andreu Fontàs Prat" ~ "Andreu Fontàs",
    player.name == "Ibrahim Afellay" ~ "Ibrahim Afellay",
    player.name == "Manuel Agudo Durán" ~ "Nolito",
    player.name == "Marc Bartra Aregall" ~ "Marc Bartra",
    player.name == "Adriano Correia Claro" ~ "Adriano",
    player.name == "Martín Montoya Torralbo" ~ "Martín Montoya",
    player.name == "Jonathan dos Santos Ramírez" ~ "Jonathan dos Santos",
    player.name == "Francesc Fàbregas i Soler" ~ "Cesc Fàbregas",
    player.name == "Alexis Alejandro Sánchez Sánchez" ~ "Alexis Sánchez",
    player.name == "Juan Isaac Cuenca López" ~ "Isaac Cuenca",
    player.name == "Gerard Deulofeu Lázaro" ~ "Gerard Deulofeu",
    player.name == "Cristian Tello" ~ "Cristian Tello",
    player.name == "Sergi Roberto Carnicer" ~ "Sergi Roberto",
    player.name == "Marc Muniesa Martínez" ~ "Marc Muniesa",
    TRUE ~ player.name
  )) %>%
  ## pass.recipient.name
  mutate(pass.recipient.name = case_when(
    pass.recipient.name == "Oleguer Presas Renom" ~ "Oleguer",
    pass.recipient.name == "Xavier Hernández Creus" ~ "Xavi",
    pass.recipient.name == "Carles Puyol i Saforcada" ~ "Carles Puyol",
    pass.recipient.name == "Anderson Luís de Souza" ~ "Deco",
    pass.recipient.name == "Rafael Márquez Álvarez" ~ "Rafa Márquez",
    pass.recipient.name == "Giovanni van Bronckhorst" ~ "Gio v.Bronckhorst",
    pass.recipient.name == "Samuel Eto'o Fils" ~ "Samuel Eto'o",
    pass.recipient.name == "Víctor Valdés Arribas" ~ "Víctor Valdés",
    pass.recipient.name == "Juliano Haus Belletti" ~ "Juliano Belletti",
    pass.recipient.name == "Ludovic Giuly" ~ "Ludovic Giuly",
    pass.recipient.name == "Andrés Iniesta Luján" ~ "Andrés Iniesta",
    pass.recipient.name == "Ronaldo de Assis Moreira" ~ "Ronaldinho",
    pass.recipient.name == "Lionel Andrés Messi Cuccittini" ~ "Lionel Messi",
    pass.recipient.name == "Fernando Navarro i Corbacho" ~ "Fernando Navarro",
    pass.recipient.name == "Sylvio Mendes Campos Junior" ~ "Sylvinho",
    pass.recipient.name == "Damià Abella Pérez" ~ "Damià",
    pass.recipient.name == "Rubén Iván Martínez Andrade" ~ "Ronaldinho",
    pass.recipient.name == "Ronaldo de Assis Moreira" ~ "Rubén",
    pass.recipient.name == "Thiago Motta" ~ "Thiago Motta",
    pass.recipient.name == "Mark van Bommel" ~ "Mark van Bommel",
    pass.recipient.name == "Henrik Larsson" ~ "Henrik Larsson",
    pass.recipient.name == "José Edmílson Gomes de Moraes" ~ "Edmílson",
    pass.recipient.name == "Gabriel Francisco García de la Torre" ~ "Gabri",
    pass.recipient.name == "Santiago Ezquerro Marín" ~ "Santi Ezquerro",
    pass.recipient.name == "Maximiliano Gastón López" ~ "Maxi López",
    pass.recipient.name == "Gianluca Zambrotta" ~ "Gianluca Zambrotta",
    pass.recipient.name == "Eiður Smári Guðjohnsen" ~ "Eiður Guðjohnsen",
    pass.recipient.name == "Lilian Thuram" ~ "Lilian Thuram",
    pass.recipient.name == "Javier Pedro Saviola Fernández" ~ "Javier Saviola",
    pass.recipient.name == "Gnégnéri Yaya Touré" ~ "Yaya Touré",
    pass.recipient.name == "Bojan Krkíc Pérez" ~ "Bojan",
    pass.recipient.name == "Eric-Sylvain Bilal Abidal" ~ "Eric Abidal",
    pass.recipient.name == "Gabriel Alejandro Milito" ~ "Gabriel Milito",
    pass.recipient.name == "Giovani dos Santos Ramírez" ~ "Giovani dos Santos",
    pass.recipient.name == "Víctor Vázquez Solsona" ~ "Víctor Vázquez",
    pass.recipient.name == "Thierry Henry" ~ "Thierry Henry",
    pass.recipient.name == "José Manuel Pinto Colorado" ~ "José Manuel Pinto",
    pass.recipient.name == "Daniel Alves da Silva" ~ "Dani Alves",
    pass.recipient.name == "Sergio Busquets i Burgos" ~ "Sergio Busquets",
    pass.recipient.name == "Seydou Kéita" ~ "Seydou Kéita",
    pass.recipient.name == "José Martín Cáceres Silva" ~ "Martín Cáceres",
    pass.recipient.name == "Gerard Piqué Bernabéu" ~ "Gerard Piqué",
    pass.recipient.name == "Aliaksandr Hleb" ~ "Aliaksandr Hleb",
    pass.recipient.name == "Pedro Eliezer Rodríguez Ledesma" ~ "Pedro",
    pass.recipient.name == "Sergio Rodríguez García" ~ "Rodri",
    pass.recipient.name == "Rafael Romero Serrano" ~ "Fali",
    pass.recipient.name == "José Manuel Rueda Sampedro" ~ "José Manuel Rueda",
    pass.recipient.name == "Zlatan Ibrahimovic" ~ "Zlatan Ibrahimovic",
    pass.recipient.name == "Dmytro Chygrynskiy" ~ "Dmytro Chygrynskiy",
    pass.recipient.name == "Maxwell Scherrer Cabelino Andrade" ~ "Maxwell",
    pass.recipient.name == "Jeffren Isaac Suárez Bermúdez" ~ "Jeffren",
    pass.recipient.name == "Víctor Sánchez Mata" ~ "Víctor Sánchez",
    pass.recipient.name == "Thiago Alcântara do Nascimento" ~ "Thiago Alcântara",
    pass.recipient.name == "David Villa Sánchez" ~ "David Villa",
    pass.recipient.name == "Javier Alejandro Mascherano" ~ "Javier Mascherano",
    pass.recipient.name == "Andreu Fontàs Prat" ~ "Andreu Fontàs",
    pass.recipient.name == "Ibrahim Afellay" ~ "Ibrahim Afellay",
    pass.recipient.name == "Manuel Agudo Durán" ~ "Nolito",
    pass.recipient.name == "Marc Bartra Aregall" ~ "Marc Bartra",
    pass.recipient.name == "Adriano Correia Claro" ~ "Adriano",
    pass.recipient.name == "Martín Montoya Torralbo" ~ "Martín Montoya",
    pass.recipient.name == "Jonathan dos Santos Ramírez" ~ "Jonathan dos Santos",
    pass.recipient.name == "Francesc Fàbregas i Soler" ~ "Cesc Fàbregas",
    pass.recipient.name == "Alexis Alejandro Sánchez Sánchez" ~ "Alexis Sánchez",
    pass.recipient.name == "Juan Isaac Cuenca López" ~ "Isaac Cuenca",
    pass.recipient.name == "Gerard Deulofeu Lázaro" ~ "Gerard Deulofeu",
    pass.recipient.name == "Cristian Tello" ~ "Cristian Tello",
    pass.recipient.name == "Sergi Roberto Carnicer" ~ "Sergi Roberto",
    pass.recipient.name == "Marc Muniesa Martínez" ~ "Marc Muniesa",
    TRUE ~ pass.recipient.name
  ))
}


saveRDS(messi_data_clean,file="messi_data_clean.RDS")


clasico_1112 <- messi_data_clean %>%
  filter(match_id == 69334) %>%
  mutate(shot.statsbomb_xg = if_else(is.na(shot.statsbomb_xg),
                                     0, shot.statsbomb_xg))

clasico_1112_xg <- clasico_1112 %>%
  group_by(team.name) %>%
  summarize(tot_xg = sum(shot.statsbomb_xg) %>% signif(digits = 2)) %>%
  mutate(team_label = glue::glue("{team.name}: {tot_xg} xG"))

clasico_1112 <- clasico_1112 %>%
  left_join(clasico_1112_xg, by = "team.name") %>%
  mutate(player_label = case_when(
    shot.outcome.name == "Goal" ~ glue::glue("{player.name}: {shot.statsbomb_xg %>% signif(digits = 2)} xG"),
    TRUE ~ ""))
