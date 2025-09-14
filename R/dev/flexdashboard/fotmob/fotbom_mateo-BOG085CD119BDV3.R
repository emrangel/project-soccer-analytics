# 0 Global
rm(list = ls(all = T))

#00 libraries -------
getwd()
# source('flexdashboard/fotmob/pitch_fotmob.R')
library('rvest')
library('dplyr')
library('ggplot2')
library("ggrepel")
library("jsonlite")
library("SBpitch")
library(tidyr)
library(stringr)
# devtools::install_github("FCrSTATS/SBpitch")

# data ----------------------

df_leagues = data.table::fread("all_leagues.csv")

# 01 Process ----------------

# path url 
# url_matches_league_season_col = 'https://www.fotmob.com/api/leagues?id=274&season=2023+-+Clausura' # url especific league and season
# tt = 'https://www.fotmob.com/api/matchDetails?matchId=4371000' # url especific league and season
# tt = 'https://www.fotmob.com/es/match/4371000/' # url especific league and season

## 02 matches ----------
rm(list = ls(all = T))

url_matches_league = paste0('https://www.fotmob.com/api/leagues?id=',47) # url especific league

matches_league <- fromJSON(url_matches_league)

length(matches_league)

matches <- matches_league[["matches"]] %>% as.data.frame() %>% janitor::clean_names()

matches %>% View()

## 03 matches season----------
rm(list = ls(all = T))

url_matches_league_season = paste0('https://www.fotmob.com/api/leagues?id=',47,'&season=2023%2F2024') # url especific league and season

matches_league_season <- fromJSON(url_matches_league_season)

length(matches_league_season)

matches <- matches_league_season[["matches"]] %>% as.data.frame() %>% janitor::clean_names()

matches %>% View()

## 04 matches today ----------
rm(list = ls(all = T))

json_matches_today = "https://www.fotmob.com/api/matches?" # url especific matches today

matches_today <- fromJSON(json_matches_today)

length(matches_today)

leagues_matches_today = matches_today$leagues %>% 
  as.data.frame() %>%
  # select(ccode, primaryId,id, name, , parentLeagueName) %>%
  janitor::clean_names() %>% 
  distinct()

# data.table::fwrite(leagues_matches_today,"flexdashboard/fotmob/out/fotmob_id_leagues.csv")


## 05 data match shotmap ---------------------------------------------------------------------------------------
rm(list = ls(all = T))

url_stats_match = paste0('https://www.fotmob.com/api/matchDetails?matchId=',4205546) # url especific league and season
  # https://www.fotmob.com/matches/real-madrid-vs-almeria/2tvf3g#4205546
stats_match <- fromJSON(url_stats_match)

shots = stats_match$content$shotmap$shots
teams = stats_match$header$teams %>% mutate(home_away = c(1,2))
hight = stats_match$content$matchFacts$highlights$image

shots = left_join(shots, teams, by = c("teamId"="id"))
shots %>% head(2)

shots$eventType

max(shots$x)
min(shots$x)

max(shots$y)
min(shots$y)

summary(shots %>% select(x,y))

df_tiros = shots

logo_team1 = df_tiros %>% filter(home_away == 1 ) %>% distinct(imageUrl)
gol_team1 <- df_tiros %>% filter(eventType == 'Goal' & home_away == 1)
no_gol_team1 <- df_tiros %>% filter(eventType != 'Goal' & home_away == 1)

logo_team2 = df_tiros %>% filter(home_away == 2)%>% distinct(imageUrl)
gol_team2 <- df_tiros %>% filter(eventType == 'Goal' & home_away == 2)
no_gol_team2 <- df_tiros %>% filter(eventType != 'Goal' & home_away == 2)

# Crear el gráfico
blank_pitch <- create_Pitch(
  goaltype = "box",
  grass_colour = "#202020", 
  line_colour = "#797876", 
  background_colour = "#202020", 
  goal_colour = "#131313",
)


# blank_pitch <- create_Pitch(
#   grass_colour = "#538032",
#   line_colour =  "#ffffff",
#   background_colour = "#538032",
#   goal_colour = "#000000"
# )

# Dibujar el gráfico de puntos
blank_pitch + 
  geom_point(data = gol_team1, aes(x = 105 - x , y = 73 - y),shape = "\U26BD",size = 4, color = "#94c7c3") +
  geom_point(data = no_gol_team1, aes(x = 105 - x , y = 73 - y), color = "#eb9293", alpha = 0.6) +
  geom_point(data = gol_team2, aes(x = x + 15, y = y + 8),shape = "\U26BD",size = 4,  color = "#94c7c3") +
  geom_point(data = no_gol_team2, aes(x = x + 15, y = y + 8), color = "#eb9293", alpha = 0.6) +
  cowplot::draw_image(logo_team2$imageUrl, 
                x = 110, y = 1, width = 7.5, height = 7.5) +
  cowplot::draw_image(logo_team1$imageUrl, 
                      x = 105- 100, y = 1, width = 7.5, height = 7.5) +
  cowplot::draw_image(hight, 
                      x = 40, y = -10, width = 40, height = 50)
  



## 06 data match stats ---------------------------------------------------------------------------------------
rm(list = ls(all = T))

url_stats_match = paste0('https://www.fotmob.com/api/matchDetails?matchId=',4205546) # url especific league and season
url_stats_match = paste0('https://www.fotmob.com/api/matchDetails?matchId=',4193450) # url especific league and season
# url_stats_player = paste0("https://www.fotmob.com/api/playerStats?playerId=1077894&seasonId=2023%2F2024-87") # url especific league and season
# url_stats_player = paste0("https://www.fotmob.com/api/playerData?id=1077894") # url especific league and season
# https://www.fotmob.com/matches/real-madrid-vs-almeria/2tvf3g#4205546
stats_match <- fromJSON(url_stats_match)

teams = stats_match$header$teams %>% mutate(name_team = c("home","away")) %>% select(name, name_team)
hight = stats_match$content$matchFacts$highlights$image

### prueba ------

dd_final <- data.frame()  # Inicializar un dataframe vacío

# Iterar sobre las diferentes secciones de stats
# for (i in max(seq_along(stats_match$content$stats$Periods$All$stats$stats))) {
for (i in seq_along(stats_match$content$stats$Periods$All$stats$stats)) {
  dd <- stats_match$content$stats$Periods$All$stats$stats[[i]]
  
  dd <- bind_rows(dd %>% mutate(name_team = "home"),
                  dd %>% mutate(name_team = "away"))
  
  dd <- dd %>% 
    mutate(home = lapply(stats, function(x) x[1] %>% unlist),
           away = lapply(stats, function(x) x[2] %>% unlist)) %>%
    mutate(stats = ifelse(name_team == "home", home, away))
  
  dd <- left_join(dd, teams, by = c("name_team" = "name_team")) %>%
    select(title, key, stats, name)
  
  dd_final <- bind_rows(dd_final, dd)
}

dd <- stats_match[["content"]][["lineup"]][["lineup"]][["bench"]][[1]][["stats"]] %>% as.data.frame()
dd <- stats_match[["content"]][["lineup"]][["lineup"]][["bench"]][[2]] %>% as.data.frame()
dd <- stats_match[["content"]][["lineup"]][["lineup"]][["bench"]][[1]][["stats"]][[1]] %>% as.data.frame()

##### termina la prueba- -----


rm(dd1, dd2, dd3)
str_extract('553 (87%)', "(\\()")

dd2 = stats_match$content$stats$Periods$All$stats$stats[[2]]

dd2 = dd2 %>% mutate(home = lapply(stats, function(x) x[1] %>% unlist),
                   away = lapply(stats, function(x) x[2] %>% unlist),)

dd3 = stats_match$content$stats$Periods$All$stats$stats[[3]]

dd3 = dd3 %>% mutate(home = lapply(stats, function(x) x[1] %>% unlist),
                     away = lapply(stats, function(x) x[2] %>% unlist),)


# Try ---------------
## html _next ----------------------
# HTML como cadena de texto
url <- "https://www.fotmob.com/"
url <- "https://www.fotmob.com/matches/real-madrid-vs-almeria/2tvf3g#4205546"

# Parsear HTML
html <- read_html(url)

# Imprime el HTML
print(html)

# Encontrar el script con el id '__NEXT_DATA__'
script_content <- html_text(html_nodes(html, "script#__NEXT_DATA__"))

script_content <- fromJSON(script_content)

# tt = list(script_content$props$pageProps$fallback$`/api/translationmapping?locale=en`$TournamentPrefixes)
# tt[[1]]

tt = list(script_content$props$pageProps$fallback$`/api/translationmapping?locale=en`$TournamentPrefixes) %>% unlist()
tt[1]

tt2 = data.frame(tt)
tt2$RowNames <- rownames(tt2)

tt2 = tt2 %>%
  rename("name" = tt, 
         "id" = RowNames)

# intento rvest  -----------


url <- "https://www.fotmob.com/matches/real-madrid-vs-almeria/2tvf3g#4205546"

# Read the HTML content of the webpage
webpage <- read_html(url)

dd = webpage %>% 
  # html_element(xpath = '//*[@id="MatchFactsWrapper"]/div/div[1]/div[3]')
  html_element(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "eufxr1w1", " " ))]')

print(dd)

webpage %>% 
# html_element(xpath = '//*[@id="MatchFactsWrapper"]/div/div[1]/div[3]/div/div[2]')
html_element(class = '.eenhk2w0')


# For example, to extract the titles of the TV series, you can use the following CSS selector:
table <- html_nodes(webpage, ".eenhk2w0") %>% html_text()
table

player <- html_nodes(webpage, ".e2gkh5u2 , .e2gkh5u0 , .e15xx1rt1") %>% html_text()
html_nodes(webpage, ".eenhk2w0 , tbody") %>% html_text()


player <- html_nodes(webpage, "body") %>% html_text() %>% str_replace_all("[//(//)]", "") 
player <- html_nodes(webpage, "body")
player
# httr2 ------------

.mw-parser-output > table:nth-child(95)

url <- "https://es.wikipedia.org/wiki/Anexo:Estad%C3%ADsticas_de_la_Copa_Am%C3%A9rica"

session <- polite::bow(url)

scrape()
copa_top_scorers <- polite::scrape(session) %>% 
  html_nodes(".mw-parser-output table") %>% 
  html_table()

html/body/div[1]/main/main/div/div[1]/div[3]
library(httr2)

req <- httr2::request("https://www.fotmob.com/matches/real-madrid-vs-almeria/2tvf3g#4205546")

httr2::
req
