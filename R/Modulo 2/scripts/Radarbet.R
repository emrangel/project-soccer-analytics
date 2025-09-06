library(dplyr)
library(readxl)
library(xlsx)
library(writexl)


#read xlsx

STATSbet <- read_excel("Stats.xlsx", sheet = "Hoja1 (2)")

radar<- STATSbet %>%
  group_by(Jugador) %>%
  summarise_all("sum")


write_xlsx(radar,"radar3.xlsx")
##write.csv(radar,file="radar1.csv")
