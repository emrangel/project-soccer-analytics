rm(list = ls(all = T))
gc(reset = T)
library(dplyr)
library(stringr)

# Data  ----------------------
df_sf = data.table::fread("C:/Users/Mateo/Downloads/masiv/METRICS_MRC.csv")

df_enero = data.table::fread("C:/Users/Mateo/Downloads/masiv/Reporte detallado - 01_01_2023 - 31_01_2023.csv") %>%
  # mutate(vali = paste0(`nombre de campaña`,celular)) %>%
  filter(`nombre de campaña` %in% df_seg$CAMPAÑA)

df_febrero = data.table::fread("C:/Users/Mateo/Downloads/masiv/Reporte detallado - 01_02_2023 - 28_02_2023.csv") %>%
  # mutate(vali = paste0(`nombre de campaña`,celular))
  filter(`nombre de campaña` %in% df_seg$CAMPAÑA)

df_marzo = data.table::fread("C:/Users/Mateo/Downloads/masiv/Reporte detallado - 01_03_2023 - 31_03_2023.csv") %>%
  # mutate(vali = paste0(`nombre de campaña`,celular))
  filter(`nombre de campaña` %in% df_seg$CAMPAÑA)

df_comuni = readxl::read_excel("C:/Users/Mateo/Downloads/comunicacion_directa.xlsx")
df_seg = readxl::read_excel("C:/Users/Mateo/Downloads/SEGUIMIENTO MEDIOS DIRECTOS.xlsx", sheet = 2) %>% filter(!is.na(Tipo))
# df_enero_sftp = data.table::fread("C:/Users/Mateo/Downloads/masiv/UrlClicDetalle_Enero2024.csv") %>% mutate(vali = paste0(Campana,Celular))
# dd = data.table::fread("C:/Users/Mateo/Downloads/maestra_transa_sftp.csv")
# df_ley2300 = data.table::fread("C:/Users/Mateo/Downloads/LEY2300.csv")

# Process --------------------
### validacion ------
df_enero %>%
  filter(str_detect(`nombre de campaña`, "AFI")) %>%
  head() %>%
  View()

df_enero %>%
  filter(`nombre de campaña`=="40831AFILI&INFORMATIVA&SMS") %>%
  group_by(`nombre de campaña`) %>% tally()

df_enero_sftp %>% head() %>% View()

### Ejercico -----

gc(reset = T)

df_sf2 = df_sf %>%
  select(-sentdate) %>%
  distin
