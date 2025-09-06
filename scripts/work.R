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





# revisar ----------
sum(df_enero_sftp$vali %in% df_enero$vali)

df = readRDS("C:/Users/Mateo/Downloads/Consolidado.rds")

dd = dd %>% select(-base)
data.table::fwrite(dd,paste0("C:/Users/Mateo/Downloads/maestra_transa_sftp.csv"), sep = ";", row.names = F, quote = FALSE)

df = Consolidado %>% select(-id_cel_o_mail)

df %>% head()

dd2 = df %>%
  select(-id_cel_o_mail) %>%
  filter(fuente == "mkc") %>%
  head(1000)


data.table::fwrite(dd2,"C:/Users/Mateo/Downloads/consolidado2.csv")

# Cruzar resultados ---------------------------------
gc(reset = T)

df = data.table::fread("C:/Users/Mateo/Downloads/Total_envios_send20240228.csv")

gc(reset = T)

# df %>% head() %>% View()

df_report2 = df %>%
  mutate(UES = case_when(
    ue == "CREDI" ~ "CREDITO",
    ue == "DROGU" ~ "MEDICAMENTOS",
    ue == "EDUCA" ~ " ECYP",
    ue == "PRODU" ~ " ECYP",
    ue == "SUPER" ~ "SUPERMERCADOS",
    ue == "MERCA" ~ "SUPERMERCADOS",
    ue == "VIVIE" ~ "VIVIENDA",
    ue == "HOTEL" ~ "HYT",
    ue == "RECRE" ~ " RYD",
    TRUE ~ NA
  ))

df_report2 = df_report2 %>%
  # head(100) %>%
  mutate(
    fecha = as.Date(ymd, format = "%d-%m-%Y"),
    anio_mes = format(fecha, "%Y-%m")
         ) %>%
  mutate(valida = paste0(SubscriberKey,UES,anio_mes))

# saveRDS(df_report,"df_reportsfmkc.rds")

df_report2 = df_report2 %>%
  distinct(valida, .keep_all = T) %>%
  filter(!is.na(aperturas_unicas))

# Consumos ------------------------------------------

# Consumos VF --------
df_consumos = readRDS(paste0("C:/Users/Mateo/Downloads/Consumo_Individual_c.rds"))

gc(reset = T)

df_consumos = df_consumos %>%
  filter(year_val == "2023") %>%
  # filter(str_detect(Grupo5,"^CC")) %>%
  mutate(year_val = as.character(year_val)) %>%
  mutate(
    fecha_x = as.Date(fecha, format = "%Y-%m-%d"),
    # year_month = format(fecha_x = "%Y-%m"),
    year = format(fecha_x, "%Y"),
    month = format(fecha_x, "%m"),
    year_month = paste0(year,"-",month)
  )

df_consu2 = df_consumos %>%
  mutate(valida = paste0(Grupo5,UES, year_month)) %>%
  distinct(valida, .keep_all = T)

# rm(df_consumos)

dd3 = left_join(df_report2 %>% filter(!is.na(clicks_unicos)) , df_consu2 %>% select(valida, Cantidad, Valor) , by = c("valida" = "valida"))

table(df_report2$ue)
df_report2 %>%filter(SubscriberKey == "CC1000118913") %>% View()
df_consu2 %>% filter(year_month == "2023-11" & UES != "ECYP") %>% head(10000) %>% View()

gc(reset = T)

dd4 = dd3 %>%
  group_by(SendClassificationType, ue ) %>%
  summarise(
    Total_envios = n(),
    Consumo_total = sum(Valor, na.rm = T),
    cantidad = sum(as.numeric(Cantidad), na.rm = T),
    Aperturas_unicas = sum(aperturas_unicas, na.rm = T),
    Clicks_unicos = sum(clicks_unicos, na.rm = T),
    clickslinks_unicos = sum(clickslinks_unicos, na.rm = T),
    Rebotes = sum(rebotes, na.rm = T),
    desuscritos = sum(desuscritos, na.rm = T)
  ) %>%
  ungroup()

dd4 = dd4 %>%
  mutate(
    openrate = Aperturas_unicas/Total_envios,
    ctr = Clicks_unicos/Total_envios
  )

writexl::write_xlsx(dd4,"C:/Users/Mateo/Downloads/Metrics.xlsx")

df_report2 = df %>%
  group_by(SendClassificationType, ue ) %>%
  summarise(
    Total_envios = n(),
    Consumo_total = sum(Valor, na.rm = T),
    cantidad = sum(as.numeric(Cantidad), na.rm = T),
    Aperturas_unicas = sum(aperturas_unicas, na.rm = T),
    Clicks_unicos = sum(clicks_unicos, na.rm = T),
    clickslinks_unicos = sum(clickslinks_unicos, na.rm = T),
    Rebotes = sum(rebotes, na.rm = T),
    desuscritos = sum(desuscritos, na.rm = T)
  ) %>%
  ungroup()

writexl::write_xlsx(df_report2,"C:/Users/Mateo/Downloads/Metrics.xlsx")

