
import pandas as pd
import numpy as np
import os

# --- Configuración de carpetas y parámetros ---
folder_data_input_r = 'R/data/input'
folder_data_out_r = 'R/data/output/'
folder_data_input_py = 'Python/data/input/'
folder_data_out_py = 'Python/data/output/'
folder_img_out_r = 'R/img/output/'
folder_img_r = 'R/img/'

ligue = 'Colombia. Liga BetPlay'
ligue_short = 'CO_'
country = 'Colombia'
team = 'America de Cali'
minutes = 10

# --- Categoría de métricas ---
categoria_metricas = pd.DataFrame([
    # Solo algunos ejemplos, agregar todos según el R script
    ["jugador", "Jugador", "General", "Player"],
    ["equipo", "Equipo", "General", "Equipment"],
    ["edad", "Edad", "General", "Age"],
    ["goles", "Goles", "Attack", "Goals"],
    ["x_g", "xG", "Attack", "xG"],
    ["asistencias", "Asistencias", "Creativity", "Assists"],
    ["minutos", "Minutos", "General", "Minutes"],
], columns=["variable", "nombre", "tipo", "name"])

# --- Posiciones por categoría ---
posiciones_por_categoria = {
    "Delanteros": ["CF"],
    "Extremos": ["RW", "RWF", "LW", "LWF"],
    "Mediocampo_Ofensivo": ["AMF", "RAMF", "LAMF"],
    "Mediocampo": ["CMF", "RCMF", "LCMF"],
    "Mediocampo_Defensivo": ["DMF", "RDMF", "LDMF"],
    "Defensas": ["CB", "RCB", "RB", "RWB", "LCB", "LB", "LWB"],
    "Arqueros": ["GK"]
}

metricas_por_tipo = {
    "Delanteros": [
        "goles_90", "x_g_90", "x_a_90", "goles_excepto_los_penaltis_90", "asistencias_90",
        "remates_90", "tiros_a_la_porteria_percent", "toques_en_el_area_de_penalti_90"
    ],
    "Extremos": [
        "regates_90", "regates_realizados_percent", "asistencias_90", "x_g_90", "x_a_90"
    ],
    "Mediocampo_Ofensivo": [
        "asistencias_90", "x_g_90", "x_a_90", "jugadas_claves_90"
    ],
    "Mediocampo": [
        "pases_90", "x_g_90", "x_a_90", "precision_pases_percent"
    ],
    "Mediocampo_Defensivo": [
        "duelos_defensivos_90", "x_g_90", "x_a_90"
    ],
    "Defensas": [
        "duelos_defensivos_90", "x_g_90", "x_a_90"
    ],
    "Arqueros": [
        "goles_recibidos_90", "x_g_en_contra_90", "goles_evitados_90"
    ]
}

# --- Leer datos ---
df_teams = pd.read_excel(os.path.join(folder_data_input_r, "Teams.xlsx"), sheet_name=country)
df_teams["logo_team"] = df_teams["Country"].str[:3].str.upper() + "_" + df_teams["Code"]

df_fbref = pd.read_excel(os.path.join(folder_data_input_py, "Betplay/players/players.xlsx"))
df_fbref = df_fbref.drop(df_fbref.columns[[47, 108]], axis=1)
df_fbref = df_fbref[df_fbref["Equipo"].notna()]
df_fbref.columns = [col.split("...")[0] for col in df_fbref.columns]
df_fbref = df_fbref.rename(columns={"Minutos jugados": "Minutos"})
df_fbref = df_fbref.rename(columns=str.lower)
df_fbref["posicion_especifica"] = df_fbref["posicion_especifica"].str.replace(",", "").str.strip().str.upper().str[:3]
df_fbref = df_fbref.merge(df_teams, left_on="equipo", right_on="Understat", how="inner")

# Normalizar posiciones
recode_dict = {
    "LCM": "LCMF", "RCM": "RCMF", "LAM": "LAMF", "RAM": "RAMF",
    "RDM": "RDMF", "LDM": "LDMF", "CF ": "CF", "LW ": "LW",
    "RW ": "RW", "LB ": "LB", "RB ": "RB", "CB ": "CB"
}
df_fbref["posicion_especifica"] = df_fbref["posicion_especifica"].replace(recode_dict)

# Categoría en inglés
categoria_map = {
    "Delanteros": "Forwards",
    "Extremos": "Wingers",
    "Mediocampo_Ofensivo": "Attacking Midfield",
    "Mediocampo": "Midfield",
    "Mediocampo_Defensivo": "Defensive Midfield",
    "Defensas": "Defenders",
    "Arqueros": "Goalkeepers"
}

def get_categoria(pos):
    for cat, pos_list in posiciones_por_categoria.items():
        if pos in pos_list:
            return categoria_map.get(cat, cat)
    return np.nan

df_fbref["categoria"] = df_fbref["posicion_especifica"].apply(get_categoria)

# --- Filtrado por tipo ---
def filtrar_por_tipo(df, posiciones, variables, minutos=minutes):
    columnas_base = ["jugador", "equipo", "logo_team", "equipo_durante_el_periodo_seleccionado",
                     "posicion_especifica", "categoria", "minutos", "edad", "pais_de_nacimiento"]
    columnas_finales = [col for col in columnas_base + variables if col in df.columns]
    return df[(df["posicion_especifica"].isin(posiciones)) & (df["minutos"] >= minutos)][columnas_finales]

lista_dfs_posiciones_filtrados = {}
for rol in posiciones_por_categoria:
    posiciones = posiciones_por_categoria[rol]
    metricas = metricas_por_tipo[rol]
    lista_dfs_posiciones_filtrados[rol] = filtrar_por_tipo(df_fbref, posiciones, metricas)

# --- Procesar para gráfico ---
def procesar_para_grafico(df):
    df = df.replace("", 0)
    df = df.drop_duplicates()
    id_vars = ["jugador", "equipo", "logo_team", "equipo_durante_el_periodo_seleccionado",
               "posicion_especifica", "categoria", "minutos", "edad", "pais_de_nacimiento"]
    melted = df.melt(id_vars=id_vars)
    melted = melted[melted["minutos"] >= minutes]
    melted["Percentil"] = melted.groupby("variable")["value"].rank(pct=True)
    melted["Decil"] = np.minimum(np.floor(melted["Percentil"] * 10).astype(int), 9)
    def rango_percentil(decil):
        rangos = ["0-9%", "10-19%", "20-29%", "30-39%", "40-49%", "50-59%", "60-69%", "70-79%", "80-89%", "90-100%"]
        return rangos[decil] if 0 <= decil < 10 else "0-9%"
    melted["RangoPercentil"] = melted["Decil"].apply(rango_percentil)
    melted["Ranking"] = melted.groupby("variable")["value"].rank(ascending=False, method="min")
    return melted

# --- Exportar resultados ---
df_exportados_global = pd.DataFrame()
for rol in lista_dfs_posiciones_filtrados:
    df_filtrado = lista_dfs_posiciones_filtrados[rol]
    df_procesado = procesar_para_grafico(df_filtrado)
    # Aquí iría la lógica de graficar y exportar imágenes, omitiendo por ser Python puro
    # Se puede guardar los datos exportados como CSV/Excel
    df_exportados_global = pd.concat([df_exportados_global, df_procesado], ignore_index=True)

df_fbref = df_fbref.merge(df_exportados_global, on=["jugador", "equipo", "categoria"], how="left")
df_fbref.to_csv(os.path.join(folder_data_out_r, f"{ligue}_players.csv"), index=False)
df_fbref.to_excel(os.path.join(folder_data_out_r, f"{ligue}_players.xlsx"), index=False)
categoria_metricas.to_excel(os.path.join(folder_data_out_r, f"{ligue}_categorias.xlsx"), index=False)