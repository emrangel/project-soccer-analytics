
import pandas as pd
import numpy as np
from plotnine import *
import os
import warnings

warnings.filterwarnings('ignore')

# Define paths
_folder_data_input_r = 'R/data/input/'
_folder_data_out_r = 'R/data/output/'
_folder_data_input_py = 'Python/data/input/'
_folder_data_out_py = 'Python/data/output/'
_folder_img_out_r = 'R/img/output/'
_folder_img_r = 'R/img/'

_ligue = 'Colombia. Liga BetPlay'
_ligue_short = 'CO_'
_country = 'Colombia'
_team = 'America de Cali'
_minutes = 10

# Create categoria_metricas DataFrame
categoria_metricas = pd.DataFrame({
    'variable': ["jugador", "equipo", "equipo_durante_el_periodo_seleccionado", "posicion_especifica", "edad", "valor_de_mercado_transfermarkt", "vencimiento_contrato", "partidos_jugados", "minutos", "goles", "x_g", "asistencias", "x_a", "duelos_90", "duelos_ganados_percent", "pais_de_nacimiento", "pasaporte", "pie", "altura", "peso", "en_prestamo", "acciones_defensivas_realizadas_90", "duelos_defensivos_90", "duelos_defensivos_ganados_percent", "duelos_aereos_en_los_90", "duelos_aereos_ganados_percent", "entradas_90", "posesion_conquistada_despues_de_una_entrada", "tiros_interceptados_90", "interceptaciones_90", "posesion_conquistada_despues_de_una_interceptacion", "faltas_90", "tarjetas_amarillas", "tarjetas_amarillas_90", "tarjetas_rojas", "tarjetas_rojas_90", "acciones_de_ataque_exitosas_90", "goles_90", "goles_excepto_los_penaltis", "goles_excepto_los_penaltis_90", "x_g_90", "goles_de_cabeza", "goles_de_cabeza_90", "remates", "remates_90", "tiros_a_la_porteria_percent", "goles_hechos_percent", "asistencias_90", "centros_90", "precision_centros_percent", "centros_desde_la_banda_izquierda_90", "precision_centros_desde_la_banda_izquierda_percent", "centros_desde_la_banda_derecha_90", "precision_centros_desde_la_banda_derecha_percent", "centros_al_area_pequena_90", "regates_90", "regates_realizados_percent", "duelos_atacantes_90", "duelos_atacantes_ganados_percent", "toques_en_el_area_de_penalti_90", "carreras_en_progresion_90", "aceleraciones_90", "pases_recibidos_90", "pases_largos_recibidos_90", "faltas_recibidas_90", "pases_90", "precision_pases_percent", "pases_hacia_adelante_90", "precision_pases_hacia_adelante_percent", "pases_hacia_atras_90", "precision_pases_hacia_atras_percent", "pases_laterales_90", "precision_pases_laterales_percent", "pases_cortos_medios_90", "precision_pases_cortos_medios_percent", "pases_largos_90", "precision_pases_largos_percent", "longitud_media_pases_m", "longitud_media_pases_largos_m", "x_a_90", "second_assists_90", "third_assists_90", "desmarques_90", "precision_desmarques_percent", "jugadas_claves_90", "pases_en_el_ultimo_tercio_90", "precision_pases_en_el_ultimo_tercio_percent", "pases_al_area_de_penalti_90", "pases_hacia_el_area_pequena_percent", "pases_en_profundidad_90", "precision_pases_en_profundidad_percent", "ataque_en_profundidad_90", "centros_desde_el_ultimo_tercio_90", "pases_progresivos_90", "precision_pases_progresivos_percent", "goles_recibidos", "goles_recibidos_90", "remates_en_contra", "remates_en_contra_90", "porterias_imbatidas_en_los_90", "paradas_percent", "x_g_en_contra", "x_g_en_contra_90", "goles_evitados", "goles_evitados_90", "pases_hacia_atras_recibidos_del_arquero_90", "salidas_90", "tiros_libres_90", "tiros_libres_directos_90", "tiros_libres_directos_percent", "corneres_90", "penaltis_a_favor", "penaltis_realizados_percent"],
    'nombre': ["Jugador", "Equipo", "Equipo Durante El Periodo Seleccionado", "Posición Específica", "Edad", "Valor de mercado", "Vencimiento contrato", "Partidos jugados", "Minutos", "Goles", "xG", "Asistencias", "xA", "Duelos / 90", "Duelos ganados (%)", "País de nacimiento", "Pasaporte", "Pie dominante", "Altura", "Peso", "En préstamo", "Acciones defensivas / 90", "Duelos defensivos / 90", "Duelos defensivos ganados (%)", "Duelos aéreos / 90", "Duelos aéreos ganados (%)", "Entradas / 90", "Posesión tras entrada", "Tiros interceptados / 90", "Intercepciones / 90", "Posesión tras intercepción", "Faltas / 90", "Tarjetas amarillas", "Tarjetas amarillas / 90", "Tarjetas rojas", "Tarjetas rojas / 90", "Acciones ofensivas exitosas / 90", "Goles / 90", "Goles (sin penaltis)", "Goles (sin penaltis) / 90", "xG / 90", "Goles de cabeza", "Goles de cabeza / 90", "Remates", "Remates / 90", "Precisión de remates (%)", "Tasa de conversión (%)", "Asistencias / 90", "Centros / 90", "Precisión centros (%)", "Centros izquierda / 90", "Precisión izquierda (%)", "Centros derecha / 90", "Precisión derecha (%)", "Centros al área pequeña / 90", "Regates / 90", "Éxito en regates (%)", "Duelos ofensivos / 90", "Duelos ofensivos ganados (%)", "Toques en área / 90", "Carreras progresivas / 90", "Aceleraciones / 90", "Pases recibidos / 90", "Pases largos recibidos / 90", "Faltas recibidas / 90", "Pases / 90", "Precisión total pases (%)", "Pases hacia adelante / 90", "Precisión adelante (%)", "Pases hacia atrás / 90", "Precisión atrás (%)", "Pases laterales / 90", "Precisión lateral (%)", "Pases cortos/medios / 90", "Precisión cortos/medios (%)", "Pases largos / 90", "Precisión pases largos (%)", "Longitud media de pases (m)", "Longitud media de pases largos (m)", "xA / 90", "Segunda asistencia / 90", "Tercera asistencia / 90", "Desmarques / 90", "Precisión desmarques (%)", "Pases clave / 90", "Pases en tercio final / 90", "Precisión tercio final (%)", "Pases al área / 90", "Precisión área pequeña (%)", "Pases en profundidad / 90", "Precisión profundidad (%)", "Ataques en profundidad / 90", "Centros desde último tercio / 90", "Pases progresivos / 90", "Precisión progresivos (%)", "Goles recibidos", "Goles recibidos / 90", "Remates en contra", "Remates en contra / 90", "Porterías imbatidas / 90", "Porcentaje de paradas", "xG en contra", "xG en contra / 90", "Goles evitados", "Goles evitados / 90", "Pases del arquero / 90", "Salidas / 90", "Tiros libres / 90", "Tiros libres directos / 90", "Precisión tiros libres (%)", "Corneres / 90", "Penaltis a favor", "Penaltis convertidos (%)"],
    'tipo': ["General", "General", "General", "General", "General", "General", "General", "General", "General", "Attack", "Attack", "Creativity", "Creativity", "Physical", "Physical", "General", "General", "General", "General", "General", "General", "Defense", "Defense", "Defense", "Defense", "Defense", "Defense", "Defense", "Defense", "Defense", "Defense", "Discipline", "Discipline", "Discipline", "Discipline", "Discipline", "Attack", "Attack", "Attack", "Attack", "Attack", "Attack", "Attack", "Attack", "Attack", "Attack", "Attack", "Creativity", "Attack", "Attack", "Attack", "Attack", "Attack", "Attack", "Attack", "Physical", "Physical", "Attack", "Attack", "Attack", "Attack", "Attack", "Physical", "Physical", "Discipline", "Passing", "Passing", "Passing", "Passing", "Passing", "Passing", "Passing", "Passing", "Passing", "Passing", "Passing", "Passing", "Passing", "Passing", "Creativity", "Creativity", "Creativity", "Attack", "Attack", "Creativity", "Creativity", "Creativity", "Creativity", "Creativity", "Creativity", "Creativity", "Attack", "Attack", "Passing", "Passing", "Goalkeeping", "Goalkeeping", "Goalkeeping", "Goalkeeping", "Goalkeeping", "Goalkeeping", "Goalkeeping", "Goalkeeping", "Goalkeeping", "Goalkeeping", "Goalkeeping", "Goalkeeping", "Attack", "Attack", "Attack", "Attack", "Attack", "Attack"],
    'name': ["Player", "Equipment", "Team During The Selected Period", "Specific Position", "Age", "Market value", "Contract expiration", "Matches played", "Minutes", "Goals", "xG", "Assists", "xA", "Duels / 90", "Duels won (%)", "Country of birth", "Passport", "Dominant foot", "Height", "Weight", "On loan", "Defensive actions / 90", "Defensive duels / 90", "Defensive duels won (%)", "Aerial Duels / 90", "Aerial duels won (%)", "Tackles / 90", "Possession after entry", "Intercepted shots / 90", "Interceptions / 90", "Possession after interception", "Fouls / 90", "Yellow cards", "Yellow cards / 90", "Red cards", "Red cards / 90", "Successful offensive actions / 90", "Goals / 90", "Goals (without penalties)", "Goals (without penalties) / 90", "xG / 90", "Headed goals", "Headed goals / 90", "Shots", "Shots / 90", "Shot accuracy (%)", "Conversion rate (%)", "Assists / 90", "Centers / 90", "Accuracy centers (%)", "Left centers / 90", "Left accuracy (%)", "Centers right / 90", "Right accuracy (%)", "Crosses into the small area / 90", "Dribbling / 90", "Dribbling success (%)", "Offensive duels / 90", "Offensive duels won (%)", "Touches in the area / 90", "Progressive Races / 90", "Accelerations / 90", "Passes received / 90", "Long passes received / 90", "Fouls received / 90", "Passes / 90", "Total pass accuracy (%)", "Forward passes / 90", "Forward Accuracy (%)", "Back passes / 90", "Backward Accuracy (%)", "Lateral passes / 90", "Lateral accuracy (%)", "Short/medium passes / 90", "Short/medium accuracy (%)", "Long passes / 90", "Long pass accuracy (%)", "Average pass length (m)", "Average length of long passes (m)", "xA / 90", "Second assist / 90", "Third assist / 90", "Unmarks / 90", "Accuracy of runs (%)", "Key passes / 90", "Passes in the final third / 90", "Final third accuracy (%)", "Passes to the area / 90", "Small area accuracy (%)", "Deep passes / 90", "Depth accuracy (%)", "Deep Attacks / 90", "Centers from the final third / 90", "Progressive passes / 90", "Progressive accuracy (%)", "Goals received", "Goals received / 90", "Counterattacks", "Shots against / 90", "Clean sheets / 90", "Percentage of stops", "xG against", "xG against / 90", "Goals avoided", "Goals avoided / 90", "Goalkeeper passes / 90", "Exits / 90", "Free throws / 90", "Direct free kicks / 90", "Free throw accuracy (%)", "Corners / 90", "Penalties in favor", "Penalties converted (%)"]
})

# Positions by category
posiciones_por_categoria = {
    'Delanteros': ["CF"],
    'Extremos': ["RW", "RWF", "LW", "LWF"],
    'Mediocampo_Ofensivo': ["AMF", "RAMF", "LAMF"],
    'Mediocampo': ["CMF", "RCMF", "LCMF"],
    'Mediocampo_Defensivo': ["DMF", "RDMF", "LDMF"],
    'Defensas': ["CB", "RCB", "RB", "RWB", "LCB", "LB", "LWB"],
    'Arqueros': ["GK"]
}

# Metrics by type
metricas_por_tipo = {
    'Delanteros': [
        "goles_90", "x_g_90", "x_a_90","goles_excepto_los_penaltis_90", "asistencias_90",
        "remates_90", "tiros_a_la_porteria_percent", "toques_en_el_area_de_penalti_90",
        "duelos_atacantes_90", "duelos_atacantes_ganados_percent", "desmarques_90",
        "precision_desmarques_percent", "carreras_en_progresion_90", "pases_al_area_de_penalti_90"
    ],
    'Extremos': [
        "regates_90", "regates_realizados_percent", "asistencias_90", "x_g_90", "x_a_90",
        "centros_90", "precision_centros_percent", "pases_en_el_ultimo_tercio_90",
        "precision_pases_en_el_ultimo_tercio_percent", "duelos_atacantes_90",
        "duelos_atacantes_ganados_percent", "toques_en_el_area_de_penalti_90",
        "carreras_en_progresion_90", "desmarques_90", "pases_al_area_de_penalti_90"
    ],
    'Mediocampo_Ofensivo': [
        "asistencias_90","x_g_90", "x_a_90", "jugadas_claves_90", "pases_en_el_ultimo_tercio_90",
        "precision_pases_en_el_ultimo_tercio_percent", "remates_90",
        "goles_90", "pases_progresivos_90", "pases_al_area_de_penalti_90",
        "carreras_en_progresion_90", "toques_en_el_area_de_penalti_90",
        "second_assists_90", "pases_en_profundidad_90"
    ],
    'Mediocampo': [
        "pases_90","x_g_90", "x_a_90", "precision_pases_percent", "pases_hacia_adelante_90",
        "precision_pases_hacia_adelante_percent", "pases_largos_90",
        "precision_pases_largos_percent", "pases_progresivos_90",
        "jugadas_claves_90", "duelos_90", "duelos_ganados_percent",
        "interceptaciones_90", "entradas_90", "posesion_conquistada_despues_de_una_interceptacion",
        "faltas_90"
    ],
    'Mediocampo_Defensivo': [
        "duelos_defensivos_90","x_g_90", "x_a_90", "duelos_defensivos_ganados_percent", "interceptaciones_90",
        "entradas_90", "posesion_conquistada_despues_de_una_entrada", "pases_90",
        "precision_pases_percent", "pases_hacia_atras_90", "precision_pases_hacia_atras_percent",
        "faltas_90", "tarjetas_amarillas_90", "posesion_conquistada_despues_de_una_interceptacion",
        "pases_laterales_90", "precision_pases_laterales_percent"
    ],
    'Defensas': [
        "duelos_defensivos_90","x_g_90", "x_a_90", "duelos_defensivos_ganados_percent", "interceptaciones_90",
        "entradas_90", "duelos_aereos_en_los_90", "duelos_aereos_ganados_percent",
        "posesion_conquistada_despues_de_una_entrada", "tiros_interceptados_90",
        "pases_90", "precision_pases_percent", "pases_largos_90",
        "precision_pases_largos_percent", "faltas_90", "tarjetas_amarillas_90"
    ],
    'Arqueros': [
        "goles_recibidos_90", "x_g_en_contra_90", "goles_evitados_90",
        "paradas_percent", "porterias_imbatidas_en_los_90",
        "remates_en_contra_90", "salidas_90",
        "pases_largos_90", "precision_pases_largos_percent"
    ]
}

# Convert list to DataFrame
posiciones_df = pd.DataFrame(
    [(categoria, posicion) for categoria, posiciones in posiciones_por_categoria.items() for posicion in posiciones],
    columns=['categoria', 'posicion_especifica']
)

def filtrar_por_tipo(df, posiciones, variables, minutos=_minutes):
    columnas_base = ["jugador", "equipo", "logo_team", "equipo_durante_el_periodo_seleccionado",
                     "posicion_especifica", "categoria", "minutos", "edad", "pais_de_nacimiento"]
    columnas_finales = list(set(columnas_base + variables).intersection(df.columns))
    
    patron = '|'.join(r'\b{}\b'.format(p) for p in posiciones)
    
    return df[(df['posicion_especifica'].str.contains(patron, na=False)) & (df['minutos'] >= minutos)][columnas_finales]

# Load data
df_teams = pd.read_excel(os.path.join(_folder_data_input_r, "Teams.xlsx"), sheet_name=_country)
df_teams['logo_team'] = df_teams['Country'].str.slice(0, 3).str.upper() + '_' + df_teams['Code']

df_fbref = pd.read_excel(os.path.join(_folder_data_input_py, "Betplay/players/players.xlsx"))
df_fbref = df_fbref.drop(columns=[df_fbref.columns[47], df_fbref.columns[108]])
df_fbref = df_fbref.dropna(subset=['Equipo'])
df_fbref.columns = df_fbref.columns.str.replace(r'\.\.\.[0-9]+$', '', regex=True)
df_fbref = df_fbref.rename(columns={'Minutos jugados': 'Minutos'})
df_fbref.columns = (df_fbref.columns.str.lower().str.replace(' ', '_', regex=False).str.replace('(', '', regex=False).str.replace(')', '', regex=False))
df_fbref['posicion_especifica'] = df_fbref['posicion_especifica'].str.replace(',', '', regex=False).str.strip().str.upper().str.slice(0, 3)

df_fbref = pd.merge(df_fbref, df_teams, left_on='equipo', right_on='Understat')

# Normalize positions
pos_replacements = {
    "LCM": "LCMF", "RCM": "RCMF", "LAM": "LAMF", "RAM": "RAMF",
    "RDM": "RDMF", "LDM": "LDMF", "CF ": "CF", "LW ": "LW",
    "RW ": "RW", "LB ": "LB", "RB ": "RB", "CB ": "CB"
}
df_fbref['posicion_especifica'] = df_fbref['posicion_especifica'].str.strip().replace(pos_replacements)
df_fbref = pd.merge(df_fbref, posiciones_df, on="posicion_especifica", how="left")

cat_replacements = {
    "Delanteros": "Forwards", "Extremos": "Wingers", "Mediocampo_Ofensivo": "Attacking Midfield",
    "Mediocampo": "Midfield", "Mediocampo_Defensivo": "Defensive Midfield",
    "Defensas": "Defenders", "Arqueros": "Goalkeepers"
}
df_fbref['categoria'] = df_fbref['categoria'].replace(cat_replacements)

lista_dfs_posiciones_filtrados = {}
for rol, posiciones in posiciones_por_categoria.items():
    metricas = metricas_por_tipo[rol]
    lista_dfs_posiciones_filtrados[rol] = filtrar_por_tipo(df_fbref, posiciones, metricas)

# Color palette
coloresrango = {
    "0-9%": "red", "10-19%": "orange", "20-29%": "gold", "30-39%": "yellow",
    "40-49%": "khaki", "50-59%": "yellowgreen", "60-69%": "olivedrab",
    "70-79%": "darkolivegreen", "80-89%": "seagreen", "90-100%": "green"
}

def procesar_para_grafico(df):
    df = df.replace("", 0).drop_duplicates()
    
    melted = pd.melt(df, id_vars=["jugador", "equipo", "logo_team", "equipo_durante_el_periodo_seleccionado",
                                  "posicion_especifica", "categoria", "minutos", "edad", "pais_de_nacimiento"])

    melted = melted[melted['minutos'] >= _minutes]

    melted['Percentil'] = melted.groupby('variable')['value'].rank(pct=True)
    melted['Decil'] = (melted['Percentil'] * 10).apply(np.floor).clip(upper=9)
    
    bins = [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
    labels = ["0-9%", "10-19%", "20-29%", "30-39%", "40-49%", "50-59%", "60-69%", "70-79%", "80-89%", "90-100%"]
    melted['RangoPercentil'] = pd.cut(melted['Percentil'], bins=bins, labels=labels, right=False, include_lowest=True)
    
    melted['Ranking'] = melted.groupby('variable')['value'].rank(method='min', ascending=False)
    
    return melted

def graficar_jugador(data_jugadores, categoria_metricas, rol="", equipos=[]):
    df_exportados = pd.DataFrame(columns=["jugador", "equipo", "categoria", "archivo_png"])
    
    jugadores = data_jugadores[data_jugadores['equipo'].isin(equipos)]['jugador'].unique()
    
    for jugador_actual in jugadores:
        Pintar_Jugador = data_jugadores[(data_jugadores['jugador'] == jugador_actual) & (data_jugadores['equipo'].isin(equipos))].copy()
        Pintar_Jugador['value'] = Pintar_Jugador['value'].round(2)
        Pintar_Jugador['pais_de_nacimiento'] = Pintar_Jugador['pais_de_nacimiento'].str.upper()
        
        Pintar_Jugador = pd.merge(Pintar_Jugador, categoria_metricas[['variable', 'name', 'tipo']], on="variable")
        Pintar_Jugador = Pintar_Jugador.drop_duplicates()
        
        Pintar_Jugador['nombre_variable'] = pd.Categorical(Pintar_Jugador['name'], categories=Pintar_Jugador.sort_values('Ranking', ascending=False)['name'].unique(), ordered=True)
        
        if Pintar_Jugador.empty:
            continue
            
        titulo = f"**{jugador_actual}** ({Pintar_Jugador['edad'].iloc[0]}) - {Pintar_Jugador['categoria'].iloc[0]}"
        subtitulo = f"{Pintar_Jugador['equipo'].iloc[0]} - {_ligue} ({Pintar_Jugador['minutos'].iloc[0]}) - min\nTotal players: {data_jugadores['jugador'].nunique()}"

        p = (ggplot(Pintar_Jugador, aes(x='Decil * 10', y='nombre_variable', group='tipo', color='RangoPercentil')) +
             geom_segment(aes(yend='nombre_variable'), xend=0, size=2) +
             geom_point(size=3.5) +
             scale_color_manual(values=coloresrango, drop=False) +
             geom_text(aes(label='value'), nudge_x=0.3, size=8, color="black", va="center", ha="left") +
             scale_x_continuous(expand=(0, 0), breaks=range(0, 91, 10), labels=[f"q{i}" for i in range(0, 91, 10)]) +
             annotate("rect", xmin=100, xmax=109, ymin=-np.inf, ymax=np.inf, fill="white", alpha=0.5, color="white") +
             geom_text(aes(label='f"#{Ranking} (q{Decil * 10})"', y='nombre_variable', x=104), size=9, color="black") +
             labs(title=titulo, subtitle=subtitulo, x="", y="", caption=f"Source Wyscout - Minutes ( +{_minutes} ) by: Erick Rangel") +
             theme_bw() +
             theme(
                 panel_grid_major_y=element_blank(), panel_grid_major_x=element_blank(),
                 panel_grid_minor_x=element_blank(),
                 plot_title=element_markdown(size=16, ha='center', colour="black"),
                 plot_subtitle=element_text(size=10, ha='center', colour="black"),
                 panel_border=element_blank(), axis_line_y=element_blank(),
                 axis_line_x=element_line(), axis_text_y=element_text(size=12),
                 legend_title=element_blank(), legend_direction="horizontal",
                 legend_position="bottom", plot_margin={'b': 2, 'l': 2, 't': 2, 'r': 2},
                 plot_caption=element_text(ha='right', size=8)
             ))
        
        nombre_archivo = jugador_actual.replace('.', '').upper().replace(' ', '_')
        nombre_archivo2 = f"{Pintar_Jugador['categoria'].iloc[0].upper().replace(' ', '_')}_{Pintar_Jugador['equipo'].iloc[0].upper().replace(' ', '_')}_{nombre_archivo}.png"

        ruta_salida = os.path.join(_folder_img_out_r, rol, nombre_archivo2)
        os.makedirs(os.path.dirname(ruta_salida), exist_ok=True)
        
        p.save(ruta_salida, width=34, height=15, units="cm", dpi=300)
        
        df_exportados = df_exportados.append({"jugador": jugador_actual, "equipo": Pintar_Jugador['equipo'].iloc[0], "categoria": Pintar_Jugador['categoria'].iloc[0], "archivo_png": nombre_archivo2}, ignore_index=True)
        
    return df_exportados

df_exportados_global = pd.DataFrame()

for rol in lista_dfs_posiciones_filtrados.keys():
    print(f"Procesando: {rol}")
    df_filtrado = lista_dfs_posiciones_filtrados[rol]
    df_procesado = procesar_para_grafico(df_filtrado)
    df_exportados = graficar_jugador(df_procesado, categoria_metricas, rol=rol, equipos=["América de Cali"])
    df_exportados_global = pd.concat([df_exportados_global, df_exportados], ignore_index=True)

df_fbref = pd.merge(df_fbref, df_exportados_global, on=["jugador", "equipo", "categoria"], how="left")

df_fbref.to_csv(os.path.join(_folder_data_out_r, f"{_ligue}_players.csv"), index=False)
df_fbref.to_excel(os.path.join(_folder_data_out_r, f"{_ligue}_players.xlsx"), index=False)
categoria_metricas.to_excel(os.path.join(_folder_data_out_r, f"{_ligue}_categorias.xlsx"), index=False)

print("Proceso completado.")
