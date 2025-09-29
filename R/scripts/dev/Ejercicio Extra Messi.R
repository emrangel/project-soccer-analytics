# Ejercicio 9.16 Messi The Atheletic

library(readxl)
library(ggtext)
library(ggplot2)

setwd("C:/Users/Mateo/Desktop/Rstudio/ObjetivoA/Curso/Modulo 2")

messi <- read_excel("messi.xlsx")

titulo=("<b>Evoluci?n de <span style = 'color:blue;'>**xT(+)**</span> y <span style = 'color:red3;'>**VAEP Ofensivo**</span>  de Leo Messi")
subtitulo="Media M?vil 5 partidos en La Liga desde la temporada 2017/2018 a J18 2020/2021"



ggplot() +
  geom_line(data=messi,aes(x=Partido,y=xT_Total),color='blue',size=1) +
  geom_smooth(data=messi,aes(x=Partido,y=xT_Total),color='blue',size=0.5,se = FALSE,alpha=0.5,linetype='dashed') +
  geom_line(data=messi,aes(x=Partido,y=VAEP_Total),color='red3',size=1) +
  geom_smooth(data=messi,aes(x=Partido,y=VAEP_Total),color='red3',size=0.5,se = FALSE,alpha=0.5,linetype='dashed') +
  geom_point(data=messi,aes(x=Partido,y=xT_Total),color='black',fill="blue",size=2,shape=21,stroke=1.2) +
  geom_point(data=messi,aes(x=Partido,y=VAEP_Total),color='black',size=2,fill='red3',shape=21,stroke=1.2) +
  ylim(0,2)+
  labs(x='Partidos',y="",
       title=titulo,
       subtitle=subtitulo,
       caption = "Elaborado por Jes?s Lagos @Vdot_Spain a partir de xT (Singh, 2019) Singh y VAEP (Decroos et al., 2019) Thanks to @p_robberechts") +
  annotate(geom="label", x=20, y=2, label="2017/2018",fill="black",color="gray45",size=3) +
  annotate(geom="label", x=50, y=2, label="2018/2019",fill="black",color="gray45",size=3) +
  annotate(geom="label", x=80, y=2, label="2019/2020",fill="black",color="gray45",size=3) +
  annotate(geom="label", x=110, y=2, label="2020/2021",fill="black",color="gray45",size=3) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=60, hjust=1,color = "gray45"),
        plot.caption = element_text(hjust = 0),
        axis.text.y=element_text(hjust=1,color = "gray45"),
        legend.background = element_rect(colour = NA),
        plot.subtitle = element_text(size = 14),
        plot.title = element_markdown(size = 20),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black",
                                        colour = "black",
                                        size = 0.5),
        text = element_text( face = "plain",
                            colour = "white",
                            lineheight = 0.9,  hjust = 0.5,
                            vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE),
        panel.grid.major.y = element_line(colour = "gray45",linetype="dashed") ,
        panel.grid.minor = element_line(colour = "black"),
        axis.line.x=element_line(color="black"),
        axis.line.y=element_line(color="black"),
        panel.grid.major.x = element_line(colour = "gray45",linetype="dashed")
  )
