library(shiny)
library('dplyr')
library(ggplot2)
library(shinyWidgets)
library(png)
library(grid)

# Nuestro Directorio de Trabajo. el mio este este:
# setwd("C:/My Program Files/Curso/Curso_R/Modulo 10/Selector_Tiros")

# Cargamos los tiros. Os los he dejado en el directorio
tiros_final <- readRDS("tiros_2122_b.rds")

# Por cierto Fotmob en su ánimo de tocarnos las narices 
# ha quitado o añadido acentos a los jugadores. Así que 
# pasando este código cambiamos todos los caracteres a sin acentos.

tiros_final$playerName  <- chartr('áéíóúñàèìòùäëïöü','aeiounaeiouaeiou',tiros_final$playerName)


# Vamos a hacer unas transformaciones que hacen falta para pintar luego
# Fijaos que dentro de tiros_final hay una columna llamada inGoalShot que es un dataframe de 3
# variables "x", "y" y "zoom" 
# Vamos a pasarlo a dos campos fijaos en la manera de apuntar con 2 $
tiros_final$xporteria<-tiros_final$onGoalShot$x
tiros_final$yporteria<-tiros_final$onGoalShot$y
tiros_final$xfin<-tiros_final$x*100
tiros_final$yfin<-tiros_final$y*100

# Quitamos el campo onGoalShot para que no de problemas luego. sus datos ya
# los pasamos a otros campos antes
tiros_final <- tiros_final %>%
  select(-19)

# Quitamos goles en propia puerta y tiros bloqueados porque nos interesa saber adonde tira
# y Cuando el vamor de expectedGoalsOnTarget sea nulo lo ponemos a 0
tiros_final <- tiros_final %>%
  filter(isBlocked==FALSE,
         isOwnGoal==FALSE) %>%
  mutate(expectedGoalsOnTarget=case_when(
           is.na(expectedGoalsOnTarget) ~ 0,
           TRUE ~expectedGoalsOnTarget
         ))



#Creamos el campo de fútbol. 
theme_blankPitch = function(size=12) {
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.length=unit(0, "lines"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background=element_rect(fill="#FFFFFF", colour=NA),
    legend.key=element_rect(colour="#FFFFFF",fill="#FFFFFF"),
    legend.key.size=unit(1.2, "lines"),
    legend.text=element_text(size=size),
    legend.title=element_text(size=size, face="bold",hjust=0),
    strip.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF", size = .5),
    panel.background=element_rect(fill="#FFFFFF",colour="#FFFFFF"),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.spacing=element_blank(),
    plot.background=element_blank(),
    plot.margin=unit(c(0, 0, 0, 0), "lines"),
    plot.title=element_text(size=size*1.2),
    strip.text.y=element_text(colour="#FFFFFF",size=size,angle=270),
    strip.text.x=element_text(size=size*1))}

ymin <- 0
xmin <- 0

GoalWidth <- 732
penspot <- 1100
boxedgeW <- 4032
boxedgeL <- 1650
box6yardW <- 1832
box6yardL <- 550


TheBoxWidth <- c(((7040 / 2) + (boxedgeW / 2)),((7040 / 2) - (boxedgeW / 2)))
TheBoxHeight <- c(boxedgeL,10600-boxedgeL)
GoalPosts <- c(((7040 / 2) + (GoalWidth / 2)),((7040 / 2) - (GoalWidth / 2)))

box6yardWidth <- c(((7040 / 2) + (box6yardW / 2)),((7040 / 2) - (box6yardW / 2)))
box6yardHeight <- c(box6yardL,10600-box6yardL)

centreCirle_d <- 1830

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

Dleft <- circleFun(c((penspot),(7040/2)),centreCirle_d,npoints = 1000)
Dleft <- Dleft[which(Dleft$x >= (boxedgeL)),]
Dright <- circleFun(c((10600-(penspot)),(7040/2)),centreCirle_d,npoints = 1000)
Dright <- Dright[which(Dright$x <= (10600-(boxedgeL))),]
center_circle <- circleFun(c((10600/2),(7040/2)),centreCirle_d,npoints = 100)
TopLeftCorner <- circleFun(c(xmin,7040),200,npoints = 1000)
TopRightCorner <- circleFun(c(10600,7040),200,npoints = 1000)
BottomLeftCorner <- circleFun(c(xmin,ymin),200,npoints = 1000)
BottomRightCorner <- circleFun(c(10600,ymin),200,npoints = 1000)


# Vamos a crear los selectores en la Parte del UI
ui <- fluidPage(
  # El titulo
  titlePanel("Shot Zones"),
  sidebarLayout(
    # Los selectores en el panel de la izquierda
    sidebarPanel(
      selectInput(inputId = "countryinput", label = "Country",choices = ""),
      selectInput(inputId = "teaminput", label = "Team",choices = ""),
      selectInput(inputId = "playerInput", label = "Player",choices = ""),
      selectInput(inputId = "eventInput", label = "Event type",
                  choices = c('AttemptSaved','Miss','Goal','Post'),
                  selected=c('AttemptSaved','Miss','Goal','Post'),
                  multiple=TRUE),
      sliderInput("minuteInput", "Range of minutes", 0, 120, c(0, 120)),
      sliderInput("xGInput", "xG range", 0, 1, c(0, 1)),
      sliderInput("xGOTInput", "xGOT range", 0, 1, c(0, 1)),
      selectInput("typeInput", "Shot type",
                  choices = c("RightFoot", "LeftFoot", "Header","OtherBodyParts"),
                  selected = c("RightFoot", "LeftFoot", "Header","OtherBodyParts"),
                  multiple = TRUE),
      selectInput("penalInput", "Situation",
                  choices = c('RegularPlay','FromCorner','FastBreak','Penalty','FreeKick','SetPiece','ThrowInSetPiece','IndividualPlay'),
                  selected = c('RegularPlay', 'FromCorner','FastBreak','Penalty','FreeKick','SetPiece','ThrowInSetPiece','IndividualPlay'),
                  multiple = TRUE)
      
      
    ),
    # Aquí el panel ya de resultados en el panel central
    mainPanel(
      plotOutput(
        # fijaos que ya indicamos que será un elemento "brush"
        outputId = "plot",
        brush = brushOpts(
          id = "plotBrush",
          delay = 5000
        ),height = "auto"
      ),
      # dos botones que añadimos para limpiar y resetear el gráfico anterior
      actionButton("clearBrush", "Clear brush"),
      actionButton("resetPlot", "Reset plot"),
      # el gráfico final de localización de tiros
      plotOutput(
        outputId = "plot2"
      )
    )
    
    
  ))


server <- function(input, output, session) {
  
  # Aquí toda la jerarquia de selectores.
  # fijaos en la inclusión de try catch que nos permite 
  # si elemento nos devuelve un error que nos devuelva algo. Seria como un If pero de errores.
  # Fijaos también en la inclusión de los req (para que solo ejecute cuando haya cargado los
  # selectores previos)
  
  equipos_filtro_fotmob <- reactive({
    tryCatch({
    Teams <- tiros_final %>%
      distinct(country) %>%
      arrange(country)},
    error = function(x){
      return('')
    })
    
  })
  
  observe({
    primero <- equipos_filtro_fotmob()
    primero <- primero$country[1]
    updateSelectizeInput(session = session, inputId = "countryinput",
                         choices = equipos_filtro_fotmob()$country,selected = primero
                         
                         
    )})
  
  team_filtro_fotmob <- reactive({
    tryCatch({
    jugadores <- tiros_final %>%
      filter(country == input$countryinput) %>%
      distinct(team_name) %>%
      arrange(team_name)},
    error = function(x){
      return('')
    })
  })
  
  observe({
    primero <- team_filtro_fotmob()
    primero <- primero$team_name[1]
    updateSelectizeInput(session = session, inputId = "teaminput",
                         choices = team_filtro_fotmob()$team_name,selected = primero
                         
    )})
  
  player_filtro_fotmob <- reactive({
    tryCatch({
    jugadores <- tiros_final %>%
      filter(country %in% input$countryinput,
             team_name %in% input$teaminput) %>%
      distinct(playerName) %>%
      arrange(playerName)},
    error = function(x){
      return('')
    })
  }) 

  observe({
    primero <- player_filtro_fotmob()
    primero <- primero$playerName[1]
    updateSelectizeInput(session = session, inputId = "playerInput",
                         choices = player_filtro_fotmob()$playerName,selected = primero
                         
                         
    )})
  
  eventInput_filtro_fotmob <- reactive({
    tryCatch({
      jugadores <- tiros_final %>%
        filter(country %in% input$countryinput,
               team_name %in% input$teaminput,
               playerName %in% input$playerInput) %>%
        distinct(eventType) %>%
        arrange(eventType)},
      error = function(x){
        return('')
      })
  }) 
  
  observe({

    updateSelectizeInput(session = session, inputId = "eventInput",
                         choices = eventInput_filtro_fotmob()$eventType,
                         selected = eventInput_filtro_fotmob()$eventType
                         
   )})
  
  minuteInput_filtro_fotmob <- reactive({
    tryCatch({
      jugadores <- tiros_final %>%
        filter(country %in% input$countryinput,
               team_name %in% input$teaminput,
               playerName %in% input$playerInput,
               eventType %in% input$eventInput
               ) %>%
        distinct(min) %>%
        arrange(min)},
      error = function(x){
        return('')
      })
  }) 
  
  observe({
    
    updateSliderInput(session = session, inputId = "minuteInput",
                      min=min(minuteInput_filtro_fotmob()$min),
                      max=max(minuteInput_filtro_fotmob()$min),
                      value=c(min(minuteInput_filtro_fotmob()$min),
                              max(minuteInput_filtro_fotmob()$min))

  )})
  

  xGInput_filtro_fotmob <- reactive({
    req(input$minuteInput)
    tryCatch({
      jugadores <- tiros_final %>%
        filter(country %in% input$countryinput,
               team_name %in% input$teaminput,
               playerName %in% input$playerInput,
               eventType %in% input$eventInput,
               min >= input$minuteInput[1] & min <= input$minuteInput[2]
        ) %>%
        distinct(expectedGoals) %>%
        arrange(expectedGoals)},
      error = function(x){
        return('')
      })
  }) 
  
  observe({
    
    updateSliderInput(session = session, inputId = "xGInput",
                      min=min(xGInput_filtro_fotmob()$expectedGoals),
                      max=max(xGInput_filtro_fotmob()$expectedGoals),
                      value=c(min(xGInput_filtro_fotmob()$expectedGoals),
                              max(xGInput_filtro_fotmob()$expectedGoals))
                      
    )})
  
  xGOTInput_filtro_fotmob <- reactive({
    
    req(input$minuteInput,input$xGInput)
    
    tryCatch({
      jugadores <- tiros_final %>%
        filter(country %in% input$countryinput,
               team_name %in% input$teaminput,
               playerName %in% input$playerInput,
               eventType %in% input$eventInput,
               min >= input$minuteInput[1] & min <= input$minuteInput[2],
               expectedGoals >= input$xGInput[1] & expectedGoals <= input$xGInput[2]
        ) %>%
        distinct(expectedGoalsOnTarget) %>%
        arrange(expectedGoalsOnTarget)},
      error = function(x){
        return('')
      })
  }) 
  
  observe({
    
    updateSliderInput(session = session, inputId = "xGOTInput",
                      min=min(xGOTInput_filtro_fotmob()$expectedGoalsOnTarget),
                      max=max(xGOTInput_filtro_fotmob()$expectedGoalsOnTarget),
                      value=c(min(xGOTInput_filtro_fotmob()$expectedGoalsOnTarget),
                              max(xGOTInput_filtro_fotmob()$expectedGoalsOnTarget))
                      
    )})

  typeInput_filtro_fotmob <- reactive({
    
    req(input$minuteInput,input$xGInput,input$xGOTInput)
    
    tryCatch({
      jugadores <- tiros_final %>%
        filter(country %in% input$countryinput,
               team_name %in% input$teaminput,
               playerName %in% input$playerInput,
               eventType %in% input$eventInput,
               min >= input$minuteInput[1] & min <= input$minuteInput[2],
               expectedGoals >= input$xGInput[1] & expectedGoals <= input$xGInput[2],
               expectedGoalsOnTarget >= input$xGOTInput[1] & expectedGoalsOnTarget <= input$xGOTInput[2]
        )  %>%
        distinct(shotType) %>%
        arrange(shotType)},
      error = function(x){
        return('')
      })
  }) 
  
  observe({
    
    updateSelectInput(session = session, inputId = "typeInput",
                      choices=typeInput_filtro_fotmob()$shotType,
                      selected=typeInput_filtro_fotmob()$shotType)
                      
    })
  
  
  penalInput_filtro_fotmob <- reactive({
    
    req(input$minuteInput,input$xGInput,input$xGOTInput,input$typeInput)
    
    tryCatch({
      jugadores <- tiros_final %>%
        filter(country %in% input$countryinput,
               team_name %in% input$teaminput,
               playerName %in% input$playerInput,
               eventType %in% input$eventInput,
               min >= input$minuteInput[1] & min <= input$minuteInput[2],
               expectedGoals >= input$xGInput[1] & expectedGoals <= input$xGInput[2],
               expectedGoalsOnTarget >= input$xGOTInput[1] & expectedGoalsOnTarget <= input$xGOTInput[2],
               shotType %in% input$typeInput
        )  %>%
        distinct(situation) %>%
        arrange(situation)},
      error = function(x){
        return('')
      })
  }) 
  
  observe({
    
    updateSelectInput(session = session, inputId = "penalInput",
                      choices=penalInput_filtro_fotmob()$situation,
                      selected=penalInput_filtro_fotmob()$situation)
    
  })


  # Aquí vamos a pintar el campo esquema donde seleccionaremos los tiros.
  
  output$plot <- renderPlot({
    # marcamos nuevamente los req para que intente pintar cuando estos inputs sean no nulos
    req(input$playerInput,input$teaminput,input$countryinput,
        input$minuteInput,input$xGInput,input$penalInput,
        input$typeInput,input$xGOTInput,input$eventInput
        )
    
    # filtramos los tiros por pais, equipo, jugador, minuto, xG, tipo...
    Disparos_1<-tiros_final %>%
      filter( country == input$countryinput,
              team_name == input$teaminput,
              playerName == input$playerInput,
              min >= input$minuteInput[1] & min <= input$minuteInput[2],
              expectedGoals >= input$xGInput[1] & expectedGoals <= input$xGInput[2],
              situation %in% input$penalInput,
              shotType %in% input$typeInput,
              expectedGoalsOnTarget >= input$xGOTInput[1] & expectedGoalsOnTarget <= input$xGOTInput[2],
              eventType %in% input$eventInput
              ) 
    

    
    # Las coordenadas no están muy bien ajustadas. el campo mide 106m y en fotmob 105 pero 
    # es algo orientativo
    
    ggplot(data=Disparos_1, aes(x=xfin, y=yfin)) +
      xlim(c(-10,10600+10)) + ylim(c(-10,7040+10)) +
      theme_blankPitch() +
      geom_rect(aes(xmin=0, xmax=10600, ymin=0, ymax=7040), fill = "#ffffff", colour = "#000000") +
      geom_rect(aes(xmin=0, xmax=TheBoxHeight[1], ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = "#ffffff", colour = "#000000") +
      geom_rect(aes(xmin=TheBoxHeight[2], xmax=10600, ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = "#ffffff", colour = "#000000") +
      geom_rect(aes(xmin=0, xmax=box6yardHeight[1], ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = "#ffffff", colour = "#000000")  +
      geom_rect(aes(xmin=box6yardHeight[2], xmax=10600, ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = "#ffffff", colour = "#000000")  +
      geom_segment(aes(x = 10600/2, y = ymin, xend = 10600/2, yend = 7040),colour = "#000000") +
      geom_path(data=Dleft, aes(x=x,y=y), colour = "#000000") +
      geom_path(data=Dright, aes(x=x,y=y), colour = "#000000") +
      geom_path(data=center_circle, aes(x=x,y=y), colour = "#000000") +
      geom_point(aes(x = penspot , y = 7040/2), colour = "#000000") +
      geom_point(aes(x = (10600-(penspot)) , y = 7040/2), colour = "#000000") +
      geom_point(aes(x = (10600/2) , y = 7040/2), colour = "#000000") +
      geom_segment(aes(x = xmin, y = GoalPosts[1], xend = xmin, yend = GoalPosts[2]),colour = "#000000", size = 1) +
      geom_segment(aes(x = 10600, y = GoalPosts[1], xend = 10600, yend = GoalPosts[2]),colour = "#000000", size = 1)+
      theme(legend.position="bottom") +
      # Aquí pintamos los tiros seleccionados
      geom_point() +
      # Y aquí pintamos en otro color los tiros que se han seleccionado con el selector si lo hemos hecho
      geom_point(
        data = brushedPoints(Disparos_1, brush),
        color = "#79D8CB",
        size = 2
      )
    },width = 500,height = 200)
  
  # Elemento brush que empieza como nulo
  brush <- NULL
  
  # Como brush es una variable con este comando convertimos en reactiva una variable
  makeReactiveBinding("brush")

  # aquí metemos en la variable brush los elementos seleccionadossi hemos seleccionado 
  observeEvent(input$plotBrush, {
    brush <<- input$plotBrush
  })
  
  # para limpiar lo seleccionado
  observeEvent(input$clearBrush, {
    session$resetBrush("plotBrush")
  })
  
  # para resetear el gráfico
  observeEvent(input$resetPlot, {
    session$resetBrush("plotBrush")
    brush <<- NULL
  })
  
  
  # Aquí ya pintamos la porteria 
  output$plot2 <- renderPlot({
    
    req(input$playerInput,input$teaminput,input$countryinput,
        input$minuteInput,input$xGInput,input$penalInput,
        input$typeInput,input$xGOTInput,input$eventInput
    )
    
    # Aqui le decimos que si el elemento input bursh (donde se mete si hemos seleccionado)
    # es nulo coja todos los tiros
    if(is.null(input$plotBrush)){
      Disparos_2<-tiros_final %>%
        filter( country == input$countryinput,
                team_name == input$teaminput,
                playerName == input$playerInput,
                min >= input$minuteInput[1] & min <= input$minuteInput[2],
                expectedGoals >= input$xGInput[1] & expectedGoals <= input$xGInput[2],
                situation %in% input$penalInput,
                shotType %in% input$typeInput,
                expectedGoalsOnTarget >= input$xGOTInput[1] & expectedGoalsOnTarget <= input$xGOTInput[2],
                eventType %in% input$eventInput
        )
    }
    # Si no es nulo selecciona en función de las coordenadas del rectangulo seleccionado
    else{

        Disparos_2<-tiros_final %>%
          filter( country == input$countryinput,
                  team_name == input$teaminput,
                  playerName == input$playerInput,
                  min >= input$minuteInput[1] & min <= input$minuteInput[2],
                  expectedGoals >= input$xGInput[1] & expectedGoals <= input$xGInput[2],
                  situation %in% input$penalInput,
                  shotType %in% input$typeInput,
                  expectedGoalsOnTarget >= input$xGOTInput[1] & expectedGoalsOnTarget <= input$xGOTInput[2],
                  eventType %in% input$eventInput,
                  # aquí tenéis como se selecciona. 
                  xfin>input$plotBrush[[1]] & xfin<input$plotBrush[[2]] & yfin>input$plotBrush[[3]] & yfin<input$plotBrush[[4]]
          )
    }

    
    
    Disparos_3 <- Disparos_2  %>%
      mutate(yporteria=yporteria*3.7527,
             xporteria=xporteria*3.71) %>%
      mutate(
        yporteria=case_when(
          yporteria>2.5415 ~ 3,
          TRUE ~ yporteria),
        xporteria=case_when(
          xporteria<=0 ~ -0.5,
          xporteria>7.41 ~ 8,
          TRUE ~ xporteria)
      ) 
    
    print("D3")
    print(Disparos_3)

    img <- readPNG('image.png')


    colores<-c('Post', 'Goal', 'Miss', 'AttemptSaved','Blocked')


    ggplot(Disparos_3, aes(xporteria, yporteria,fill=eventType,label=round(expectedGoals,2)))+
      annotation_custom(rasterGrob(img,
                                   width = unit(1,"npc"),
                                   height = unit(1,"npc")),
                        0.1, 7.40, 0.1, 2.51) +
      geom_point(aes(size=expectedGoals),alpha=0.6,shape=21,stroke=1) +
      scale_fill_manual(values = c("Post" = "orange", "Goal" = "green", "Miss" = "red","AttemptSaved"="yellow","Blocked"="blue"),
                        name="Resultado tiro") +
      xlim(-1,9)+
      ylim(0,4)+
      scale_discrete_identity(aesthetics = c("color", "orientation")) +
     xlab("")+
      ylab("")+
      theme_minimal() +
      theme(
        axis.line = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.box="vertical",
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size=8)
      )


  })
  
  
}

shinyApp(ui, server)