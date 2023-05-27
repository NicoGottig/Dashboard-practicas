library(shiny)
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(plotly)
library(shinydashboard)
library(lubridate)
library(janitor)

# Definición de temas ----------------------------------------------------------
# Tema
library(fresh)

# create_theme(
#   adminlte_color(
#     light_blue = "#820000"
#   ),
#   
#   adminlte_sidebar(
#     width = "350px",
#     dark_bg = "#66717a",
#     dark_hover_bg = "#be615f",
#     dark_color = "#820000"
#   ),
#   
#   adminlte_global(
#     content_bg = "#fcfcfc",
#     box_bg = "#FFFFFF", 
#     info_box_bg = "#FFFFFF"
#   ),
#   
#   output_file = "app/www/myCustomFresh.css"
#   
# )

# Código
# Df ---------------------------------------
# Read Google Sheets

# Limpieza y manipulación ------------------------------------------------------
# Dataframe recortado
raw_pna <- raw_pna %>%
  filter(!is.na(raw_pna[1]))

raw_pna <- raw_pna[, 5:14]
raw_cpo <- raw_cpo[, 4:11]

# Columnas y fusion
raw_pna <- raw_pna %>%
  select(carrera = ...5,
         organismo = ...6,
         inicio = ...7,
         fin = ...8,
         renov1 = ...9,
         fin1 = ...10,
         renov2 = ...11,
         fin2 = ...12,
         renov3 = RENOVACIÓN,
         fin3 = ...14)

raw_cpo <- raw_cpo %>%
  select(carrera = ...4,
         organismo = ...5,
         inicio = ...6,
         fin = ...7,
         renov1 = RENOVACIÓN,
         fin1 = ...9,
         renov2 = ...10,
         fin2 = ...11) %>%
  mutate(renov3 = NA,
         fin3 = NA)

raw_pna["sede"] <- "Parana"
raw_cpo["sede"] <- "Crespo"

raw <- rbind(raw_cpo, raw_pna)
raw <- as.data.frame(raw)
rm(raw_pna, raw_cpo)

# Filtrado
raw <- raw %>%
  filter(!(carrera %in% c("CARRERA", "CARRERAS", NA)))

# Computo de fechas
inicio <- vector()
fin <- vector()
prirenov <- vector()
segrenov <- vector()
tercrenov <- vector()
segfin <- vector()
tercfin <- vector()
cuarfin <- vector()

for (i in 1:nrow(raw)) {
  if (is.null(unlist(raw$inicio[i]))) { # Inicio
    inicio[i] <- NA
  }else{
    inicio[i] <- unlist(raw$inicio[i])
  }

  if (is.null(unlist(raw$fin[i]))) { # Fin
    fin[i] <- NA
  }else{
    fin[i] <- unlist(raw$fin[i])
  }

  if (is.null(unlist(raw$renov1[i]))) { # Primera renovacion
    prirenov[i] <- NA
  }else{
    prirenov[i] <- unlist(raw$renov1[i])
  }

  if (is.null(unlist(raw$renov2[i]))) { # Segunda renovacion
    segrenov[i] <- NA
  }else{
    segrenov[i] <- unlist(raw$renov2[i])
  }

  if (is.null(unlist(raw$renov3[i]))) { # Tercera renovacion
    tercrenov[i] <- NA
  }else{
    tercrenov[i] <- unlist(raw$renov3[i])
  }

  if (is.null(unlist(raw$fin1[i]))) { # Segundo Fin
    segfin[i] <- NA
  }else{
    segfin[i] <- unlist(raw$fin1[i])
  }

  if (is.null(unlist(raw$fin2[i]))) { # Tercer Fin
    tercfin[i] <- NA
  }else{
    tercfin[i] <- unlist(raw$fin2[i])
  }

  if (is.null(unlist(raw$fin3[i]))) { # Cuarto Fin
    cuarfin[i] <- NA
  }else{
    cuarfin[i] <- unlist(raw$fin3[i])
  }
}

# Transformacion de carreras
tipo <- vector()
grado <- vector()
carrera <- vector()

raw$carrera <- chartr("áéíóú", "aeiou", raw$carrera)
grado <- ifelse(grepl("Tec", raw$carrera), "Tec.", "Lic.")

regexp <- c("Administracion", "Marketing", "Archi", "Comerc", "Turismo", "Administracion Publica", "Bibliot", "Gerenc")
reemp <- c("Admin.", "Marketing", "Archiv.", "Com. Int.", "Turismo", "Adm. Pub.", "Biblio.", "TUEGSG")

for (i in 1:nrow(raw)) {
  tipo[grepl(regexp[i], raw$carrera)] <- reemp[i]
}

carrera <- paste0(tipo, " (", grado, ")")

# Transformación de Organismos
tipo_org <- vector()
raw$organismo <- tolower(raw$organismo)

org_pub <- c("muni",
             "ater",
             "sec.",
             "copnaf",
             "contadur",
             "museo",
             "min",
             "utn",
             "uader",
             "uner",
             "caja")

for (i in 1:nrow(raw)) {
  if (any(grepl(paste(org_pub, collapse = "|"), raw$organismo[i]))) {
    tipo_org[i] <- "Público"
  }else{
    tipo_org[i] <- "Privado/mixto"
  }
}

# Crear dataframe con fechas
raw <- bind_cols(carrera = carrera,
                 organismo = raw$organismo,
                 tipo_org = tipo_org,
                 inicio1 = as.POSIXct(inicio, tz = "UTZ", origin = "1970-01-01"),
                 fin1 = as.POSIXct(fin, tz = "UTZ", origin = "1970-01-01"),
                 inicio2 = as.POSIXct(prirenov, tz = "UTZ", origin = "1970-01-01"),
                 fin2 = as.POSIXct(segfin, tz = "UTZ", origin = "1970-01-01"),
                 inicio3 = as.POSIXct(as.numeric(segrenov), tz = "UTZ", origin = "1970-01-01"),
                 fin3 = as.POSIXct(as.numeric(tercfin), tz = "UTZ", origin = "1970-01-01"),
                 inicio4 = as.POSIXct(as.numeric(tercrenov), tz = "UTZ", origin = "1970-01-01"),
                 fin4 = as.POSIXct(as.numeric(cuarfin), tz = "UTZ", origin = "1970-01-01"),
                 sede = raw$sede)

library(lubridate)
col_fechas <- colnames(raw[4:11])
raw[col_fechas] <- lapply(raw[col_fechas], ymd)

# Construir el estado del pasante
estado <- vector()
hoy <- lubridate::today()

for (i in 1:nrow(raw)) {
  ref <- NULL
  ref <- max(raw$fin1[i], raw$fin2[i], raw$fin3[i], raw$fin4[i], na.rm = TRUE)

  if (hoy < ref) {
    estado[i] <- "Activo"
  }else{
    estado[i] <- "Finalizado"
  }

  if (hoy < raw$inicio1[i]) {
    estado[i] <- "Por iniciar"
  }
}

df <- data.frame(carrera = raw$carrera,
                 organismo = raw$organismo,
                 tipo_org = raw$tipo_org,
                 estado = estado,
                 inicio1 = raw$inicio1,
                 fin1 = raw$fin1,
                 inicio2 = raw$inicio2,
                 fin2 = raw$fin2,
                 inicio3 = raw$inicio3,
                 fin3 = raw$fin3,
                 inicio4 = raw$inicio4,
                 fin4 = raw$fin4)

df["anio"] <- as.character(year(df$inicio1))
df["mes"] <- as.character(month(df$inicio1))
rm(list = setdiff(ls(), "df"))

# # Guardado
# write_delim(df, delim = "\t", file = "app/dffull.csv")
# df <- read_delim("dffull.csv", delim = "\t")
# df["mes"] <- as.character(lubridate::month(df$inicio1))

### Interface ------------------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "Tablero de Pasantías",
                  tags$li(class = "dropdown",
                    tags$img(
                      src= 'FCG-_Blanco.png',
                      style = 'height: 40px; width: 131.2px;  
                      position: center;
                      transform: translateX(-50%);
                      margin-top: 7px;',
                    ),
                    tags$style(".main-header {max-height: 55px}"),
                    tags$style(".main-header .logo {height: 55px}"))),
  
  # 1. Sidebar
  dashboardSidebar(
    width = 250,
    
    tags$blockquote(class = "blockquote",
                    hr(),
                    h3("Opciones: "),
                    # Seleccionar año
                    selectizeInput(
                      'selanio', 
                      'Año: ',
                      choices = c(2021, 2022, 2023),
                      selected = "2022"
                    ),
                    
                    
                    # Seleccionar tipo de pasante
                    selectizeInput(
                      'seltipo', 
                      'Pasantes: ',
                      choices = c("Todos", unique(df$estado)),
                      selected = "Todos"
                    ),
                    hr(),
    
    # Texto
    h3("  Aclaraciones:"),
    tags$p("Para volver al tamaño original de los gráficos deberá
           hacer doble click en el cuadro."),
    br(),
    p("Los ", strong("años"), " consideran los inicios de pasantías independientemente del momento de consulta, entre el 1 de enero y
          el 31 de diciembre.", align = "justify"),
    br(),
    p("Los pasantes ", strong("activos"), " representan la cantidad de estudiantes que, al día de la consulta, se encuentran en los
           límites de las fechas establecidas en el último acuerdo individual firmado.", align = "justify"),
    hr(),
    h4(strong("Ejemplo de consulta: ")),
    p("Año: 2022, pasantes: Activos, Resultado: 18 pasantes en organismos privados iniciaron en 2022 y continúan activos.", align = "justify"),
    hr(),
    p(em("Ultima actualización: 5/02/2023."))
    )),
  
  # 2. Cuerpo
  dashboardBody(
    
    br(),
    br(),
    # Colors
    use_theme("myCustomFresh.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    
    # Primera columna - 3 Botones
    fluidRow(
      valueBoxOutput("total.activos"),
      valueBoxOutput("total.iniciar"),
      valueBoxOutput("total.finalizados")
    ),
    
    br(),
    br(),
    
    # Segunda columna - Por carrera y tipo de organismo
    fluidRow(
      box(title = "Pasantes por Tipo de Organismo",
          solidHeader = T,
          width = 4,
          collapsible = T,
          plotlyOutput("plt_org")),
      box(title = "Pasantes por Carrera", solidHeader = T,
          width = 8, collapsible = T,
          plotlyOutput("plt_carrera"))
    ),
    
    # Tercera columna - Mejor mes y Por participación relativa en sector
    fluidRow(
      box(title = "Pasantías por mes",
          solidHeader = T,
          width = 6,
          collapsible = T,
          plotlyOutput("mejormes")),
        box(title = "Pasantías por carrera y tipo de organismo",
            solidHeader = T,
            width = 6,
            collapsible = T,
            plotlyOutput("combinado")
      )
    )
  ) 
)


### Servidor -------------------------------------------------------------------

server <- function(input, output, session) {

  # 1. Botones ----------------------------------------------
  output$total.activos <- renderValueBox({ 
    
    total.activos <- df %>% 
      group_by(estado) %>% 
      summarise(n = n()) %>% 
      filter(estado == "Activo")
    
    valueBox(total.activos$n, "Pasantes activos", icon = icon("stats", lib = "glyphicon"), color = "blue")
  
  })
  
  output$total.finalizados <- renderValueBox({ 
    
    total.finalizados <- df %>% 
      group_by(estado) %>% 
      summarise(n = n()) %>% 
      filter(estado == "Finalizado")
    
    valueBox(total.finalizados$n, "Pasantes que culminaron", icon = icon(lib = "glyphicon", "circle-arrow-down"), color = "red")
    
  })
  
  output$total.iniciar <- renderValueBox({ 

    total.iniciar <- df %>% 
      group_by(estado) %>% 
      summarise(n = n()) %>% 
      filter(estado == "Por iniciar")
    
    # Si no hay pasantes activos informar "0"
    if (nrow(total.iniciar) != 0) {
      ninic <- total.iniciar$n
      valueBox(as.character(ninic), "Pasantes por iniciar", icon = icon(lib = "glyphicon", "circle-arrow-up"), color = "aqua")
    }else{
      ninic <- 0
      valueBox(as.character(ninic), "Pasantes por iniciar", icon = icon(lib = "glyphicon", "circle-arrow-up"), color = "aqua")
    }
    
  })
  
  
  # 2. Plot -------------------------------------------------
  
  # Df condicional
  dfc <- reactive({
    if(input$seltipo != "Todos"){
      tmp <- df %>% 
        filter(estado == input$seltipo)
    }else{
      tmp <- df
    }
  })
  
  # Plot por carrera
  output$plt_carrera <- renderPlotly({
    
    dfc() %>% 
      filter(anio == input$selanio) %>% 
      group_by(carrera) %>% 
      summarise(n = n()) %>% 
      mutate(n = n, p = round(n/sum(n), 2)*100) %>% 
      arrange(desc(p)) %>%
      plot_ly(y = ~carrera,
              x = ~n,
              type = "bar",
              marker = list(color = '#AF2D2D'),
              hovertemplate = paste(
                "<b>Carrera:</b> %{y}<br>",
                "<b>Pasantes:</b> %{x}<br>",
                "<extra></extra>"
              )) %>% 
      layout(yaxis = list(categoryorder = "total ascending",
                          title = ""),
             xaxis = list(title = "Cantidad"))

  })
  
  # Plot por tipo de organismo 
  output$plt_org <- renderPlotly({
    
    dfc() %>% 
      filter(anio == input$selanio) %>% 
      group_by(tipo_org) %>% 
      summarise(n = n()) %>% 
      mutate(n = n, p = round(n/sum(n), 2)*100) %>% 
      arrange(desc(p)) %>%
      plot_ly(x = ~tipo_org,
              y = ~n,
              text = ~paste(p, "%"),
              type = "bar",
              marker = list(color = '#AF2D2D'),
              hovertemplate = paste(
                "<b>Tipo de organismo:</b> %{x}<br>",
                "<b>Pasantes:</b> %{y}<br>",
                "<b>Porc.: </b> %{text} <br>",
                "<extra></extra>"
              )) %>% 
      layout(xaxis = list(categoryorder = "total ascending",
                          title = ""),
             yaxis = list(title = "Cantidad"))
  })
  
  # Plot de mejor mes
  output$mejormes <- renderPlotly({
    
    dfc() %>% 
      filter(anio == input$selanio) %>% 
      group_by(mes) %>% 
      summarise(n = n()) %>% 
      mutate(n = n, p = round(n/sum(n), 2)*100) %>% 
      arrange(desc(p)) %>%
      plot_ly(x = ~mes,
              y = ~n,
              type = "bar",
              marker = list(color = '#AF2D2D'),
              hovertemplate = paste(
                "<b>Mes:</b> %{x}<br>",
                "<b>Pasantes:</b> %{y}<br>",
                "<extra></extra>"
              )) %>% 
      layout(xaxis = list(categoryorder = "array",
                          title = "mes",
                          categoryarray = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)),
             yaxis = list(title = "Cantidad"))
    
  })
  
  # Plot de carrera y tipo de organismo
  
  # tabla condicional
  df_tabla <- reactive({
    
    if(input$seltipo != "Todos"){
      tmp <- df %>% 
        filter(estado == input$seltipo & anio == input$selanio) %>% 
        tabyl(carrera, tipo_org) %>% 
        adorn_percentages("row") %>% 
        adorn_rounding(digits = 2, rounding = "half to even")
      
      if (!('Privado/mixto' %in% colnames(tmp))) {
        tmp['Privado/mixto'] = 0
      }
      
      if (!('Público' %in% colnames(tmp))) {
        tmp['Público'] = 0
      }
      
      
      if (!('carrera' %in% colnames(tmp))) {
        tmp['carrera'] = 0
      }
      
    }else{
      tmp <- df %>% 
        filter(anio == input$selanio) %>% 
        tabyl(carrera, tipo_org) %>% 
        adorn_percentages("row") %>% 
        adorn_rounding(digits = 2, rounding = "half to even") 
      
      if (!('Privado/mixto' %in% colnames(tmp))) {
        tmp['Privado/mixto'] = 0
      }
      
      if (!('Público' %in% colnames(tmp))) {
        tmp['Público'] = 0
      }
      
      if (!('carrera' %in% colnames(tmp))) {
        tmp['carrera'] = 0
      }
      
      
    }
    
    

    tmp <-as.data.frame(tmp)
    
  })
  
  # Grafico combinado
  output$combinado <- renderPlotly({
    
    df_tabla() %>% 
      plot_ly(x = ~carrera,
              y = ~`Privado/mixto`,
              type = "bar",
              name = "Privado/mixto",
              marker = list(color = '#AF2D2D')) %>% 
      add_trace(y = ~Público,
                marker = list(color = '#DBC8AC'),
                name = "Público") %>%
      layout(title = list(),
             xaxis = list(title = ""), 
             barmode = "stack")
    
  })

}

shinyApp(ui = ui, server = server)
