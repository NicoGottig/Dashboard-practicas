library(shinydashboard)
library(shiny)
library(tidyverse)
library(googledrive)
library(googlesheets4)

# options(gargle_oauth_cache = ".secrets")
# drive_auth(cache = ".secrets", email = "fcg_pasantias@uader.edu.ar")
# gs4_auth(token = drive_token())

### Interface ---------------------------------------------------------------

ui <- dashboardPage(

  # 1. Encabezado
  dashboardHeader(title = "Monitor de Pasantías."),
  
  # 2. Barra
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  
  # 3. Cuerpo
  dashboardBody(
    tabItem(
     h1(paste0("Pasantías del año ", "2023")),
     fluidRow(
       valueBoxOutput("Holaaa")
     )
       
    )
  )
)
### Servidor ---------------------------------------------------------------

server <- function(input, output) {
  # Carga y transformación -----
  
  # Read Google Sheets
  # raw_pna <- read_sheet("https://docs.google.com/spreadsheets/d/10Y0Oq0Vf5dyEoMT23GdvBn-7ZZY0VuccYv-afySlrms/edit#gid=1669228784")
  # raw_cpo <- read_sheet("https://docs.google.com/spreadsheets/d/10Y0Oq0Vf5dyEoMT23GdvBn-7ZZY0VuccYv-afySlrms/edit#gid=1669228784", range = "Pasantias crespo")
  # 
  # raw_pna <- raw_pna %>% 
  #   filter(!is.na(raw_pna[1]))
  # 
  # raw_pna <- raw_pna[, 5:14]
  # raw_cpo <- raw_cpo[, 4:11]
  # 
  # # Columnas y fusion
  # raw_pna <- raw_pna %>% 
  #   select(carrera = ...5, 
  #          organismo = ...6,
  #          inicio = ...7,
  #          fin = ...8,
  #          renov1 = ...9,
  #          fin1 = ...10,
  #          renov2 = ...11,
  #          fin2 = ...12,
  #          renov3 = RENOVACIÓN,
  #          fin3 = ...14)
  # 
  # raw_cpo <- raw_cpo %>% 
  #   select(carrera = ...4,
  #          organismo = ...5,
  #          inicio = ...6,
  #          fin = ...7,
  #          renov1 = RENOVACIÓN,
  #          fin1 = ...9,
  #          renov2 = ...10,
  #          fin2 = ...11) %>% 
  #   mutate(renov3 = NA,
  #          fin3 = NA) 
  # 
  # raw_pna["sede"] <- "Parana"
  # raw_cpo["sede"] <- "Crespo"
  # 
  # raw <- bind_rows(raw_cpo, raw_pna)
  # raw <- as.data.frame(raw)
  # rm(raw_pna, raw_cpo)
  # 
  # # Filtrado
  # raw <- raw %>% 
  #   filter(!(carrera %in% c("CARRERA", "CARRERAS", NA)))
  # 
  # # Computo de fechas
  # inicio <- vector()
  # fin <- vector()
  # prirenov <- vector()
  # segrenov <- vector()
  # tercrenov <- vector()
  # segfin <- vector()
  # tercfin <- vector()
  # cuarfin <- vector()
  # 
  # for (i in 1:nrow(raw)) {
  #   if (is.null(unlist(raw$inicio[i]))) { # Inicio
  #     inicio[i] <- NA
  #   }else{
  #     inicio[i] <- unlist(raw$inicio[i])
  #   }
  #   
  #   if (is.null(unlist(raw$fin[i]))) { # Fin
  #     fin[i] <- NA
  #   }else{
  #     fin[i] <- unlist(raw$fin[i])
  #   }
  #   
  #   if (is.null(unlist(raw$renov1[i]))) { # Primera renovacion
  #     prirenov[i] <- NA
  #   }else{
  #     prirenov[i] <- unlist(raw$renov1[i])
  #   }
  #   
  #   if (is.null(unlist(raw$renov2[i]))) { # Segunda renovacion
  #     segrenov[i] <- NA
  #   }else{
  #     segrenov[i] <- unlist(raw$renov2[i])
  #   }
  #   
  #   if (is.null(unlist(raw$renov3[i]))) { # Tercera renovacion
  #     tercrenov[i] <- NA
  #   }else{
  #     tercrenov[i] <- unlist(raw$renov3[i])
  #   }
  #   
  #   if (is.null(unlist(raw$fin1[i]))) { # Segundo Fin
  #     segfin[i] <- NA
  #   }else{
  #     segfin[i] <- unlist(raw$fin1[i])
  #   }
  #   
  #   if (is.null(unlist(raw$fin2[i]))) { # Tercer Fin
  #     tercfin[i] <- NA
  #   }else{
  #     tercfin[i] <- unlist(raw$fin2[i])
  #   }
  #   
  #   if (is.null(unlist(raw$fin3[i]))) { # Cuarto Fin
  #     cuarfin[i] <- NA
  #   }else{
  #     cuarfin[i] <- unlist(raw$fin3[i])
  #   }
  # }
  # 
  # # Transformacion de carreras
  # tipo <- vector()
  # grado <- vector()
  # carrera <- vector()
  # 
  # raw$carrera <- chartr("áéíóú", "aeiou", raw$carrera)
  # grado <- ifelse(grepl("Tec", raw$carrera), "Tec.", "Lic.")
  # 
  # regexp <- c("Administracion", "Marketing", "Archi", "Comerc", "Turismo", "Administracion Publica", "Bibliot", "Gerenc")
  # reemp <- c("Admin.", "Marketing", "Archiv.", "Com. Int.", "Turismo", "Adm. Pub.", "Biblio.", "TUEGSG")
  # 
  # for (i in 1:nrow(raw)) {
  #   tipo[grepl(regexp[i], raw$carrera)] <- reemp[i]
  # }
  # 
  # carrera <- paste0(tipo, " (", grado, ")")
  # 
  # # Transformación de Organismos
  # tipo_org <- vector()
  # raw$organismo <- tolower(raw$organismo)
  # 
  # org_pub <- c("muni", 
  #              "ater", 
  #              "sec.", 
  #              "copnaf",
  #              "contadur", 
  #              "museo", 
  #              "min",
  #              "utn",
  #              "uader",
  #              "uner",
  #              "caja")
  # 
  # for (i in 1:nrow(raw)) {
  #   if (any(grepl(paste(org_pub, collapse = "|"), raw$organismo[i]))) {
  #     tipo_org[i] <- "Público"
  #   }else{
  #     tipo_org[i] <- "Privado/mixto"
  #   }
  # }
  # 
  # # Crear dataframe con fechas
  # raw <- bind_cols(carrera = carrera, 
  #                  organismo = raw$organismo, 
  #                  tipo_org = tipo_org,
  #                  inicio1 = as.POSIXct(inicio, tz = "UTZ", origin = "1970-01-01"), 
  #                  fin1 = as.POSIXct(fin, tz = "UTZ", origin = "1970-01-01"), 
  #                  inicio2 = as.POSIXct(prirenov, tz = "UTZ", origin = "1970-01-01"), 
  #                  fin2 = as.POSIXct(segfin, tz = "UTZ", origin = "1970-01-01"), 
  #                  inicio3 = as.POSIXct(segrenov, tz = "UTZ", origin = "1970-01-01"), 
  #                  fin3 = as.POSIXct(tercfin, tz = "UTZ", origin = "1970-01-01"), 
  #                  inicio4 = as.POSIXct(tercrenov, tz = "UTZ", origin = "1970-01-01"), 
  #                  fin4 = as.POSIXct(cuarfin, tz = "UTZ", origin = "1970-01-01"),
  #                  sede = raw$sede)
  # 
  # library(lubridate)
  # col_fechas <- colnames(raw[4:11])
  # raw[col_fechas] <- lapply(raw[col_fechas], ymd)
  # 
  # # Construir el estado del pasante
  # estado <- vector()
  # hoy <- lubridate::today()
  # 
  # for (i in 1:nrow(raw)) {
  #   ref <- NULL
  #   ref <- max(raw$fin1[i], raw$fin2[i], raw$fin3[i], raw$fin4[i], na.rm = TRUE)
  #   
  #   if (hoy < ref) {
  #     estado[i] <- "Activo"
  #   }else{
  #     estado[i] <- "Finalizado"
  #   }
  #   
  #   if (hoy < raw$inicio1[i]) {
  #     estado[i] <- "Por iniciar"
  #   }
  # }
  # 
  # df <- data.frame(carrera = raw$carrera,
  #                  organismo = raw$organismo,
  #                  tipo_org = raw$tipo_org,
  #                  estado = estado,
  #                  inicio1 = raw$inicio1,
  #                  fin1 = raw$fin1,
  #                  inicio2 = raw$inicio2,
  #                  fin2 = raw$fin2,
  #                  inicio3 = raw$inicio3,
  #                  fin3 = raw$fin3,
  #                  inicio4 = raw$inicio4,
  #                  fin4 = raw$fin4)
  # 
  # rm(list = setdiff(ls(), "df"))
  
  
  
  
  # Gráficos
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
