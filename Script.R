library(googledrive)
library(googlesheets4)
library(tidyverse)

# Read Google Sheets
raw_pna <- read_sheet()
raw_cpo <- read_sheet()

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

rm(list = setdiff(ls(), "df"))

### Componentes del sistema ----------------------------------------------------
library(plotly)

## Sistema transversal

# 3 Botones con cantidad activos, finalizados y por iniciar.
df %>% 
  group_by(estado) %>% 
  summarise(n = n()) 

# Filtro de: todos / activos / inactivos / por comenzar.

# 1 - Gráfico de pasantías por tipo de organismo
tabla_tipoorg <- df %>% 
  group_by(tipo_org) %>% 
  summarise(n = n()) %>% 
  mutate(n = n, p = round(n/sum(n),2)*100)

colors <- c('rgba(202,25,18,0.9)', 'rgb(128,133,133)')

plot_ly(tabla_tipoorg,
               labels = ~tipo_org, 
               values = ~p, 
               type = 'pie',
               textposition = "inside",
               textinfo = "label+percent",
               insidetextfont = list(color = "#FFFFFF", size = 11),
               hoverinfo = "text",
               text = ~paste(n, "pasantias."),
               marker = list(colors = colors,
                            line = list(color = "#000015", 
                                        width = 1)),
               showlegend = FALSE) %>% 
  
  layout(title = list(text='Pasantías por tipo de organismo', 
                      y = 0.991, 
                      x = 0.541, 
                      xanchor = 'center', 
                      yanchor =  'top'))

# 2 - Grafico de pasantías por carrera
tabla_carreras <- df %>% 
  group_by(carrera) %>% 
  summarise(n = n()) %>% 
  mutate(n = n, p = round(n/sum(n), 2)*100) %>% 
  arrange(desc(p))

tabla_carreras %>% 
  plot_ly(y = ~carrera,
          x = ~n,
          type = "bar",
          marker = list(color = 'rgba(202,25,18,0.9)')) %>% 
  layout(title = list(text = "Pasantías por carrera",
                                            y = 0.991,
                                            x = 0.541),
                               yaxis = list(categoryorder = "total ascending",
                                            title = ""),
                               xaxis = list(title = "Cantidad"))
          
# 3 - Grafico de pasantías por carrera y tipo de organismo
library(janitor)

tabla_orgcarrera <- df %>% 
  tabyl(carrera, tipo_org) %>% 
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0)

colors <- c('rgba(202,25,18,0.9)', 'rgb(128,133,133)')

tabla_orgcarrera %>% 
  plot_ly(x = ~carrera,
          y = ~`Privado/mixto`,
          type = "bar",
          name = "Privado/mixto",
          marker = list(color = 'rgba(202,25,18,0.9)')) %>% 
  add_trace(y = ~Público,
            marker = list(color = 'rgb(128,133,133)'),
            name = "Público") %>% 
  layout(title = list(text = "Pasantías por carreras, según tipo de organismo.",
                      x = 0.5,
                      y = 0.981),
         xaxis = list(title = "Count"), barmode = "stack")

# 4 - Gráfico de promedio de días para iniciar
inicio <- sort(df$inicio1) 

dias <- inicio %>% 
  diff(units = "days") %>% 
  as.numeric()

hist(dias, col = "orange")

# 5 - Duracion promedio de un pasante con y sin renovacion
df["renovacion"] <- ifelse(!is.na(df$inicio2), "Renovado", "No Renovado")
df["max"] <- NULL

# Pasar a sapply
for (i in 1:nrow(df)) {
  df$max[i] <- max(ymd(df$fin1[i]), ymd(df$fin2[i]),
                   ymd(df$fin3[i]), ymd(df$fin4[i]), na.rm = TRUE)
}
df$max <- as.Date(df$max, tz = "UTZ", origin = "1970-01-01")

df["duracion_dias"] <- difftime(df$max, df$inicio1, units = "days")

df %>% 
  group_by(renovacion) %>% 
  summarise(dias = as.numeric(round(mean(duracion_dias))),
            meses = as.numeric(round(mean(duracion_dias/31))))

# 6. Promedio de dias hasta un inicio.
library(EnvStats)
eexp(dias, ci = TRUE, conf = 0.95, ci.type = "upper")

## Sistema longitudinal
df["año"] <- lubridate::year(df$inicio1)
df["mes"] <- lubridate::month(df$inicio1)

# Frecuencia por año y por mes
df %>% 
  group_by(año) %>% 
  summarise(n = n())

df %>% 
  group_by(mes) %>% 
  summarise(n = n())

df %>% 
  tabyl(mes, año) 

