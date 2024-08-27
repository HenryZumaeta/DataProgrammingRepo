# ==============================================================================.
# TEMA      : Gráficas de series de tiempo climáticas
# AUTOR     : Henry P. Zumaeta Lozano
# CORREO    : henry.zumaeta.l@uni.pe
# VERSION   : 1.0
# CREADO    : 26/08/2024
# VALIDADO  : 26/08/2024
# ==============================================================================.

# Descripción: Graficar las variables climáticas en función del tiempo.

#_______________________________ CONFIGURACIONES _______________________________ ----

# ******************************************************************************
# CONFIGURACIONES INICIALES ----
# ******************************************************************************

## +++++++++++++++++++++++++++++++
## LIMPIAR ESPACIO DE TRABAJO ----
## +++++++++++++++++++++++++++++++

rm(list = ls())

## ++++++++++++++++++++++++++++++
## CARGA DE LIBRERÍAS A USAR ----
## ++++++++++++++++++++++++++++++

# install.packages("pacman")
library(pacman)

p_load(tidyverse, extrafont, readxl, dplyr, modeest, e1071, gghighlight)

# font_import() # Solo se corre por única vez.
loadfonts(device="win", quiet=F)

## +++++++++++++++++++++
## FIJAR DIRECTORIO ----
## +++++++++++++++++++++

setwd(r"(D:\ZEngi\Hydro\Projects\Morin\HIDROLOGIA\EXCEL\00_CLIMA)")
# getwd()

## +++++++++++++++++++++
## CARGA DE LA DATA ----
## +++++++++++++++++++++
df <- read_excel("IN/CLIMATOLOGIA.xlsx", sheet = "Climatologia")

#_____________________________________ ETL _____________________________________ ----

# ******************************************************************************
# MANIPULACIÓN DE DATOS ----
# ******************************************************************************

## +++++++++++++++++++++
## MANEJO DE FECHAS ----
## +++++++++++++++++++++

df$Fecha <- as.Date(df$Fecha, format = "%Y-%m-%d")

#___________________________________ GRÁFICOS ___________________________________ ----

# ******************************************************************************
# GRÁFICO DE UNA SERIE DE TIEMPO ----
# ******************************************************************************

## +++++++++
## TEMA ----
## +++++++++

tema <- theme(text=element_text(family="Montserrat"),
              plot.title = element_text(size=15,
                                        hjust = 0.5),
              plot.subtitle = element_text(size=12,
                                           hjust = 0.5),
              plot.caption =element_text(size=10,
                                         hjust = 0),
              panel.background = element_blank(),
              axis.ticks = element_line(alpha("grey", 0.3)),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(alpha("grey", 0.3)),
              axis.text.x = element_text(angle=270),
              axis.text = element_text(size=10))

## ++++++++++++++++++++
## GUARDAR GRÁFICO ----
## ++++++++++++++++++++

# función para guardar gráficos
guardar_grafico <- function(grafico,
                            filename,
                            width = 12,
                            height = 7,
                            units = "in",
                            res = 300) {
    
    png(filename = filename, 
        width = width, 
        height = height, 
        units = units, 
        res = res)
    
    print(grafico)
    
    dev.off()
}

## +++++++++++++++++++++++++++++++
## GRÁFICO TEMPERATURA MÍNIMA ----
## +++++++++++++++++++++++++++++++

promedio_Tmin <- mean(df$Tmin, na.rm = TRUE)

tmin <- df %>% 
    ggplot(aes(x = Fecha, y = Tmin)) +
    geom_line(linewidth = 1.1, color = "darkred") +
    geom_area(alpha = 0.3, fill = "#56B1F7") +
    annotate("segment", 
             x = min(df$Fecha), 
             xend = max(df$Fecha), 
             y = promedio_Tmin, 
             yend = promedio_Tmin, 
             color = "blue", 
             linetype = "dashed", 
             size = 1) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(min(df$Fecha), max(df$Fecha))) +
    scale_y_continuous(labels = scales::comma, breaks = seq(0, 30, 4)) +
    labs(title = "Temperatura mínima mensual \nde 1981-2016*",
         subtitle = "Estación Laredo",
         caption = "Elaboración propia con datos de PISCO (Peruvian Interpolated Data of the SENAMHI’s Climatological and Hydrological Observations). \n*Los valores se obtuvieron en la coordenada de la Estación. Resolución espacial: 5-km grid (1/2-deg)",
         x = "",
         y = "°C") +
    tema +
    annotate("text", x = as.Date("1996-01-01"), y = promedio_Tmin + 1,
             label = paste("Promedio anual:", round(promedio_Tmin,2), "°C"), color = "blue",
             size = 5, hjust = 0)
# \n*Los valores se obtuvieron en la coordenada de la Estación. Resolución espacial: 4-km grid (1/24-deg)"
### .....................
### EXPORTAR GRÁFICO ----
### .....................

# Usar la función para guardar el gráfico
guardar_grafico(grafico = tmin, 
                filename = "OUT/Gráficos/Tmin_1980_2023.png")

## +++++++++++++++++++++++++++++++
## GRÁFICO TEMPERATURA MÁXIMA ----
## +++++++++++++++++++++++++++++++

promedio_Tmax <- mean(df$Tmax, na.rm = TRUE)

tmax <- df %>% 
    ggplot(aes(x = Fecha, y = Tmax)) + 
    geom_line(linewidth = 1.1, color = "darkred") +
    geom_area(alpha = 0.3, fill = "#D7191C") +
    annotate("segment", 
             x = min(df$Fecha), 
             xend = max(df$Fecha), 
             y = promedio_Tmax, 
             yend = promedio_Tmax, 
             color = "blue", 
             linetype = "dashed", 
             size = 1) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(min(df$Fecha), max(df$Fecha))) +
    scale_y_continuous(labels = scales::comma, breaks = seq(0, 30, 4)) +
    labs(title = "Temperatura máxima mensual \nde 1981-2016*",
         subtitle = "Estación Laredo",
         caption = "Elaboración propia con datos de PISCO (Peruvian Interpolated Data of the SENAMHI’s Climatological and Hydrological Observations). \n*Los valores se obtuvieron en la coordenada de la Estación. Resolución espacial: 5-km grid (1/2-deg)",
         x = "",
         y = "°C") +
    tema +
    annotate("text", x = as.Date("1996-01-01"), y = promedio_Tmax + 1,
             label = paste("Promedio anual:", round(promedio_Tmax, 2), "°C"), color = "blue",
             size = 5, hjust = 0)

### .....................
### EXPORTAR GRÁFICO ----
### .....................

# Usar la función para guardar el gráfico
guardar_grafico(grafico = tmax, 
                filename = "OUT/Gráficos/Tmax_1980_2023.png")

## +++++++++++++++++++++++++++++++++
## GRÁFICO TEMPERATURA PROMEDIO ----
## +++++++++++++++++++++++++++++++++

promedio_Tprom <- mean(df$Tmed, na.rm = TRUE)

tprom <- df %>% 
    ggplot(aes(x = Fecha, y = Tmed)) +  # Cambiar y = Tmin por y = Tmed
    geom_line(linewidth = 1.1, color = "darkred") +
    geom_area(alpha = 0.3, fill = "#FDB863") +
    annotate("segment", 
             x = min(df$Fecha), 
             xend = max(df$Fecha), 
             y = promedio_Tprom, 
             yend = promedio_Tprom, 
             color = "blue", 
             linetype = "dashed", 
             size = 1.2) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(min(df$Fecha), max(df$Fecha))) +
    scale_y_continuous(labels = scales::comma, breaks = seq(0, 30, 4)) +
    labs(title = "Temperatura promedio mensual \nde 1981-2016*",
         subtitle = "Estación Laredo",
         caption = "Elaboración propia con datos de PISCO (Peruvian Interpolated Data of the SENAMHI’s Climatological and Hydrological Observations). \n*Los valores se obtuvieron en la coordenada de la Estación. Resolución espacial: 5-km grid (1/2-deg)",
         x = "",
         y = "°C") +
    tema +
    annotate("text", x = as.Date("1996-01-01"), y = promedio_Tprom + 1,
             label = paste("Promedio anual:", round(promedio_Tprom, 2), "°C"), color = "blue",
             size = 5, hjust = 0)

### .....................
### EXPORTAR GRÁFICO ----
### .....................

# Usar la función para guardar el gráfico
guardar_grafico(grafico = tprom, 
                filename = "OUT/Gráficos/Tprom_1980_2023.png")

## ++++++++++++++++
## GRÁFICO ETP ----
## ++++++++++++++++

promedio_ETP <- mean(df$ETP, na.rm = TRUE)

etp <- df %>% 
    ggplot(aes(x = Fecha, y = ETP)) +  
    geom_line(linewidth = 1.1, color = "darkred") +
    geom_area(alpha = 0.3, fill = "#66c2a5") +
    annotate("segment", 
             x = min(df$Fecha), 
             xend = max(df$Fecha), 
             y = promedio_ETP, 
             yend = promedio_ETP, 
             color = "blue", 
             linetype = "dashed", 
             size = 1.2) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(min(df$Fecha), max(df$Fecha))) +
    scale_y_continuous(labels = scales::comma, breaks = seq(0, 160, 20)) +
    labs(title = "Evapotranspiración mensual \nde 1981-2016*",
         subtitle = "Estación Laredo",
         caption = "Elaboración propia con datos de PISCO (Peruvian Interpolated Data of the SENAMHI’s Climatological and Hydrological Observations). \n*Los valores se obtuvieron en la coordenada de la Estación. Resolución espacial: 5-km grid (1/2-deg)",
         x = "",
         y = "mm/dia") +
    tema +
    annotate("text", x = as.Date("1996-01-01"), y = promedio_ETP + 10,
             label = paste("Promedio anual:", round(promedio_ETP, 2), "mm/día"), color = "blue",
             size = 5, hjust = 0)

### .....................
### EXPORTAR GRÁFICO ----
### .....................

# Usar la función para guardar el gráfico
guardar_grafico(grafico = etp, 
                filename = "OUT/Gráficos/ETP_1980_2023.png")


## ++++++++++++++++
## EVAPORACIÓN ----
## ++++++++++++++++

promedio_Evap <- mean(df$Evaporacion, na.rm = TRUE)

evapo <- df %>% 
    ggplot(aes(x = Fecha, y = Evaporacion)) +  # Cambiar y = ETP por y = Evaporacion
    geom_line(linewidth = 1.1, color = "darkred") +
    geom_area(alpha = 0.3, fill = "#76c7c0") +
    annotate("segment", 
             x = min(df$Fecha), 
             xend = max(df$Fecha), 
             y = promedio_Evap, 
             yend = promedio_Evap, 
             color = "blue", 
             linetype = "dashed", 
             size = 1.2) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(min(df$Fecha), max(df$Fecha))) +
    scale_y_continuous(labels = scales::comma, breaks = seq(0, 160, 20)) +
    labs(title = "Evaporación mensual \nde 1981-2016*",
         subtitle = "Estación Laredo",
         caption = "Elaboración propia con datos de PISCO (Peruvian Interpolated Data of the SENAMHI’s Climatological and Hydrological Observations). \n*Los valores se obtuvieron en la coordenada de la Estación. Resolución espacial: 5-km grid (1/2-deg)",
         x = "",
         y = "mm/dia") +
    tema +
    annotate("text", x = as.Date("1996-01-01"), y = promedio_Evap + 10,
             label = paste("Promedio anual:", round(promedio_Evap, 2), "mm"), color = "blue",
             size = 5, hjust = 0)
### .....................
### EXPORTAR GRÁFICO ----
### .....................

# Usar la función para guardar el gráfico
guardar_grafico(grafico = evapo, 
                filename = "OUT/Gráficos/Evaporacion_1980_2023.png")


## +++++++++++++++++++++++++
## VELOCIDAD DEL VIENTO ----
## +++++++++++++++++++++++++

promedio_Wind <- mean(df$WindSpeed, na.rm = TRUE)

wind <- df %>% 
    ggplot(aes(x = Fecha, y = WindSpeed)) + 
    geom_line(linewidth = 1.1, color = "darkgreen") +  
    geom_area(alpha = 0.3, fill = "#87CEEB") +  
    annotate("segment", 
             x = min(df$Fecha), 
             xend = max(df$Fecha), 
             y = promedio_Wind, 
             yend = promedio_Wind, 
             color = "blue", 
             linetype = "dashed", 
             size = 1.2) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(min(df$Fecha), max(df$Fecha))) +
    scale_y_continuous(labels = scales::comma, breaks = seq(0, 10, 1)) +  # Ajustar los breaks según los valores de WindSpeed
    labs(title = "Velocidad del viento mensual \nde 1981-2016*",
         subtitle = "Estación Laredo",
         caption = "Elaboración propia con datos de TerraClimate (University of California Merced, UCM). \n*Los valores se obtuvieron en la coordenada de la Estación. Resolución espacial: 4-km grid (1/24-deg)",
         x = "",
         y = "m/s") +  # Cambiar la unidad de medida en el eje Y
    tema +
    annotate("text", x = as.Date("1996-01-01"), y = promedio_Wind + 0.5,
             label = paste("Promedio anual:", round(promedio_Wind, 2), "m/s"), color = "blue",
             size = 5, hjust = 0)

### .....................
### EXPORTAR GRÁFICO ----
### .....................

# Usar la función para guardar el gráfico
guardar_grafico(grafico = wind, 
                filename = "OUT/Gráficos/VelocidaViento_1980_2023.png")


## ++++++++++++++++++++
## RADIACIÓN SOLAR ----
## ++++++++++++++++++++

promedio_Radiation <- mean(df$DSRatiation, na.rm = TRUE)

radiation <- df %>% 
    ggplot(aes(x = Fecha, y = DSRatiation)) +  # Cambiar y = WindSpeed por y = DSRatiation
    geom_line(linewidth = 1.1, color = "darkorange") +  # Color más representativo para la radiación solar
    geom_area(alpha = 0.3, fill = "#FFD700") +  # Color amarillo dorado para el área de radiación solar
    annotate("segment", 
             x = min(df$Fecha), 
             xend = max(df$Fecha), 
             y = promedio_Radiation, 
             yend = promedio_Radiation, 
             color = "blue", 
             linetype = "dashed", 
             size = 1.2) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(min(df$Fecha), max(df$Fecha))) +
    scale_y_continuous(labels = scales::comma, breaks = seq(0, 400, 50)) +  # Ajustar los breaks según los valores de DSRatiation
    labs(title = "Radiación solar de onda corta mensual \nde 1981-2016*",
         subtitle = "Estación Laredo",
         caption = "Elaboración propia con datos de TerraClimate (University of California Merced, UCM). \n*Los valores se obtuvieron en la coordenada de la Estación. Resolución espacial: 4-km grid (1/24-deg)",
         x = "",
         y = "W/m²") +  # Cambiar la unidad de medida en el eje Y
    tema +
    annotate("text", x = as.Date("1996-01-01"), y = promedio_Radiation + 20,
             label = paste("Promedio anual:", round(promedio_Radiation, 2), "W/m²"), color = "blue",
             size = 5, hjust = 0)


### .....................
### EXPORTAR GRÁFICO ----
### .....................

# Usar la función para guardar el gráfico
guardar_grafico(grafico = radiation, 
                filename = "OUT/Gráficos/Radiation_1980_2023.png")

## ++++++++++++++++++++
## HUMEDAD RELATIVA ----
## ++++++++++++++++++++

promedio_HR <- mean(df$HR, na.rm = TRUE)

humidity <- df %>% 
    ggplot(aes(x = Fecha, y = HR)) +  # Cambiar y = DSRatiation por y = HR
    geom_line(linewidth = 1.1, color = "darkblue") +  # Color más representativo para la humedad
    geom_area(alpha = 0.3, fill = "#87CEFA") +  # Color azul claro para el área de humedad relativa
    annotate("segment", 
             x = min(df$Fecha), 
             xend = max(df$Fecha), 
             y = promedio_HR, 
             yend = promedio_HR, 
             color = "darkred", 
             linetype = "dashed", 
             size = 1.2) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(min(df$Fecha), max(df$Fecha))) +
    scale_y_continuous(labels = scales::number, breaks = seq(0, 100, 5)) +  # Ajustar los breaks según los valores de HR y formato en porcentaje
    labs(title = "Humedad relativa mensual \nde 1981-2016*",
         subtitle = "Estación Laredo",
         caption = "Elaboración propia calculados con datos de TerraClimate y Pisco. \n*Los valores se calcularon para la coordenada de la Estación.",
         x = "",
         y = "Porcentaje (%)") +  # Cambiar la unidad de medida en el eje Y
    tema +
    annotate("text", x = as.Date("1996-01-01"), y = promedio_HR + 5,
             label = paste("Promedio anual:", round(promedio_HR, 2), "%"), color = "darkred",
             size = 5, hjust = 0)

### .....................
### EXPORTAR GRÁFICO ----
### .....................

# Usar la función para guardar el gráfico
guardar_grafico(grafico = humidity, 
                filename = "OUT/Gráficos/HR_1980_2023.png")

#_________________________________ ESTADÍSTICOS _________________________________ ----

# ******************************************************************************
# CALCULO DE ESTADÍSTICOS ----
# ******************************************************************************

# Función de curtosis
kurtosis <- function(x) {
    n <- length(x)
    mean_x <- mean(x)
    sd_x <- sd(x)
    z <- (x - mean_x) / sd_x
    mean(z^4) - 3 # Curtosis estandarizada
}

# Función de resumen estadístico
Resumen_estadistico <- function(x) {
    Media <- round(mean(x, na.rm = TRUE), 2)
    Mediana <- round(median(x, na.rm = TRUE), 2)
    Moda <- round(modeest::mfv1(x), 2)
    Varianza <- round(var(x, na.rm = TRUE), 2)
    `Desviación estándar` <- round(sd(x, na.rm = TRUE), 2)
    Minimo <- round(min(x, na.rm = TRUE), 2)
    Maximo <- round(max(x, na.rm = TRUE), 2)
    Rango <- round(Maximo - Minimo, 2)
    `1er cuartil` <- round(quantile(x, 0.25, na.rm = TRUE), 2)
    `3er cuartil` <- round(quantile(x, 0.75, na.rm = TRUE), 2)
    `Rango intercuartil` <- round(`3er cuartil` - `1er cuartil`, 2)
    `Datos atípicos positivos` <- round(`3er cuartil` + 1.5 * `Rango intercuartil`, 2)
    `Datos atípicos negativos` <- round(`1er cuartil` - 1.5 * `Rango intercuartil`, 2)
    `Datos atípicos` <- sum(x < `Datos atípicos negativos` | x > `Datos atípicos positivos`, na.rm = TRUE)
    Asimetría <- round(skewness(x, na.rm = TRUE), 2)
    Curtosis <- round(kurtosis(x), 2)
    
    Datos <- c(Media, Mediana, Moda, Varianza, `Desviación estándar`,
               Minimo, Maximo, Rango, `1er cuartil`, `3er cuartil`,
               `Rango intercuartil`, `Datos atípicos`, Asimetría, Curtosis)
    
    return(Datos)
}

# Estadísticos
Estadisticos <- c('Media', 'Mediana', 'Moda', 'Varianza',
                  'Desviación estándar', 'Minimo',
                  'Maximo', 'Rango', '1er cuartil', '3er cuartil',
                  'Rango intercuartil', 'Datos atípicos',
                  'Asimetría', 'Curtosis')

lista_estadisticos <- lapply(df[,-1], Resumen_estadistico)

Tabla_RE <- data.frame(Estadisticos, do.call(cbind, lista_estadisticos))

colnames(Tabla_RE)[-1] <- colnames(df)[-1]

Tabla_RE