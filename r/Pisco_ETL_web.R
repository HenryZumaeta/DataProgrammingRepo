# ==============================================================================.
# TEMA      : Extrae datos de PISCO (Web)
# AUTOR     : Henry P. Zumaeta Lozano
# CORREO    : henry.zumaeta.l@uni.pe
# VERSION   : 2.0
# CREADO    : 24/04/2024
# VALIDADO  : 24/06/2024
# ==============================================================================.

# Descripción: Este script extrae datos de precipitación y temperatura diarios 
# y mensuales del conjunto de datos PISCO desde archivos netCDF (.nc) 
# descargados de la web.

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

p_load(raster, sp, dplyr, openxlsx, googledrive)

## +++++++++++++++++++++
## FIJAR DIRECTORIO ----
## +++++++++++++++++++++

# setwd(r"(D:\ZEngi\Hydro\Projects\Shambo\R_PY\Pisco)")
# getwd()

## +++++++++++++++++++++++++++++++++++++++++++++
## FUNCION PARA DESCARGAR ARCHIVOS DE DRIVE ----
## +++++++++++++++++++++++++++++++++++++++++++++

# Verificar si ya está autenticado
if (!drive_has_token()) {
    # Autenticar tu cuenta de Google
    drive_auth()
}

# Descargar archivos desde drive
descargar_archivo_desde_drive <- function(enlace_original, nombre_archivo='archivo.nc') {
    # Convierte un enlace de Google Drive a un enlace de descarga directa
    convertir_enlace_gdrive <- function(enlace_original) {
        file_id <- sub(".*d/(.*)/view.*", "\\1", enlace_original)
        return(paste0('https://drive.google.com/uc?id=', file_id))
    }
    
    url <- convertir_enlace_gdrive(enlace_original)
    
    # Descargar del archivo
    tryCatch({
        drive_download(as_id(sub(".*id=(.*)", "\\1", url)), path = nombre_archivo, overwrite = TRUE)
        if (!file.exists(nombre_archivo)) {
            stop("No se pudo descargar el archivo. Puede que no sea accesible.")
        }
    }, error = function(e) {
        print(paste("Error:", e))
        print("Por favor, verifica los permisos del archivo o usa otro método de compartición.")
        return(NULL)
    })
    
    # Leer el archivo descargado
    tryCatch({
        raster_data <- brick(nombre_archivo)
        return(raster_data)
    }, error = function(e) {
        print(paste("Error al leer el archivo:", e))
        return(NULL)
    })
}


## +++++++++++++++++++++
## CARGA DE LA DATA ----
## +++++++++++++++++++++

### ................
### COORDENADAS ----
### ................

long_lat <- data.frame(
    DESC = c("PICOTA", "SHAMBOYACU", "TINGO"),
    LON = c(-76.330214, -76.129111, -76.252667),
    LAT = c(-6.919892, -7.0223, -6.937222)
    )

### ........................
### DATA GRILLADA PISCO ----
### ........................

#### MENSUAL ----
#### ****++++++++

# Descarga precipitación promedio mensual
URL1 <- 'https://drive.google.com/file/d/13eD6cqqIdu4mu7JF3ckoMde9VxX4-GgW/view?usp=sharing'
nombre_extension1 <- 'Prec_m.nc' # Colocar su extensión del archivo

raster_pp <- descargar_archivo_desde_drive(URL1, nombre_extension1)

# Descarga temperatura máxima mensual
URL2 <- 'https://drive.google.com/file/d/1BVb2NStH4jUdHlFVh_vomo3V7v3ork0A/view?usp=sharing'
nombre_extension2 <- 'Tmax_m.nc' # Colocar su extensión del archivo

raster_tmax <- descargar_archivo_desde_drive(URL2, nombre_extension2)

# Descarga temperatura mínima mensual
URL3 <- 'https://drive.google.com/file/d/1-NarOdp0NQNQaVkyi0eemcy_9JrkJ2C_/view?usp=sharing'
nombre_extension3 <- 'Tmin_m.nc' # Colocar su extensión del archivo

raster_tmin <- descargar_archivo_desde_drive(URL3, nombre_extension3)


#### DIARIA ----
#### ****+++++++

# Descarga precipitación promedio diaria
URL4 <- 'https://drive.google.com/file/d/1CF2Tub6DpkEJ6vkWU8VfNK1pKtg56Wmo/view?usp=sharing'
nombre_extension4 <- 'Prec_d.nc' # Colocar su extensión del archivo

raster_pp_d <- descargar_archivo_desde_drive(URL4, nombre_extension4)

# Descarga temperatura máxima diaria
URL5 <- 'https://drive.google.com/file/d/1lFELgJshGDbDy7OzKx2ooHoDxTPBtqdf/view?usp=sharing'
nombre_extension5 <- 'Tmax_d.nc' # Colocar su extensión del archivo

raster_tmax_d <- descargar_archivo_desde_drive(URL5, nombre_extension5)

# Descarga temperatura mínima diaria
URL6 <- 'https://drive.google.com/file/d/1QTC_TE8rCgpbqeLIQMGWvxwfOUn7dady/view?usp=sharing'
nombre_extension6 <- 'Tmin_d.nc' # Colocar su extensión del archivo

raster_tmin_d <- descargar_archivo_desde_drive(URL6, nombre_extension6)



#____________________________ EXTRACCIÓN DE LA DATA ____________________________ ----

# ******************************************************************************
# EXTRACCIÓN DE DATOS PISCO POR PUNTOS ----
# ******************************************************************************

## ++++++++++++++++++++++++++++++
## ASIGNACIÓN DE COORDENADAS ----
## ++++++++++++++++++++++++++++++

coordinates(long_lat) <- ~LON+LAT

projection(long_lat) <- projection(raster_pp)

## +++++++++++++++++++++++++++++++++++
## EXTRACCIÓN DE LA PRECIPITACIÓN ----
## +++++++++++++++++++++++++++++++++++

# Mensual
points_long_lat_pp <- extract(raster_pp[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_pp <- t(raster_pp[points_long_lat_pp])
colnames(data_long_lat_pp) <- as.character(long_lat$DESC)

# Diaria
points_long_lat_pp_d <- extract(raster_pp_d[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_pp_d <- t(raster_pp_d[points_long_lat_pp_d])
colnames(data_long_lat_pp_d) <- as.character(long_lat$DESC)

## ++++++++++++++++++++++++++++++++++++++++
## EXTRACCIÓN DE LA TEMPERATURA MÁXIMA ----
## ++++++++++++++++++++++++++++++++++++++++

# Mensual
points_long_lat_tmax <- extract(raster_tmax[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_tmax <- t(raster_tmax[points_long_lat_tmax])
colnames(data_long_lat_tmax) <- as.character(long_lat$DESC)

# Diaria
points_long_lat_tmax_d <- extract(raster_tmax_d[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_tmax_d <- t(raster_tmax_d[points_long_lat_tmax_d])
colnames(data_long_lat_tmax_d) <- as.character(long_lat$DESC)


## ++++++++++++++++++++++++++++++++++++++++
## EXTRACCIÓN DE LA TEMPERATURA MÍNIMA ----
## ++++++++++++++++++++++++++++++++++++++++

# Mensual
points_long_lat_tmin <- extract(raster_tmin[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_tmin <- t(raster_tmin[points_long_lat_tmin])
colnames(data_long_lat_tmin) <- as.character(long_lat$DESC)

# Diaria
points_long_lat_tmin_d <- extract(raster_tmin_d[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_tmin_d <- t(raster_tmin_d[points_long_lat_tmin_d])
colnames(data_long_lat_tmin_d) <- as.character(long_lat$DESC)


## ++++++++++++++++++++++
## TEMPERATURA MEDIA ----
## ++++++++++++++++++++++

# Mensual
tmedia <- (data_long_lat_tmax+data_long_lat_tmin)/2

# Diaria
tmedia_d <- (data_long_lat_tmax_d+data_long_lat_tmin_d)/2

#_____________________________ LIMPIEZA DE LA DATA _____________________________ ----

# ******************************************************************************
# MODIFICANDO LA DATA ----
# ******************************************************************************

# Extraer nombre data
extracted_name <- long_lat$DESC[1]

## ++++++++++++++++++
## PRECIPITACIÓN ----
## ++++++++++++++++++

# Mensual
Pmen_pisco <- data_long_lat_pp %>%
    as.data.frame() %>% 
    `rownames<-` (seq_len(nrow(data_long_lat_pp))) %>% 
    tibble() %>%
    mutate(
        Fecha = seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "month")
    )

# Diaria
Pdia_pisco <- data_long_lat_pp_d %>%
    as.data.frame() %>% 
    `rownames<-` (seq_len(nrow(data_long_lat_pp_d))) %>% 
    tibble() %>%
    mutate(
        Fecha = seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "day")
    )


## +++++++++++++++++++++++
## TEMPERATURA MÁXIMA ----
## +++++++++++++++++++++++

# Mensual
Tmax_pisco <- data_long_lat_tmax %>%
    as.data.frame() %>% 
    `rownames<-` (seq_len(nrow(data_long_lat_tmax))) %>% 
    tibble() %>%
    mutate(
        Fecha = seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "month")
    )

# Diaria
Tmax_pisco_d <- data_long_lat_tmax_d %>%
    as.data.frame() %>% 
    `rownames<-` (seq_len(nrow(data_long_lat_tmax_d))) %>% 
    tibble() %>%
    mutate(
        Fecha = seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "day")
    )


## +++++++++++++++++++++++
## TEMPERATURA MÍNIMA ----
## +++++++++++++++++++++++

# Mensual
Tmin_pisco <- data_long_lat_tmin %>%
    as.data.frame() %>% 
    `rownames<-` (seq_len(nrow(data_long_lat_tmin))) %>% 
    tibble() %>%
    mutate(
        Fecha = seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "month")
    )

# Diaria
Tmin_pisco_d <- data_long_lat_tmin_d %>%
    as.data.frame() %>% 
    `rownames<-` (seq_len(nrow(data_long_lat_tmin_d))) %>% 
    tibble() %>%
    mutate(
        Fecha = seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "day")
    )


## ++++++++++++++++++++++
## TEMPERATURA MEDIA ----
## ++++++++++++++++++++++

# Mensual
Tmed_pisco <- tmedia %>%
    as.data.frame() %>% 
    `rownames<-` (seq_len(nrow(tmedia))) %>% 
    tibble() %>%
    mutate(
        Fecha = seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "month")
    )

# Diaria
Tmed_pisco_d <- tmedia_d %>%
    as.data.frame() %>% 
    `rownames<-` (seq_len(nrow(tmedia_d))) %>% 
    tibble() %>%
    mutate(
        Fecha = seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "day")
    )

#_________________________________ EXPORTACIÓN _________________________________ ----

# ******************************************************************************
# EXPORTACIÓN DE DATA PROCESADA ----
# ******************************************************************************

## ++++++++++++++++++
## PRECIPITACIÓN ----
## ++++++++++++++++++

# Mensual

# Ordenando las columnas
Pmen_pisco <- Pmen_pisco[c("Fecha", setdiff(names(Pmen_pisco), "Fecha"))]

# csv
write.csv(Pmen_pisco, paste0("OUT/pmen_salida_",extracted_name,"_csv.csv"),
          quote = F, 
          row.names = FALSE)
# Excel
write.xlsx(Pmen_pisco, paste0("OUT/pmen_salida_",extracted_name,"_xlsx.xlsx"),
           quote = F, 
           rowNames = FALSE)


# Diario

# Ordenando las columnas
Pdia_pisco <- Pdia_pisco[c("Fecha", setdiff(names(Pdia_pisco), "Fecha"))]

# csv
write.csv(Pdia_pisco, paste0("OUT/Pdia_salida_",extracted_name,"_csv.csv"),
          quote = F, 
          row.names = FALSE)
# Excel
write.xlsx(Pdia_pisco, paste0("OUT/Pdia_salida_",extracted_name,"_xlsx.xlsx"),
           quote = F, 
           rowNames = FALSE)


## +++++++++++++++++++++++
## TEMPERATURA MÁXIMA ----
## +++++++++++++++++++++++

# Mensual
# Ordenando las columnas
Tmax_pisco <- Tmax_pisco[c("Fecha", setdiff(names(Tmax_pisco), "Fecha"))]

write.csv(Tmax_pisco, paste0("OUT/tmax_salida_m_",extracted_name,"_csv.csv"),
          quote = F, 
          row.names = FALSE)

# Diaria
Tmax_pisco_d <- Tmax_pisco_d[c("Fecha", setdiff(names(Tmax_pisco_d), "Fecha"))]

write.csv(Tmax_pisco_d, paste0("OUT/tmax_salida_d_",extracted_name,"_csv.csv"),
          quote = F, 
          row.names = FALSE)


## +++++++++++++++++++++++
## TEMPERATURA MÍNIMA ----
## +++++++++++++++++++++++

# Mensual
# Ordenando las columnas
Tmin_pisco <- Tmin_pisco[c("Fecha", setdiff(names(Tmin_pisco), "Fecha"))]

write.csv(Tmin_pisco, paste0("OUT/tmin_salida_m_ ",extracted_name,"_csv.csv"),
          quote = F, 
          row.names = FALSE)


# Diaria
# Ordenando las columnas
Tmin_pisco_d  <- Tmin_pisco_d [c("Fecha", setdiff(names(Tmin_pisco_d ), "Fecha"))]

write.csv(Tmin_pisco_d , paste0("OUT/tmin_salida_d_",extracted_name,"_csv.csv"),
          quote = F, 
          row.names = FALSE)


## ++++++++++++++++++++++
## TEMPERATURA MEDIA ----
## ++++++++++++++++++++++

# Mensual
# Ordenando las columnas
Tmed_pisco <- Tmed_pisco[c("Fecha", setdiff(names(Tmed_pisco), "Fecha"))]

write.csv(Tmed_pisco, paste0("OUT/tmed_salida_m_",extracted_name,"_csv.csv"),
          quote = F, 
          row.names = FALSE)


# Diaria
Tmed_pisco_d <- Tmed_pisco_d[c("Fecha", setdiff(names(Tmed_pisco_d), "Fecha"))]

write.csv(Tmed_pisco_d, paste0("OUT/tmed_salida_d_",extracted_name,"_csv.csv"),
          quote = F, 
          row.names = FALSE)
