# ==============================================================================.
# TEMA      : Extrae datos de PISCO (Local)
# AUTOR     : Henry P. Zumaeta Lozano
# CORREO    : henry.zumaeta.l@uni.pe
# VERSION   : 1.0
# CREADO    : 01/04/2024
# VALIDADO  : 24/06/2024
# ==============================================================================.

# Descripción: Extrae precipitación y temperatura diaria y mensual de PISCO
# desde archivo .nc local.

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

p_load(raster, sp, dplyr, openxlsx)

## +++++++++++++++++++++
## FIJAR DIRECTORIO ----
## +++++++++++++++++++++

# setwd(r"(D:\ZEngi\Hydro\Projects\Shambo\R_PY\Pisco)")
# getwd()

## +++++++++++++++++++++
## CARGA DE LA DATA ----
## +++++++++++++++++++++

path <- "IN/Coordenadas/long_lat_SHAMBOYACU.csv"
long_lat <- read.csv(path, header = T)

# Mensual
raster_pp   <- brick("D:/ZEngi/Data/Pisco/Precipitacion/Mensual/Prec_m.nc")
raster_tmax <- brick("D:/ZEngi/Data/Pisco/Temperatura/Mensual/Tmax_m.nc")
raster_tmin <- brick("D:/ZEngi/Data/Pisco/Temperatura/Mensual/Tmin_m.nc")

# Diaria
raster_pp_d   <- brick("D:/ZEngi/Data/Pisco/Precipitacion/Diaria/Prec_d.nc")
raster_tmax_d <- brick("D:/ZEngi/Data/Pisco/Temperatura/Diaria/Tmax_d.nc")
raster_tmin_d <- brick("D:/ZEngi/Data/Pisco/Temperatura/Diaria/Tmin_d.nc")


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
extracted_name <- sub(".*lat_(.*)\\.csv", "\\1", path)

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
