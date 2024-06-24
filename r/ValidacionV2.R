# ==============================================================================..
# TEMA      : FUNCIONES DE VALIDACIÓN DE MODELOS DE SIMULACIÓN
# AUTOR     : Henry P. Zumaeta Lozano
# CORREO    : henry.zumaeta.l@uni.pe
# VERSION   : 2.0
# CREADO    : 24/06/2024
# VALIDADO  : 24/06/2024
# ==============================================================================..

# ******************************************************************************
# FUNCIONES DE VALIDACION DE SIMULACIONES ----
# ******************************************************************************

# ==============================================================================..
# Funciones de Errores Absolutos ----
# ==============================================================================..

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Error Absoluto Medio (MAE) ----
## Descripción: Calcula el error absoluto medio entre las predicciones y las
## observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValMAE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    mae <- matrix(nrow = numreal, ncol = numsim, 
                  dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    pmae <- matrix(nrow = numreal, ncol = numsim, 
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            mae[j, i] <- mean(abs(xx[valid_indices] - yy[valid_indices]))
            pmae[j, i] <- mae[j, i] / mean(yy[valid_indices]) * 100
        }
    }
    
    return(list(mae = mae, pmae = pmae))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Error Absoluto Mediano (MedAE) ----
## Descripción: Calcula el error absoluto mediano entre las predicciones y las
## observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValMedAE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    medae <- matrix(nrow = numreal, ncol = numsim, 
                    dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            medae[j, i] <- median(abs(xx[valid_indices] - yy[valid_indices]))
        }
    }
    
    return(list(medae = medae))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Error Absoluto Medio Relativo (RMAE) ----
## Descripción: Calcula el error absoluto medio relativo entre las predicciones
## y las observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValRMAE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    rmae <- matrix(nrow = numreal, ncol = numsim, 
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            rmae[j, i] <- mean(abs(xx[valid_indices] - yy[valid_indices])) / mean(yy[valid_indices])
        }
    }
    
    return(list(rmae = rmae))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Error Absoluto Porcentual Medio (MAPE) ----
## Descripción: Calcula el error absoluto porcentual medio entre las
## predicciones y las observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValMAPE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    mape <- matrix(nrow = numreal, ncol = numsim, 
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(yy != 0 & !is.na(xx) & !is.na(yy))
            mape[j, i] <- mean(abs((yy[valid_indices] - xx[valid_indices]) / yy[valid_indices])) * 100
        }
    }
    
    return(list(mape = mape))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Error Absoluto Porcentual Medio Simétrico (sMAPE) ----
## Descripción: Calcula el error absoluto porcentual medio simétrico entre
## las predicciones y las observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValSMAPE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    smape <- matrix(nrow = numreal, ncol = numsim, 
                    dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            smape[j, i] <- mean(2 * abs(xx[valid_indices] - yy[valid_indices]) / (abs(xx[valid_indices]) + abs(yy[valid_indices]))) * 100
        }
    }
    
    return(list(smape = smape))
}

# ==============================================================================.
# Funciones de Errores Cuadráticos ----
# ==============================================================================.

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Error Cuadrático Medio (MSE) ----
## Descripción: Calcula el error cuadrático medio entre las predicciones y las
## observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValMSE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    mse <- matrix(nrow = numreal, ncol = numsim, 
                  dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    rmse <- matrix(nrow = numreal, ncol = numsim, 
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    prmse <- matrix(nrow = numreal, ncol = numsim, 
                    dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            mse[j, i] <- mean((xx[valid_indices] - yy[valid_indices])^2)
            rmse[j, i] <- sqrt(mse[j, i])
            prmse[j, i] <- rmse[j, i] / mean(yy[valid_indices]) * 100
        }
    }
    
    return(list(mse = mse, rmse = rmse, prmse = prmse))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Error Cuadrático Medio Relativo (RRMSE) ----
## Descripción: Calcula el error cuadrático medio relativo entre las
## predicciones y las observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValRRMSE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    rrmse <- matrix(nrow = numreal, ncol = numsim, 
                    dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            rrmse[j, i] <- sqrt(mean((xx[valid_indices] - yy[valid_indices])^2)) / mean(yy[valid_indices])
        }
    }
    
    return(list(rrmse = rrmse))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Error Cuadrático Medio Logarítmico (MSLE) ----
## Descripción: Calcula el error cuadrático medio logarítmico entre las
## predicciones y las observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValMSLE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    msle <- matrix(nrow = numreal, ncol = numsim, 
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(xx > 0 & yy > 0 & !is.na(xx) & !is.na(yy))
            msle[j, i] <- mean((log1p(xx[valid_indices]) - log1p(yy[valid_indices]))^2)
        }
    }
    
    return(list(msle = msle))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Error Cuadrático Medio Logarítmico Relativo (RMSLE) ----
## Descripción: Calcula el error cuadrático medio logarítmico relativo entre
## las predicciones y las observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValRMSLE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    rmsle <- matrix(nrow = numreal, ncol = numsim, 
                    dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(xx >= 0 & yy >= 0 & !is.na(xx) & !is.na(yy))
            rmsle[j, i] <- sqrt(mean((log1p(xx[valid_indices]) - log1p(yy[valid_indices]))^2))
        }
    }
    
    return(list(rmsle = rmsle))
}

# ==============================================================================.
# Funciones de Errores Relativos y Porcentuales ----
# ==============================================================================.

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Sesgo Porcentual (PBIAS) ----
## Descripción: Calcula el sesgo porcentual entre las predicciones y las
## observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValPBIAS <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    pbias <- matrix(nrow = numreal, ncol = numsim, 
                    dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            pbias[j, i] <- 100 * (sum(xx[valid_indices] - yy[valid_indices]) / sum(yy[valid_indices]))
        }
    }
    
    return(list(pbias = pbias))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Error Cuadrático Porcentual Medio (RMSPE) ----
## Descripción: Calcula el error cuadrático porcentual medio entre las
## predicciones y las observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValRMSPE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    rmspe <- matrix(nrow = numreal, ncol = numsim, 
                    dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(yy != 0 & !is.na(xx) & !is.na(yy))
            perc_error <- ((xx[valid_indices] - yy[valid_indices]) / yy[valid_indices])^2
            rmspe[j, i] <- sqrt(mean(perc_error)) * 100
        }
    }
    
    return(list(rmspe = rmspe))
}

# ==============================================================================.
# Funciones de Concordancia y Correlación ----
# ==============================================================================.

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Índice de Concordancia (d) de Willmott ----
## Descripción: Calcula el índice de concordancia de Willmott entre las
## predicciones y las observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValWillmott <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    W <- matrix(nrow = numreal, ncol = numsim, 
                dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            numerador <- sum((xx[valid_indices] - yy[valid_indices])^2)
            denominador <- sum((abs(xx[valid_indices] - mean(yy[valid_indices])) + abs(yy[valid_indices] - mean(yy[valid_indices])))^2)
            W[j, i] <- 1 - numerador / denominador
        }
    }
    
    return(list(W = W))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Coeficiente de Correlación de Spearman (ro) ----
## Descripción: Calcula el coeficiente de correlación de Spearman entre las
## predicciones y las observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValCorrela <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    correla <- matrix(nrow = numreal, ncol = numsim, 
                      dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            correla[j, i] <- cor(xx, yy, method = "spearman", use = "complete.obs")
        }
    }
    
    return(list(Correlacion = correla))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Coeficiente de Determinación R cuadrado (R²) ----
## Descripción: Calcula el coeficiente de determinación R cuadrado entre las
## predicciones y las observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValR2 <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    rcuadrado <- matrix(nrow = numreal, ncol = numsim, 
                        dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            rcuadrado[j, i] <- cor(xx, yy, use = "complete.obs")^2
        }
    }
    
    return(list(rcuadrado = rcuadrado))
}

# ==============================================================================.
# Funciones de Modelos y Estimaciones ----
# ==============================================================================.

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Coeficiente de Información de Akaike (AIC) ----
## Descripción: Calcula el coeficiente de información de Akaike entre las
## predicciones y las observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValAIC <- function(x, y, k) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- ncol(x)
    n <- nrow(y)
    
    AIC <- numeric(m)
    lL <- numeric(m)
    sigmas2 <- numeric(m)
    rss <- numeric(m)
    modelo <- na.omit(cbind(y, x))
    n_modelo <- nrow(modelo)
    
    for (i in 1:m) {
        rss[i] <- sum((modelo[, 1] - modelo[, i + 1])^2)
        sigmas2[i] <- rss[i] / n_modelo
        lL[i] <- (-n_modelo / 2) * log(sigmas2[i])
    }
    
    AIC <- -2 * lL + 2 * k
    if (max(k) != n + 1) {
        AICc <- AIC + 2 * k * (k + 1) / (n_modelo - k - 1)
    } else {
        stop("No se puede calcular AICc, división entre cero")
    }
    
    return(list(AIC = AIC, AICc = AICc))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Inferencia Bayesiana ----
## Descripción: Calcula la inferencia bayesiana entre las predicciones y las
## observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValBayes <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- ncol(x)
    n <- nrow(y)
    
    apriori <- rep(1 / m, m)
    lL <- numeric(m)
    sigmas2 <- numeric(m)
    rss <- numeric(m)
    modelo <- na.omit(cbind(y, x))
    n_modelo <- nrow(modelo)
    
    for (i in 1:m) {
        rss[i] <- sum((modelo[, 1] - modelo[, i + 1])^2)
        sigmas2[i] <- rss[i] / n_modelo
        lL[i] <- (-n_modelo / 2) * log(sigmas2[i])
    }
    
    L <- exp(lL)
    numerador <- apriori * L
    denominador <- sum(numerador)
    posteriori <- numerador / denominador
    
    return(posteriori)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Análisis de Perfil (Profile Analysis) ----
## Descripción: Realiza un análisis de perfil entre las predicciones y las
## observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValProfile <- function(x, y, instantetiempo) {
    x <- as.data.frame(x)
    y <- na.omit(as.data.frame(y))
    ins <- instantetiempo
    temp3 <- ins - 1
    temp4 <- nrow(x)
    var <- ncol(y) / ins
    T2 <- numeric(temp4)
    valorp <- numeric(temp4)
    
    for (m in 1:temp4) {
        fila <- x[m, ]
        yyy <- matrix(nrow = nrow(y), ncol = temp3 * var)
        k <- 1
        for (i in 1:var) {
            temp1 <- (i - 1) * ins + 1
            temp2 <- ins * i
            yy <- y[temp1:temp2, ]
            xx <- fila[temp1:temp2]
            for (j in 1:temp3) {
                yyy[, k] <- yy[, j] - yy[, j + 1] - xx[j] + xx[j + 1]
                k <- k + 1
            }
        }
        res <- HotellingsT2(yyy, test = "chi")
        T2[m] <- res$statistic
        valorp[m] <- res$p.value
    }
    
    return(list(T2 = T2, valorp = valorp))
}

# ==============================================================================.
# Funciones de Pruebas Estadísticas ----
# ==============================================================================.

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Prueba F de Fisher ----
## Descripción: Realiza la prueba F de Fisher entre las predicciones y las
## observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValPruebaF <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    Famb <- matrix(nrow = numreal, ncol = numsim, 
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    ValP <- matrix(nrow = numreal, ncol = numsim, 
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            m1 <- lm(yy ~ xx)
            n <- length(xx)
            s2xy <- sum(resid(m1)^2) / (n - 2)
            a <- coef(m1)[1]
            b <- coef(m1)[2]
            est <- (n * a^2 + 2 * a * (b - 1) * sum(xx) + (b - 1)^2 * sum(xx^2)) / (2 * s2xy)
            Famb[j, i] <- est
            ValP[j, i] <- pf(est, 2, n - 2, lower.tail = FALSE)
        }
    }
    
    return(list(F = Famb, p = ValP))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Prueba t de Student ----
## Descripción: Realiza la prueba t de Student entre las predicciones y las
## observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValPruebat <- function(x, y, confidence = 0.95) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    Valt <- matrix(nrow = numreal, ncol = numsim, 
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    p <- matrix(nrow = numreal, ncol = numsim, 
                dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    confiinf <- matrix(nrow = numreal, ncol = numsim, 
                       dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    confisup <- matrix(nrow = numreal, ncol = numsim, 
                       dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            res <- t.test(xx, yy, paired = TRUE, conf.level = confidence)
            Valt[j, i] <- res$statistic
            p[j, i] <- res$p.value
            confiinf[j, i] <- res$conf.int[1]
            confisup[j, i] <- res$conf.int[2]
        }
    }
    
    return(list(Estadisticot = Valt, p = p, limiteinferiorIC = confiinf, limitesuperiorIC = confisup))
}

# ==============================================================================.
# Funciones de Índices y Métricas Compuestas ----
# ==============================================================================.

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Índice de Eficiencia (EF) o Índice de Nash ----
## Descripción: Calcula el índice de eficiencia de Nash-Sutcliffe entre las
## predicciones y las observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValEF <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    EF <- matrix(nrow = numreal, ncol = numsim, 
                 dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            mean_yy <- mean(yy, na.rm = TRUE)
            EF[j, i] <- 1 - (sum((xx - yy)^2, na.rm = TRUE) / sum((yy - mean_yy)^2, na.rm = TRUE))
        }
    }
    
    return(list(Eficiencia = EF))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Índice de Theil ----
## Descripción: Calcula el índice de Theil entre las predicciones y las
## observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValTheil <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    MC <- matrix(nrow = numreal, ncol = numsim, 
                 dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    SC <- matrix(nrow = numreal, ncol = numsim, 
                 dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    RC <- matrix(nrow = numreal, ncol = numsim, 
                 dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            n <- length(xx)
            MSE <- mean((xx - yy)^2, na.rm = TRUE)
            MC[j, i] <- (mean(xx, na.rm = TRUE) - mean(yy, na.rm = TRUE))^2 / MSE
            SC[j, i] <- ((n - 1) / n) * (sd(xx, na.rm = TRUE) - sd(yy, na.rm = TRUE))^2 / MSE
            RC[j, i] <- 2 * (1 - cor(xx, yy, use = "complete.obs")) * (n - 1) / n * sd(xx, na.rm = TRUE) * sd(yy, na.rm = TRUE) / MSE
        }
    }
    
    return(list(MC = MC, SC = SC, RC = RC))
}

# ==============================================================================.
# Funciones de Pérdida y Sesgo ----
# ==============================================================================.

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Pérdida de Huber (Huber Loss) ----
## Descripción: Calcula la pérdida de Huber entre las predicciones y las
## observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValHuberLoss <- function(x, y, delta = 1.0) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    huber_loss <- matrix(nrow = numreal, ncol = numsim, 
                         dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            error <- xx[valid_indices] - yy[valid_indices]
            huber_loss[j, i] <- mean(ifelse(abs(error) <= delta, 0.5 * error^2, delta * (abs(error) - 0.5 * delta)))
        }
    }
    
    return(list(huber_loss = huber_loss))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Pérdida Log-Cosh (Log-Cosh Loss) ----
## Descripción: Calcula la pérdida Log-Cosh entre las predicciones y las
## observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValLogCoshLoss <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    log_cosh_loss <- matrix(nrow = numreal, ncol = numsim, 
                            dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            log_cosh_loss[j, i] <- mean(log(cosh(xx[valid_indices] - yy[valid_indices])))
        }
    }
    
    return(list(log_cosh_loss = log_cosh_loss))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Pérdida Cuantílica (Quantile Loss) ----
## Descripción: Calcula la pérdida cuantílica entre las predicciones y las
## observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValQuantileLoss <- function(x, y, quantile = 0.5) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    quantile_loss <- matrix(nrow = numreal, ncol = numsim, 
                            dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            errors <- yy[valid_indices] - xx[valid_indices]
            quantile_loss[j, i] <- mean(pmax(quantile * errors, (quantile - 1) * errors))
        }
    }
    
    return(list(quantile_loss = quantile_loss))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Error Cuadrático Medio Logarítmico (MSLE) ----
## Descripción: Calcula el error cuadrático medio logarítmico entre las
## predicciones y las observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValMSLE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    msle <- matrix(nrow = numreal, ncol = numsim, 
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(xx > 0 & yy > 0 & !is.na(xx) & !is.na(yy))
            msle[j, i] <- mean((log1p(xx[valid_indices]) - log1p(yy[valid_indices]))^2)
        }
    }
    
    return(list(msle = msle))
}

# ==============================================================================.
# Funciones de Variabilidad y Diversidad ----
# ==============================================================================.

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Coeficiente de Variación (CV) ----
## Descripción: Calcula el coeficiente de variación entre las predicciones y
## las observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValCV <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    cv <- matrix(nrow = numreal, ncol = numsim, 
                 dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            cv[j, i] <- (sd(xx[valid_indices]) / mean(xx[valid_indices])) * 100
        }
    }
    
    return(list(cv = cv))
}

# ==============================================================================.
# Funciones de Distribución y Desigualdad ----
# ==============================================================================.

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Coeficiente de Gini ----
## Descripción: Calcula el coeficiente de Gini entre las predicciones y las
## observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValGini <- function(x, y) {
    library(reldist)
    x <- as.data.frame(x)
    y <- as.factor(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    gini <- matrix(nrow = numreal, ncol = numsim, 
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- as.numeric(x[[i]])
            yy <- as.numeric(y[[j]])
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            gini[j, i] <- gini(xx[valid_indices])
        }
    }
    
    return(list(gini = gini))
}

# ==============================================================================.
# Funciones de Clasificación y Precisión ----
# ==============================================================================.

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Pérdida de Bisagra (Hinge Loss) ----
## Descripción: Calcula la pérdida de bisagra entre las predicciones y las
## observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValHingeLoss <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.factor(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    hinge_loss <- matrix(nrow = numreal, ncol = numsim, 
                         dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            hinge_loss[j, i] <- mean(pmax(0, 1 - yy[valid_indices] * xx[valid_indices]))
        }
    }
    
    return(list(hinge_loss = hinge_loss))
}

# ==============================================================================.
# Funciones de Sesgo y Robustez ----
# ==============================================================================.

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Desviación Media del Sesgo (MBD) ----
## Descripción: Calcula la desviación media del sesgo entre las predicciones y
## las observaciones.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ValMBD <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)
    
    mbd <- matrix(nrow = numreal, ncol = numsim, 
                  dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    
    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            mbd[j, i] <- mean(xx[valid_indices] - yy[valid_indices])
        }
    }
    
    return(list(mbd = mbd))
}
