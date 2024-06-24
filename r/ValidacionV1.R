# ==============================================================================.
# TEMA      : FUNCIONES DE VALIDACIÓN
# AUTOR     : Henry P. Zumaeta Lozano
# CORREO    : henry.zumaeta.l@uni.pe
# VERSION   : 1.0
# VALIDADO  : 04/03/2024
# VALIDADO  : 24/06/2024
# ==============================================================================.

# ******************************************************************************
# FUNCIONES DE VALIDACION DE SIMULACIONES ----
# ******************************************************************************

## +++++++++++++++++++++++++++++++++++++++++++++++
## Coeficiente de informacion de Akaike (AIC) ----
## +++++++++++++++++++++++++++++++++++++++++++++++

ValAIC <- function (x, y, k) 
{
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- length(x)
    AIC <- numeric(m)
    lL <- numeric(m)
    sigmas2 <- numeric(m)
    rss <- numeric(m)
    z <- data.frame(y, x)
    modelo <- na.omit(z)
    n <- nrow(z)
    for (i in 1:m) {
        rss[i] <- sum((modelo[, 1] - modelo[, i + 1])^2)
        sigmas2[i] <- rss[i]/n
        lL[i] <- (-n/2) * (log(sigmas2[i]))
    }
    AIC <- -2 * lL + 2 * k
    if (max(k) != n + 1) {
        AICc <- AIC + 2 * k * (k + 1)/(n - k - 1)
    }
    else {
        (cat("no se puede calcular, division entre cero"))
    }
    return(list(AIC = AIC, AICc = AICc))
}

## +++++++++++++++++++++++++
## Inferencia bayesiana ----
## +++++++++++++++++++++++++

ValBayes <- function (x, y) 
{
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- length(x)
    L <- numeric(m)
    numerador <- numeric(m)
    apriori <- rep(1/m, m)
    lL <- numeric(m)
    sigmas2 <- numeric(m)
    rss <- numeric(m)
    z <- data.frame(y, x)
    modelo <- na.omit(z)
    n <- nrow(z)
    for (i in 1:m) {
        rss[i] <- sum((modelo[, 1] - modelo[, i + 1])^2)
        sigmas2[i] <- rss[i]/n
        lL[i] <- (-n/2) * (log(sigmas2[i]))
    }
    L <- exp(lL)
    numerador <- apriori * L
    denominador <- sum(numerador)
    posteriori <- numerador/denominador
    return(posteriori)
}

## ++++++++++++++++++++++++++++++++++++++++++++++++
## Coeficiente de correlacion de Spearman (ro) ----
## ++++++++++++++++++++++++++++++++++++++++++++++++

ValCorrela <- function (x, y) 
{
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- length(y)
    numsim <- length(x)
    correla <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                           1:numreal), paste("Modelo", 1:numsim)))
    for (i in 1:length(x)) {
        for (j in 1:length(y)) {
            xxx <- x[i]
            yyy <- y[j]
            z <- data.frame(xxx, yyy)
            zz <- na.omit(z)
            xx <- data.frame(zz[1])
            yy <- data.frame(zz[2])
            n <- nrow(xx)
            correla[j, i] = cor(xx, yy)
        }
    }
    return(list(Correlacion = correla))
}

## +++++++++++++++++++++++++++++++++++++++++++++++
## Indice de Eficiencia (EF) o Indice de Nash ----
## +++++++++++++++++++++++++++++++++++++++++++++++

ValEF <- function (x, y) 
{
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- length(y)
    numsim <- length(x)
    EF <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                      1:numreal), paste("Modelo", 1:numsim)))
    for (i in 1:length(x)) {
        for (j in 1:length(y)) {
            xxx <- x[i]
            yyy <- y[j]
            z <- data.frame(xxx, yyy)
            zz <- na.omit(z)
            xx <- data.frame(zz[1])
            yy <- data.frame(zz[2])
            EF[j, i] <- 1 - (sum((xx - yy)^2)/sum((yy - colMeans(yy))^2))
        }
    }
    return(list(Eficiencia = EF))
}

## +++++++++++++++++++++++++++++++
## Error Absoluto Medio (MAE) ----
## +++++++++++++++++++++++++++++++

ValMAE <- function (x, y) 
{
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- length(y)
    numsim <- length(x)
    mae <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                       1:numreal), paste("Modelo", 1:numsim)))
    pmae <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                        1:numreal), paste("Modelo", 1:numsim)))
    for (i in 1:length(x)) {
        for (j in 1:length(y)) {
            xxx <- x[i]
            yyy <- y[j]
            z <- data.frame(xxx, yyy)
            zz <- na.omit(z)
            xx <- data.frame(zz[1])
            yy <- data.frame(zz[2])
            n <- nrow(xx)
            mae[j, i] <- sum(abs(xx - yy))/n
            pmae[j, i] <- mae[j, i]/colMeans(yy) * 100
        }
    }
    return(list(mae = mae, pmae = pmae))
}

## +++++++++++++++++++++++++++++++++
## Error cuadratico medio (MSE) ----
## +++++++++++++++++++++++++++++++++

ValMSE <- function (x, y) 
{
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- length(y)
    numsim <- length(x)
    ecm <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                       1:numreal), paste("Modelo", 1:numsim)))
    recm <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                        1:numreal), paste("Modelo", 1:numsim)))
    precm <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                         1:numreal), paste("Modelo", 1:numsim)))
    for (i in 1:length(x)) {
        for (j in 1:length(y)) {
            xxx <- x[i]
            yyy <- y[j]
            z <- data.frame(xxx, yyy)
            zz <- na.omit(z)
            xx <- data.frame(zz[1])
            yy <- data.frame(zz[2])
            n <- nrow(xx)
            ecm[j, i] <- sum((xx - yy)^2)/n
            recm[j, i] <- sqrt(ecm[j, i])
            precm[j, i] <- recm[j, i]/colMeans(yy) * 100
        }
    }
    return(list(mse = ecm, rmse = recm, prmse = precm))
}

## ++++++++++++++++++++++++++++++++++++++++++
## Análisis de Perfil (Profile Analysis) ----
## ++++++++++++++++++++++++++++++++++++++++++

ValProfile <- function (x, y, instantetiempo) 
{
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    y <- na.omit(y)
    tam <- length(y)
    ins <- instantetiempo
    temp3 <- ins - 1
    temp4 <- nrow(x)
    var <- tam/ins
    T2 <- numeric(temp4)
    valorp <- numeric(temp4)
    for (m in 1:temp4) {
        fila <- x[m, ]
        yyy <- matrix(nrow = nrow(y), ncol = temp3 * var)
        k <- 1
        for (i in 1:var) {
            temp1 <- (i - 1) * ins + 1
            temp2 <- ins * i
            yy <- y[temp1:temp2]
            xx <- fila[temp1:temp2]
            yy = as.matrix(yy)
            xx = as.matrix(xx)
            for (j in 1:temp3) {
                yyy[, k] = yy[, j] - yy[, j + 1] - xx[, j] + 
                    xx[, j + 1]
                k <- k + 1
            }
        }
        res <- HotellingsT2(yyy, test = "chi")
        T2[m] <- res$statistic
        valorp[m] <- res$p.value
    }
    return(list(T2 = T2, valorp = valorp))
}

## +++++++++++++++++++++++
## Prueba F de Fisher ----
## +++++++++++++++++++++++

ValPruebaF <- function (x, y) 
{
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- length(y)
    numsim <- length(x)
    Famb <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                        1:numreal), paste("Modelo", 1:numsim)))
    ValP <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                        1:numreal), paste("Modelo", 1:numsim)))
    for (i in 1:length(x)) {
        for (j in 1:length(y)) {
            xxx <- x[i]
            yyy <- y[j]
            z <- data.frame(xxx, yyy)
            zz <- na.omit(z)
            xx <- data.frame(zz[1])
            yy <- data.frame(zz[2])
            xx <- as.matrix(xx)
            yy <- as.matrix(yy)
            m1 <- lm(yy ~ xx)
            a <- as.numeric(m1$coefficients[1])
            b <- as.numeric(m1$coefficients[2])
            n <- length(xx)
            s2xy <- as.numeric(sum(m1$residuals^2)/(n - 2))
            est <- (n * a^2 + 2 * a * (b - 1) * sum(xx) + (b - 
                                                               1)^2 * sum(xx^2))/(2 * s2xy)
            Famb[j, i] <- est
            ValP[j, i] <- pf(est, 2, n - 2, lower.tail = FALSE)
        }
    }
    return(list(F = Famb, p = ValP))
}

## ++++++++++++++++++++++++
## Prueba t de Student ----
## ++++++++++++++++++++++++

ValPruebat <- function (x, y, confidence = 0.95) 
{
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- length(y)
    numsim <- length(x)
    Valt <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                        1:numreal), paste("Modelo", 1:numsim)))
    p <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                     1:numreal), paste("Modelo", 1:numsim)))
    confiinf <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                            1:numreal), paste("Modelo", 1:numsim)))
    confisup <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                            1:numreal), paste("Modelo", 1:numsim)))
    for (i in 1:length(x)) {
        for (j in 1:length(y)) {
            xxx <- x[i]
            yyy <- y[j]
            z <- data.frame(xxx, yyy)
            zz <- na.omit(z)
            xx <- data.frame(zz[1])
            yy <- data.frame(zz[2])
            n <- nrow(xx)
            xx <- as.matrix(xx)
            yy <- as.matrix(yy)
            res <- t.test(xx, yy, paired = TRUE, conf.int = confidence)
            Valt[j, i] <- res$statistic
            p[j, i] <- res$p.value
            confiinf[j, i] <- res$conf.int[1]
            confisup[j, i] <- res$conf.int[2]
        }
    }
    return(list(Estadisticot = Valt, p = p, limiteinferiorIC = confiinf, 
                limitesuperiorIC = confisup))
}

## ++++++++++++++++++++++++++++++++++++++++++++
## Coeficiente de determinacion R cuadrado ----
## ++++++++++++++++++++++++++++++++++++++++++++

ValR2 <- function (x, y) 
{
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- length(y)
    numsim <- length(x)
    rcuadrado <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                             1:numreal), paste("Modelo", 1:numsim)))
    for (i in 1:length(x)) {
        for (j in 1:length(y)) {
            xxx <- x[i]
            yyy <- y[j]
            z <- data.frame(xxx, yyy)
            zz <- na.omit(z)
            xx <- data.frame(zz[1])
            yy <- data.frame(zz[2])
            n <- nrow(xx)
            rcuadrado[j, i] = (cor(xx, yy))^2
        }
    }
    return(list(rcuadrado = rcuadrado))
}

## ++++++++++++++++++++
## Indice de Theil ----
## ++++++++++++++++++++

ValTheil <- function (x, y) 
{
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- length(y)
    numsim <- length(x)
    MC <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                      1:numreal), paste("Modelo", 1:numsim)))
    SC <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                      1:numreal), paste("Modelo", 1:numsim)))
    RC <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                      1:numreal), paste("Modelo", 1:numsim)))
    for (i in 1:length(x)) {
        for (j in 1:length(y)) {
            xxx <- x[i]
            yyy <- y[j]
            z <- data.frame(xxx, yyy)
            zz <- na.omit(z)
            xx <- data.frame(zz[1])
            yy <- data.frame(zz[2])
            n <- nrow(xx)
            MSE <- sum((xx - yy)^2)/n
            MC[j, i] = ((sapply(xx, mean) - sapply(yy, mean))^2)/MSE
            SC[j, i] <- ((n - 1)/n) * ((sapply(xx, sd) - sapply(yy, 
                                                                sd))^2)/MSE
            RC[j, i] <- 2 * (1 - cor(xx, yy)) * ((n - 1)/n) * 
                (sapply(xx, sd) * sapply(yy, sd))/MSE
        }
    }
    return(list(MC = MC, SC = SC, RC = RC))
}

## ++++++++++++++++++++++++++++++
## Indice de ajuste Willmott ----
## ++++++++++++++++++++++++++++++

ValWillmott <- function (x, y) 
{
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- length(y)
    numsim <- length(x)
    W <- matrix(nrow = numreal, ncol = numsim, dimnames = list(paste("Observacion", 
                                                                     1:numreal), paste("Modelo", 1:numsim)))
    for (i in 1:length(x)) {
        for (j in 1:length(y)) {
            xxx <- x[i]
            yyy <- y[j]
            z <- data.frame(xxx, yyy)
            zz <- na.omit(z)
            xx <- data.frame(zz[1])
            yy <- data.frame(zz[2])
            numerador <- sum((yy - xx)^2)
            denominador <- sum((abs(xx - colMeans(yy)) + abs(yy - 
                                                                 colMeans(yy)))^2)
            W[j, i] = (1 - numerador/denominador)
        }
    }
    return(list(W = W))
}
