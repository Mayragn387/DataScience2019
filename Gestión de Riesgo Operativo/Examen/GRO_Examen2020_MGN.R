
#                 Gestión de Riesgo Operativo: Examen 2020                 
#                       Nombre: Mayra Goicochea Neyra                      
#                            Fecha: 07/06/2020                             


#------------------------------- Librerias --------------------------------
## Data Wrangling
library(tidyverse)
library(dplyr)
library(skimr)

## Estadisticos
library(actuar)
library(qrmtools)
library(MASS)
library(fitdistrplus)
library(moments)
library(CASdatasets) 
library(car)
library(extRemes)
library(qrmtools)

## Visualizacion
library(ggplot2)
library(vcd)
library(reshape2)
#Paleta de Colores
colors= c('#729ea1','#b5bd89','#dfbe99','#ec9192','#db5375')

#Funciones
library(vcd)
hist.period <- function (data, period, wknd = TRUE, crt = 0, begin = NULL, end = NULL,...) {
  if (missing(period)) {
    period <- "days" }
  v_date <- data[, "Date"]
  a <- as.Date(v_date)
  a <- sort(a)
  if (is.null(begin)) {
    begin <- min(a)
  }
  if (is.null(end)) {
    end <- max(a)
  }
  if (period == "days") {
    x <- as.numeric(difftime(end, begin)) + 1
    y <- as.numeric(table(a))
    if (wknd == T) {
      zero <- x - as.numeric(length(y))
    }
    if (wknd == F) {
      zero <- x - as.numeric(length(y) + 2 * floor(x/7))
    }
    if (crt != 0) {
      zero <- x - as.numeric(length(y) + 2 * floor(x/7) + 
                               crt)
    }
    z <- table(c(y, rep(0, zero)))
  }
  if (period == "weeks") {
    week.days <- weekdays(c(0:6) + as.Date("2010-01-04"))
    x <- which(week.days == weekdays(as.Date(begin)))
    begin <- as.Date(begin) - (x - 1)
    x <- which(week.days == weekdays(as.Date(end)))
    end <- as.Date(end) + (7 - x)
    n <- (as.numeric(difftime(end, begin)) + 1)/7
    y <- as.numeric(table(cut(a, br = begin + 7 * c(0:n))))
    z <- table(y)
  }
  if (period == "months") {
    a <- as.Date(cut(a, breaks = c("month")))
    b <- as.Date(cut(as.Date(c(begin, end)), breaks = c("month")))
    c <- seq(b[1], b[2] + 31, by = "month")
    y <- as.numeric(table(cut(a, br = c)))
    z <- table(y)
  }
  if (period == "quarters") {
    a <- as.Date(cut(a, breaks = c("quarter")))
    b <- as.Date(cut(as.Date(c(begin, end)), breaks = c("quarter")))
    c <- seq(b[1], b[2] + 92, by = "month")
    d <- as.Date(cut(c, breaks = c("quarter")))
    d <- unique(d)
    y <- as.numeric(table(cut(a, br = d)))
    z <- table(y)
  }
  barplot(z, main = c(paste("Frequency for", period)),...)
  structure = list(y = z)
}



fit.plot <-function (x, densfun, param, distname = NULL, col = c("red"), 
                     col2 = c("grey"), col3 = c("blue"), ylim = c(), xlim = c(), 
                     kernel = NULL, n = NULL, draw.diff = F, draw.max = F, scaled = F, 
                     positive = T, ...) 
{
  if (!is.null(distname)) {
    if (distname == "beta") {
      scaled <- T
    }
  }
  nm <- names(param)
  f <- formals(densfun)
  args <- names(f)
  m <- match(nm, args)
  formals(densfun) <- c(f[c(1, m)], f[-c(1, m)])
  dens <- function(parm, x, ...) densfun(x, parm, ...)
  if ((l <- length(nm)) > 1) 
    body(dens) <- parse(text = paste("densfun(x,", paste("parm[", 
                                                         1:l, "]", collapse = ", "), ", ...)"))
  dn <- function(x, n, kernel) {
    if (!is.null(n) & !is.null(kernel)) {
      dn <- density(x = x, n = n, kernel = kernel)
    }
    if (!is.null(n) & is.null(kernel)) {
      dn <- density(x = x, n = n)
    }
    if (is.null(n) & !is.null(kernel)) {
      dn <- density(x = x, kernel = kernel)
    }
    if (is.null(n) & is.null(kernel)) {
      dn <- density(x = x)
    }
    dn
  }
  if (scaled == T) {
    maximum <- max(x)
    max1 <- max(dn(x, n, kernel)$y)
    x.new <- x/max(x)
    max2 <- max(dens(x = x.new, as.numeric(param)))
    scale <- max1/max2
  }
  if (length(xlim) == 0 & length(ylim) == 0) {
    plot(dn(x, n, kernel), main = paste("Empirical and fitted density:", 
                                        distname), ...)
  }
  if (length(xlim) != 0 & length(ylim) == 0) {
    plot(dn(x, n, kernel), xlim = xlim, main = paste("Empirical and fitted density:", 
                                                     distname), ...)
  }
  if (length(xlim) == 0 & length(ylim) != 0) {
    plot(dn(x, n, kernel), ylim = ylim, main = paste("Empirical and fitted density:", 
                                                     distname), ...)
  }
  if (length(xlim) != 0 & length(ylim) != 0) {
    plot(dn(x, n, kernel), ylim = ylim, xlim = xlim, main = paste("Empirical and fitted density:", 
                                                                  distname), ...)
  }
  if (scaled == FALSE) {
    curve(dens(x = x, as.numeric(param)), add = TRUE, col = col, 
          lwd = 2)
  }
  if (scaled == TRUE) {
    curve(scale * dens(x = x/maximum, as.numeric(param)), 
          add = TRUE, col = col, lwd = 2)
  }
  xp <- dn(x = x, n, kernel)$x
  if (positive == T) {
    nmbrs <- which(xp > 0)
    xp <- xp[xp > 0]
  }
  if (positive == F) {
    nmbrs <- c(1:length(xp))
  }
  yp <- dn(x = x, n, kernel)$y[nmbrs]
  if (scaled == F) {
    teor <- dens(x = xp, as.numeric(param))
  }
  if (scaled == T) {
    teor <- scale * dens(x = xp/maximum, as.numeric(param))
  }
  emp <- yp
  if (draw.diff == T) {
    for (i in 1:length(xp)) {
      lines(c(xp[i], xp[i]), c(emp[i], teor[i]), col = col2)
    }
  }
  diff <- abs(teor - emp)
  ad <- sum(diff)
  maxdiff <- max(diff)
  meandiff <- mean(diff)
  if (draw.max == T) {
    num <- which(diff == max(diff))
    for (i in num) {
      lines(c(xp[i], xp[i]), c(emp[i], teor[i]), col = col3)
    }
  }
  structure(list(teor = teor, emp = emp, ad = ad, maxdiff = maxdiff, 
                 meandiff = meandiff), class = "fitplot")
}

loss.fit.dist <-function(densfun, x, start = NULL, name = NULL, qq = FALSE, 
                         period, ylim = c(), xlim = c(), col = "red", from = 0.1^15, 
                         to = 1 - 0.1^15, length.out = 10000, by = NULL, kernel = NULL, 
                         n = NULL, draw.diff = F, draw.max = F, xlog.scale = F, ...) 
{
  y <- match.call()[[2]]
  if (missing(period)) {
    period <- "none"
  }
  if (!is.element(period, c("none", "days", "weeks", "months", 
                            "quarters"))) {
    stop("period should be none, days, weeks, months or quarters")
  }
  if (!is.null(dim(x))) {
    if (dim(x)[2] != 2) 
      stop("x should be one- or two dimensional")
    if (period != "none") {
      x <- period.loss(x, period)
    }
    if (period == "none") {
      x <- x[, 2]
    }
    if (length(dim(x)) > 2) 
      stop("x should be one- or two dimensional")
  }
  distname <- NULL
  qfun <- NULL
  k <- NULL
  if (is.character(densfun)) {
    distname <- tolower(densfun)
    k <- switch(EXPR = densfun, beta = "qbeta", cauchy = "qcauchy", 
                `chi-squared` = "qchisq", exponential = "qexp", f = "qf", 
                gamma = "qgamma", `log-normal` = "qlnorm", lognormal = "qlnorm", 
                logistic = "qlogis", normal = "qnorm", weibull = "qweibull", 
                `inverse gaussian` = "qinvGauss", NULL)
    densfun <- switch(EXPR = densfun, beta = dbeta, cauchy = dcauchy, 
                      `chi-squared` = dchisq, exponential = dexp, f = df, 
                      gamma = dgamma, `log-normal` = dlnorm, lognormal = dlnorm, 
                      logistic = dlogis, normal = dnorm, weibull = dweibull, 
                      `inverse gaussian` = dinvGauss, NULL)
  }
  if (is.null(densfun)) {
    stop("unsupported distribution")
  }
  m <- mean(x)
  v <- var(x)
  sd <- sd(x)
  if (!is.null(distname)) {
    if (distname == "beta" & is.null(start)) {
      x.old <- x
      x <- x/max(x)
      print("Argument scaled; x<- x/max(x)")
      scaled = TRUE
      m <- mean(x)
      v <- var(x)
      x <- x[x < 1]
      start <- list(shape1 = max(-(m * (m^2 - m + v))/v, 
                                 0.1^(100)), shape2 = max((-1 + m) * (-m + m^2 + 
                                                                        v)/v, 0.1^(100)))
    }
    if (distname == "gamma" & is.null(start)) {
      start <- list(shape = max(m^2/v, 0), scale = max(v/m, 
                                                       0.1^(100)))
    }
    if (distname == "inverse gaussian" & is.null(start)) {
      start <- list(lambda = max(m^3/v, 0.1^(100)), nu = max(m, 
                                                             0.1^(100)))
    }
    if (distname == "chi-squared") {
      start <- list(df = m)
    }
    if (distname == "f") {
      start <- list(df1 = max(-2 * m^2/(-m^2 + m^3 - 2 * 
                                          v + m * v), 4), df2 = max(2 * m/(m - 1), 2))
    }
    if (!is.null(start) & distname %in% c("lognormal", "log-normal", 
                                          "exponential", "normal")) {
      stop(paste(" supplying pars for the ", distname, 
                 " is not supported"))
    }
    if (is.null(start) & !(distname %in% c("inverse gaussian"))) {
      param <- as.list(fitdistr(x, distname, ...)$estimate)
      loglik <- fitdistr(x, distname, ...)$loglik
      ese <- fitdistr(x, distname, ...)$sd
    }
    if (!is.null(start) & !distname %in% c("inverse gaussian", 
                                           "f")) {
      param <- as.list(fitdistr(x, distname, start = start, 
                                ...)$estimate)
      loglik <- fitdistr(x, distname, start = start, ...)$loglik
      ese <- fitdistr(x, distname, start = start, ...)$sd
    }
    if (distname %in% c("inverse gaussian")) {
      param <- as.list(fitdistr(x, densfun, start = start, 
                                ...)$estimate)
      loglik <- fitdistr(x, densfun, start = start, ...)$loglik
      ese <- fitdistr(x, densfun, start = start, ...)$sd
    }
    if (distname %in% c("f")) {
      param <- as.list(fitdistr(x, densfun, start = start, 
                                lower = 0.01, ...)$estimate)
      loglik <- fitdistr(x, densfun, start = start, lower = 0.01, 
                         ...)$loglik
      ese <- fitdistr(x, densfun, start = start, lower = 0.01, 
                      ...)$sd
    }
  }
  if (is.null(start) & is.null(distname)) {
    stop("'start' must be a named list")
  }
  if (!is.null(start) & is.null(distname)) {
    param <- as.list(fitdistr(x, densfun, start = start, 
                              ...)$estimate)
    loglik <- fitdistr(x, densfun, start = start, ...)$loglik
    ese <- fitdistr(x, densfun, start = start, ...)$sd
  }
  nm <- names(param)
  f <- formals(densfun)
  args <- names(f)
  m <- match(nm, args)
  formals(densfun) <- c(f[c(1, m)], f[-c(1, m)])
  dens <- function(parm, x, ...) densfun(x, parm, ...)
  if ((l <- length(nm)) > 1) 
    body(dens) <- parse(text = paste("densfun(x,", paste("parm[", 
                                                         1:l, "]", collapse = ", "), ", ...)"))
  max <- max(dens(x = x, as.numeric(param)))
  min <- min(dens(x = x, as.numeric(param)))
  log.scale <- if (!is.null(distname)) {
    if (distname == "beta") {
      out <- fit.plot(densfun = densfun, x = x.old, param = param, 
                      distname = "beta", col = col, ylim = ylim, xlim = xlim, 
                      kernel = kernel, n = n, draw.diff = draw.diff, 
                      draw.max = draw.max, log = ifelse(xlog.scale == 
                                                          F, paste(""), paste("x")))
    }
    if (distname != "beta") {
      out <- fit.plot(densfun = densfun, x = x, param = param, 
                      distname = distname, col = col, ylim = ylim, 
                      xlim = xlim, kernel = kernel, n = n, draw.diff = draw.diff, 
                      draw.max = draw.max, log = ifelse(xlog.scale == 
                                                          F, paste(""), paste("x")))
    }
  }
  if (is.null(distname)) {
    out <- fit.plot(densfun = densfun, x = x, param = param, 
                    distname = NULL, col = col, ylim = ylim, xlim = xlim, 
                    kernel = kernel, n = n, draw.diff = draw.diff, draw.max = draw.max, 
                    log = ifelse(xlog.scale == F, paste(""), paste("x")))
  }
  ad <- out$ad
  names(ad) <- c("ad")
  teor.dens <- out$teor
  emp.dens <- out$emp
  maxdiff <- out$maxdiff
  meandiff <- out$meandiff
  if (qq == TRUE) {
    y <- as.character(y)
    ifelse(is.null(k), k <- paste("q", substr(y, 2, nchar(as.character(y))), 
                                  sep = ""), k)
    if (is.null(qfun)) {
      qfun <- get(k, mode = "function", envir = parent.frame())
    }
    f <- formals(paste(k))
    args <- names(f)
    m <- match(nm, args)
    formals(qfun) <- c(f[c(1, m)], f[-c(1, m)])
    quan <- function(parm, p, ...) qfun(p, parm, ...)
    if ((l <- length(nm)) > 1) 
      body(quan) <- parse(text = paste("qfun(p,", paste("parm[", 
                                                        1:l, "]", collapse = ", "), ", ...)"))
    if (is.null(by)) {
      q.t <- quan(p = seq(from = from, to = to, length.out = length.out), 
                  as.numeric(param))
      q.e <- quantile(x, seq(from = from, to = to, length.out = length.out))
    }
    else {
      q.t <- quan(p = seq(from = from, to = to, by = by), 
                  as.numeric(param))
      q.e <- quantile(x, seq(from = from, to = to, by = by))
    }
    qqplot(q.t, q.e, main = paste("QQ-plot distr.", if (!is.null(distname)) 
      distname, if (!is.null(name)) 
        name), xlim = c(q.t[2], q.t[length(q.t) - 1]), ylim = c(q.e[1], 
                                                                q.e[length(q.e)]))
    abline(0, 1)
  }
  if (qq == FALSE) {
    q.e <- NULL
    q.t <- NULL
  }
  structure(list(loglik = loglik, param = param, sd = ese, 
                 q.t = q.t, q.e = q.e, ad = ad, teor.dens = teor.dens, 
                 emp.dens = emp.dens, maxdiff = maxdiff, meandiff = meandiff), 
            class = "lf")
}



period.loss <- function(data, period = c("none", "days", "weeks", "months", 
                                         "quarters"), dts = FALSE){
  a <- as.Date(data[, "Date"])
  a <- sort(a)
  if (missing(period)) {
    period <- "none"
  }
  if (period == "none") {
    z <- data[, 2]
    if (dts == TRUE) {
      names <- data[, 1]
    }
  }
  if (period != "none") {
    if (period == "days") {
      y <- as.numeric(table(a))
      if (dts == TRUE) {
        names <- names(table(a))
      }
    }
    if (period == "weeks") {
      week.days <- weekdays(c(0:6) + as.Date("2010-01-04"))
      x <- which(week.days == weekdays(a[1]))
      begin <- a[1] - (x - 1)
      x <- which(week.days == weekdays(a[length(a)]))
      end <- a[length(a)] + (7 - x)
      n <- (as.numeric(difftime(end, begin)) + 1)/7
      y <- table(cut(a, br = begin + 7 * c(0:n)))
      if (dts == TRUE) {
        names <- names(y[y > 0])
      }
      y <- as.numeric(y)
    }
    if (period == "months") {
      a <- as.Date(cut(a, breaks = c("month")))
      b <- as.Date(cut(c(min(a), max(a)), breaks = c("month")))
      c <- seq(b[1], b[2] + 31, by = "month")
      y <- table(cut(a, br = c))
      if (dts == TRUE) {
        names <- names(y[y > 0])
      }
      y <- as.numeric(y)
    }
    if (period == "quarters") {
      a <- as.Date(cut(a, breaks = c("quarter")))
      b <- as.Date(cut(c(min(a), max(a)), breaks = c("quarter")))
      c <- seq(b[1], b[2] + 92, by = "month")
      d <- as.Date(cut(c, breaks = c("quarter")))
      d <- unique(d)
      y <- table(cut(a, br = d))
      if (dts == TRUE) {
        names <- names(y[y > 0])
      }
      y <- as.numeric(y)
    }
    data2 <- data[, "Loss"]
    data2 <- data2[order(a)]
    z <- key.sum(data2, y)
  }
  if (dts == TRUE) {
    names(z) <- names
  }
  z
}
key.sum <-function(v, u){
  wym <- {
  }
  j = 1
  k = 1
  for (i in 1:length(u)) {
    if (u[i] != 0) {
      wym[k] = sum(v[j:(j + u[i] - 1)])
      k = k + 1
    }
    j <- j + u[i]
  }
  if (length(v) != sum(u)) {
    print("There is not enough u or v data")
  }
  wym
}

## Valores extremos
library(evir)
library(evmix)

# Data Loading

setwd('C:/Users/Goicochea/Desktop/CUNEF/Cursos/Gestion del Riesgo Operativo/Examen')

freq <- read.csv2('frecuencias.csv')
importe <- read.csv2('importes.csv')

#--------------------------------- EDA ------------------------------------

data <- do.call(rbind, Map(data.frame, Frecuencia=freq, Severidad=importe))
date <- seq(as.Date("2020/01/01") , by = "day", length.out = 75)
#Resumen
## Se realiza un resumen de los datos de Severidad y Frecuencia
summary(data)

##Calculo de la Moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
d1 <- data.frame(Min = min(data$Frecuencia),
                 Mediana = median(data$Frecuencia),
                 Media = mean(data$Frecuencia),
                 Moda = getmode(data$Frecuencia),
                 SD = sd(data$Frecuencia),
                 Max = max(data$Frecuencia), row.names = c("Frecuencia"))
d2 <- data.frame(Min = min(data$Severidad),
                 Mediana = median(data$Severidad),
                 Media = mean(data$Severidad),
                 Moda = getmode(data$Severidad),
                 SD = sd(data$Severidad),
                 Max = max(data$Severidad), row.names = c("Pérdidas"))

q_stats <- rbind(d2,d1)
q_stats

#| VARIABLE    | Min       | Mediana | Media    | Moda     | SD       | Max      |
#|-------------|-----------|---------|----------|----------|----------|----------|
#| Pérdidas    | 0.7376091 | 6.70145 | 7.470175 | 4.371318 | 3.745131 | 18.41415 |
#| Frecuencia  |     5     |    13   |   12.24  |     9    | 3.657055 |    21    |

# Ambas variables no son asimetricas, según los estadisticos, por que la media y la moda son diferentes.

#Cuantiles
## Frecuencia
quantile(data$Frecuencia,seq(0,1, 0.20))

#| 0% | 20% | 40% | 60% | 80%   | 100% |
#|----|-----|-----|-----|-------|------|
#| 5  | 9   | 11  | 13  | 15.2  | 21   |
  
quantile(data$Frecuencia,probs = c(0.05, 0.95))
quantile(data$Frecuencia,seq(0.9,1, 0.01))

## Severidad
quantile(data$Severidad,seq(0,1, 0.20))
#| 0%         | 20%       | 40%       | 60%       | 80%        | 100%       |
#|------------|-----------|-----------|-----------|------------|------------|
#|  0,7376091 | 4,0756986 | 5,9618997 | 8,3976426 | 10,8934549 | 18,4141460 |
  
quantile(data$Severidad,probs = c(0.05, 0.95))
quantile(data$Severidad,seq(0.9,1, 0.01))

#Representación Gráfica
## Frecuencia

ggplot(data, aes(x = Frecuencia)) +
  geom_histogram(aes(y = ..density..),
                 bins = 20,
                 colour = colors[2],
                 fill = colors[2], alpha=0.5) +
  geom_density(fill = colors[1],
               color = colors[1],
               alpha = 0.4) +
  labs(title = "Densidad de los Eventos", x = "Frecuencia", y = "Densidad") +
  theme_bw()

## Severidad
ggplot(data, aes(x = Severidad)) +
  geom_histogram(aes(y = ..density..),
                 bins = 20,
                 colour = colors[5],
                 fill = colors[5], alpha=0.5) +
  geom_density(fill = colors[4],
               color = colors[4],
               alpha = 0.4) +
  labs(title = "Densidad de los Pérdidas", x = "Severidad", y = "Densidad") +
  theme_bw()

#Representación de series temporales
## Frecuencia
frecuencia <- as.xts(data$Frecuencia,order.by = as.Date(date))
plot.ts(frecuencia, col=colors[1], lwd = 2)

## Severidad
severidad <- as.xts(data$Severidad,order.by = as.Date(date))
plot.ts(severidad, col=colors[4], lwd = 2)

# Boxplot
## Frecuencia
boxplot(data$Frecuencia, col=colors[1])

## Severidad
boxplot(data$Severidad, col=colors[4])

#Simetria
#El coeficiente de Skewness prueba si se tiene simetria en 
#la distribución de los datos, y si es igual a 0 significa 
#que los datos son totalmente simétricos.
## Frecuencia
skewness(data$Frecuencia) #0.07854948, Es ligeramente asimetrica

## Severidad
skewness(data$Severidad) #0.4781153, es ligeramente asimetrica

#Kurtosis
#Esta medida determina el grado de concentración que presentan 
#los valores en la región central de la distribución. Por medio
#del Coeficiente de Curtosis, podemos identificar si existe una
#gran concentración de valores (Leptocúrtica), una concentración
#normal (Mesocúrtica) ó una baja concentración (Platicúrtica).

## Frecuencia
kurtosis(data$Frecuencia) #-0.7683295, es una platicurtica, significa que es menos apuntada que la normal.

## Severidad
kurtosis(data$Severidad) #-0.2658595, es una platicurtica, significa que es menos apuntada que la normal.

#---------------SELECCIÓN DEL MODELO: INFERENCIA PARAMÉTRICA--------------------
## Frecuencia

### Metodo de Maxima Verosimilitud
####Binomial Negativa
fit.nbinom <- fitdist(data$Frecuencia,"nbinom",method = "mle",discrete = TRUE)
plot(fit.nbinom)
data$Date = date
rootogram(goodfit(data$Frecuencia, "nbinomial", "ML"))

####Poisson
fit.pois <- fitdist(data$Frecuencia,"pois",method = "mle",discrete = TRUE)
plot(fit.pois)
rootogram(goodfit(data$Frecuencia, "poisson", "ML"))

####Geometrica
fit.geo <- fitdist(data$Frecuencia,"geom",method = "mle",discrete = TRUE)
plot(fit.geo)
rootogram(goodfit(data$Frecuencia, "nbinomial", "ML",par=list(size=1))) 
#La geometrica es una binomial negativa con size=1 (https://cran.r-project.org/doc/contrib/Saez-Castillo-RRCmdrv21.pdf)

####Uniforme Discreta
fit.uni <- fitdist(data$Frecuencia,"unif",method = "mle",discrete = TRUE)
plot(fit.uni)

####Binomial
fit.bin <- fitdist(data$Frecuencia,"binom",method = "mle",
                   fix.arg=list(size = 100),
                   start=list(prob = 1/length(freq$x)),
                   discrete = TRUE)
rootogram(goodfit(data$Frecuencia, "binomial", "ML"))
plot(fit.bin)

##### Bondad de Ajuste
gofstat(list(fit.nbinom,fit.pois,fit.geo,fit.uni,fit.bin), 
        chisqbreaks = c(0:4, 9), 
        discrete = TRUE,
        fitnames = c("Binomial negativa", "Poisson", "Geométrica", "Uniforme Discreta", "Binomial"))
#|         | Binomial Negativa | Poisson  | Geometrica | Uniforme Discreta | Binomial |
#|---------|-------------------|----------|------------|-------------------|----------|
#| AIC     | 410,0171          | 408,2442 | 533,6735   | NA                | 409,7016 |
#| BIC     | 414,6521          | 410,5617 | 535,9910   | NA                | 412,0191 |
#| Ranking | 3                 | 1        | 4          | 5                 | 2        |

#Los dos mejores son la Uniforme Discreta y la Poisson.
cdfcomp(list(fit.nbinom,fit.pois,fit.geo,fit.uni,fit.bin),
        ylab = "Probabilidad", datapch=".",
        addlegend = TRUE,
        datacol="grey40", fitcol=colors, lwd=4,
        legendtext=c("Binomial negativa", "Poisson", "Geométrica", "Uniforme Discreta", "Binomial"),
        main="Distribuciones ajustadas a la Frecuencia", plotstyle = "ggplot") +
  theme_bw()


##### Diagrama QQ-Plot
qqcomp(list(fit.nbinom,fit.pois,fit.geo,fit.uni,fit.bin),
       ylab = "cuantiles empíricos",
       xlab = "cuantiles teóricos",
       fitcol = colors, 
       main = "QQ-plot sobre Distribuciones Ajustadas a la frecuencia - MLE",
       addlegend = TRUE,
       legendtext = c("Binomial negativa", "Poisson", "Geométrica", "Uniforme Discreta", "Binomial")) +
  theme_bw()

ppcomp(list(fit.nbinom,fit.pois,fit.geo,fit.uni,fit.bin),  
       ylab="Probabilidades empíricas", xlab="Probabilidades teóricas", fitcol=colors,
       main="PP-plot sobre distribuciones ajustadas a la Frecuencia", 
       legendtext=c("Binomial negativa", "Poisson", "Geométrica", "Uniforme Discreta", "Binomial"),
       #plotstyle = "ggplot", 
       addlegend = TRUE,
       fitpch=1:4)+
  theme_bw()

### Metodo MME
####Binomial Negativa
fit.nbinomMME <- fitdist(data$Frecuencia,"nbinom",method = "mme",discrete = TRUE)
plot(fit.nbinomMME)

####Poisson
fit.poisMME <- fitdist(data$Frecuencia,"pois",method = "mme",discrete = TRUE)
plot(fit.poisMME)

####Geometrica
fit.geoMME <- fitdist(data$Frecuencia,"geom",method = "mme",discrete = TRUE)
plot(fit.geoMME)

####Uniforme Discreta
fit.uniMME <- fitdist(data$Frecuencia,"unif",method = "mme",discrete = TRUE)
plot(fit.uniMME)

##### Bondad de Ajuste
gofstat(list(fit.nbinomMME,fit.poisMME,fit.geoMME,fit.uniMME), 
        chisqbreaks = c(0:4, 9), 
        discrete = TRUE,
        fitnames = c("Binomial negativa", "Poisson", "Geométrica", "Uniforme Discreta"))
#|         | Binomial Negativa | Poisson  | Geometrica | Uniforme Discreta |
#|---------|-------------------|----------|------------|-------------------|
#| AIC     | 410,0175          | 408,2442 | 533,6735   | Inf               |
#| BIC     | 414,6525          | 410,5617 | 535,9910   | Inf               |
#| Ranking | 2                 | 1        | 3          | 4                 |
  

#El mejor es la Poisson.
cdfcomp(list(fit.nbinomMME,fit.poisMME,fit.geoMME,fit.uniMME),
        ylab = "Probabilidad", datapch=".",
        addlegend = TRUE,
        datacol="grey40", fitcol=colors, lwd=4,
        legendtext=c("Binomial negativa", "Poisson", "Geométrica", "Uniforme Discreta"),
        main="Distribuciones ajustadas a la Frecuencia", plotstyle = "ggplot") +
  theme_bw()


##### Diagrama QQ-Plot
qqcomp(list(fit.nbinomMME,fit.poisMME,fit.geoMME,fit.uniMME),
       ylab = "cuantiles empíricos",
       xlab = "cuantiles teóricos",
       fitcol = colors, 
       main = "QQ-plot sobre Distribuciones Ajustadas a la frecuencia - MME",
       addlegend = TRUE,
       legendtext = c("Binomial negativa", "Poisson", "Geométrica", "Uniforme Discreta"),
       fitpch = 1:4) +
  theme_bw()

## Severidad

### Metodo de Maxima Verosimilitud
####Exponencial
fit.exp <- fitdist(data$Severidad,"exp",method = "mle")
plot(fit.exp)
fit <- loss.fit.dist("exponential",data$Severidad, ylim = c(0,0.2), draw.diff=T)
####Lognormal
fit.ln <- fitdist(data$Severidad,"lnorm",method = "mle")
plot(fit.ln)
fit <- loss.fit.dist("log-normal",data$Severidad, ylim = c(0,0.2), draw.diff=T)

####Weibull
fit.wei <- fitdist(data$Severidad,"weibull",method = "mle")
plot(fit.wei)
fit <- loss.fit.dist("weibull",data$Severidad, ylim = c(0,0.2), draw.diff=T) #Tiene menos diferencias

####Gamma
fit.gam <- fitdist(data$Severidad,"gamma",method = "mle")
plot(fit.gam)
fit <- loss.fit.dist("gamma",data$Severidad, ylim = c(0,0.2), draw.diff=T)


##### Bondad de Ajuste
gofstat(list(fit.exp,fit.ln,fit.wei,fit.gam), 
        chisqbreaks = c(0:4, 9), 
        fitnames = c("Exponencial", "Lognormal", "Weibull", "Gamma"))
#|         | Exponencial | Lognormal | Weibull  | Gamma    |
#|---------|-------------|-----------|----------|----------|
#| AIC     | 453,6378    | 417,5396  | 406,8760 | 409,1073 |
#| BIC     | 455,9553    | 422,1745  | 411,5109 | 413,7422 |
#| Ranking | 4           | 3         | 1        | 2        |
  

#Los dos mejores son la Weibull y Gamma
denscomp(list(fit.exp,fit.ln,fit.wei,fit.gam),
         ylab = "Probabilidad", datapch=".",
         addlegend = TRUE,
         datacol="white", fitcol=colors,lwd=3, 
         legendtext=c("Exponencial", "Lognormal", "Weibull", "Gamma"),
         main="Distribuciones ajustadas a la Severidad", plotstyle = "ggplot") +
  theme_bw()


##### Diagrama QQ-Plot
qqcomp(list(fit.exp,fit.ln,fit.wei,fit.gam),
       ylab = "cuantiles empíricos",
       xlab = "cuantiles teóricos",
       fitcol = colors, 
       main = "QQ-plot sobre Distribuciones Ajustadas a la Severidad - MLE",
       addlegend = TRUE,
       legendtext = c("Exponencial", "Lognormal", "Weibull", "Gamma")) +
  theme_bw()
ppcomp(list(fit.exp,fit.ln,fit.wei,fit.gam),  
       ylab="probabilidades empíricas", xlab="probabilidades teóricas", 
       fitcol=colors,
       main="PP-plot sobre Distribuciones Ajustadas a la Severidad",
       addlegend = TRUE,
       legendtext=c("Exponencial", "Lognormal", "Weibull", "Gamma"),
       fitpch=1:4) +
  theme_bw()

### Metodo MME
####Exponencial
fit.expMME <- fitdist(data$Severidad,"exp",method = "mme")
plot(fit.expMME)

####Lognormal
fit.lnMME <- fitdist(data$Severidad,"lnorm",method = "mme")
plot(fit.lnMME)

####Weibull
memp  <-  function(x, order) mean(x^order)
fit.weiMME <- fitdist(data$Severidad, "weibull", method = "mme", order=c(1, 2),
                       memp = memp,start=list(shape=10, scale=10), lower=0, upper=Inf)
plot(fit.weiMME)

####Gamma
fit.gamMME <- fitdist(data$Severidad,"gamma",method = "mme")
plot(fit.gamMME)

##### Bondad de Ajuste
gofstat(list(fit.expMME,fit.lnMME,fit.weiMME,fit.gamMME), 
        chisqbreaks = c(0:4, 9), 
        fitnames = c("Exponencial", "Lognormal", "Weibull", "Gamma"))
#|         | Exponencial | Lognormal | Weibull | Gamma    |
#|---------|-------------|-----------|---------|----------|
#| AIC     | 453,6378    | 427,3547  | 406,881 | 409,9499 |
#| BIC     | 455,9553    | 431,9897  | 411,516 | 414,5848 |
#| Ranking | 4           | 3         | 1       | 2        |
  
#Los dos mejores son la Weibull y Gamma
denscomp(list(fit.expMME,fit.lnMME,fit.weiMME,fit.gamMME),
         ylab = "Probabilidad", datapch=".",
         addlegend = TRUE,
         datacol="white", fitcol=colors,lwd=3, 
         legendtext=c("Exponencial", "Lognormal", "Weibull", "Gamma"),
         main="Distribuciones ajustadas a la Severidad", plotstyle = "ggplot") +
  theme_bw()


##### Diagrama QQ-Plot
qqcomp(list(fit.expMME,fit.lnMME,fit.weiMME,fit.gamMME),
       ylab = "cuantiles empíricos",
       xlab = "cuantiles teóricos",
       fitcol = colors, 
       main = "QQ-plot sobre Distribuciones Ajustadas a la Severidad - MME",
       addlegend = TRUE,
       legendtext = c("Exponencial", "Lognormal", "Weibull", "Gamma"),
       fitpch = 1:4) +
  theme_bw()
ppcomp(list(fit.expMME,fit.lnMME,fit.weiMME,fit.gamMME),  
       ylab="probabilidades empíricas", xlab="probabilidades teóricas", 
       fitcol=colors,
       main="PP-plot sobre Distribuciones Ajustadas a la Severidad",
       addlegend = TRUE,
       legendtext=c("Exponencial", "Lognormal", "Weibull", "Gamma"),
       fitpch=1:4) +
  theme_bw()

#--------------------------- VALORES EXTREMOS ------------------------------

## EVT:Threshold Over Peak

#Como no se tiene la información del periodo de tiempo, se dividen en 5 grupos para determinar el umbral:
#se observa que el umbra más adecuado segun los valores maximos es 14.5
max(data$Severidad[1:15])
max(data$Severidad[16:30])
max(data$Severidad[31:45])
max(data$Severidad[46:60]) 
max(data$Severidad[61:75]) 

# Se fija el umbral en 14.5
u <- 14.5

exc <- data$Severidad[data$Severidad > u] #se extraen las observaciones que exceden el umbral
exc #3 observaciones

nObs <- length(exc) #cuantas observaciones son exceso del umbral

# Hay 3 casos que exceden el umbral.

prob <- 1 - base::rank(exc)/(nObs + 1)  #rank ofrece el n de orden
prob


# Se calcula la función de distribución acumulada:

alfa <- -cov(log(exc), log(prob)) / var(log(exc))
alfa

x = seq(u, max(exc), length = 100) #divide de u a max() 100 interv.
y = (x / u)^(-alfa)


prob <- rank(exc) / (nObs + 1)
y = 1 - (x / u)^(-alfa)
plot(exc, prob, log = "xy", xlab = "Excesos Fraudes (Phishing)", 
     ylab = "Probabilidades", ylim=c(0.01, 1), col=colors[5])
lines(x, y)

# Se procede a la estimación reutilizando el código visto en las prácticas.

#Distribucion valores extremos generalizados (GEV) 
nllik.gev <- function(par, data){
  mu <- par[1]
  sigma <- par[2]
  xi <- par[3]
  if ((sigma <= 0) | (xi <= -1))
    return(1e6)
  n <- length(data)
  if (xi == 0)
    n * log(sigma) + sum((data - mu) / sigma) +
    sum(exp(-(data - mu) / sigma))
  else {
    if (any((1 + xi * (data - mu) / sigma) <= 0))
      return(1e6)
    n * log(sigma) + (1 + 1 / xi) *
      sum(log(1 + xi * (data - mu) / sigma)) +
      sum((1 + xi * (data - mu) / sigma)^(-1/xi))
  }
}

# GEV
sigma.start <- sqrt(6) * sd(exc) / pi
mu.start <- mean(exc) + digamma(1) * sigma.start
fit.gev <- nlm(nllik.gev, c(mu.start, sigma.start, 0),
               hessian = TRUE, data = exc)
fit.gev

fit.gev$estimate 

# Generalizada de Pareto

nllik.gp <- function(par, u, data){
  tau <- par[1]
  xi <- par[2]
  if ((tau <= 0) | (xi < -1))
    return(1e6)
  m <- length(data)
  if (xi == 0)
    m * log(tau) + sum(data - u) / tau
  else {
    if (any((1 + xi * (data - u) / tau) <= 0))
      return(1e6)
    m * log(tau) + (1 + 1 / xi) *
      sum(log(1 + xi * (data - u) / tau))
  }
}

# Obtención de los parámetros PARETO
tau.start <- mean(exc) - u 
fit.gp <- nlm(nllik.gp, c(tau.start, 0), u = u, hessian = TRUE,
              data = exc)
fit.gp
fit.gp$estimate 
sqrt(diag(solve(fit.gp$hessian))) 

qqgpd <- function(data, u, tau, xi){
  excess <- data[data > u]
  m <- length(excess)
  prob <- 1:m / (m + 1)
  x.hat <- u + tau / xi * ((1 - prob)^-xi - 1)
  ylim <- xlim <- range(x.hat, excess)
  plot(sort(excess), x.hat, xlab = "Cuantiles en la muestra",
       ylab = "Cuantiles ajustados", xlim = xlim, ylim = ylim,
       col=colors[5])
  abline(0, 1, col = "grey")
}

qqgpd(data$Severidad, u, fit.gp$estimate[1], fit.gp$estimate[2]) 


#P-P Plot para la Dist. Generalizada de Pareto (DGP)
ppgpd <- function(data, u, tau, xi){
  excess <- data[data > u]
  m <- length(excess)
  emp.prob <- 1:m / (m + 1)
  prob.hat <- 1 - (1 + xi * (sort(excess) - u) / tau)^(-1/xi)
  plot(emp.prob, prob.hat, xlab = "Probabilidades empiricas",
       ylab = "Probabilidades ajustadas", xlim = c(0, 1),
       ylim = c(0, 1), col=colors[5])
  abline(0, 1, col = "grey")
}

ppgpd(data$Severidad, u, fit.gp$estimate[1], fit.gp$estimate[2]) 


#----------------- DISTRIBUCION DE PERDIDAS AGREGADAS ----------------------

#N- sigue una Poisson y X una Weibull
# Severidad
parsev <- c(fit.wei$estimate[1], fit.wei$estimate[2]); parfreq <- fit.pois$estimate #Fijamos parametros v.a. severidad y frecuencia

meansev <- mweibull(1, parsev[1], parsev[2]) #Momento de orden 1 lognormal
varsev <- mweibull(2, parsev[1], parsev[2]) - meansev^2 #Momento de orden 2 lognormal
skewsev <- (mweibull(3, parsev[1], parsev[2]) -
              3*meansev*varsev - meansev^3)/varsev^(3/2) #Coef.Asimetria lognormal
# Frecuencia
meanfreq <- varfreq <- parfreq[1]; skewfreq <- 1/sqrt(parfreq[1]) #Momento de orden 1 Poisson

# V. Agregada
meanagg <- meanfreq * meansev # Momento 1 Variable agregeda
varagg <- varfreq * (varsev + meansev^2) # Varianza v. agregada
skewagg <- (skewfreq*varfreq^(3/2)*meansev^3 + 3*varfreq*meansev*
              varsev + meanfreq*skewsev*varsev^(3/2))/varagg^(3/2) # Coef.asimetria agre

#Simulación
fsimul <- aggregateDist("simulation", model.freq = expression(y =rpois(parfreq)),
                        model.sev = expression(y =rweibull(parsev[1], parsev[2])),
                        nb.simul = 1000)

#Normal
fnormal <- aggregateDist("normal", moments = c(meanagg, varagg))

#Normal-Power
fnpower <- aggregateDist("npower", moments = c(meanagg, varagg, skewagg))

x <- seq(0,500) #Cambiar a 0,40

plot(x, fsimul(x), type="l",
     main="Distribución Agregada de Pérdidas", ylab="F(x)", col="grey69", lwd=2)
lines(x, fnormal(x), lty=3,col = "palegreen3",lwd=2)
lines(x, fnpower(x), lty=4, col = "orchid3",lwd=2)
legend("bottomright", leg=c("Exacta", "Simulación",
                            "Aprox.normal", "Approx.NP"),
       col = c("grey69", "skyblue3", "palegreen3", "orchid3"),
       lty = 1:4, text.col = "black")

plot(fsimul)

####VAR y CVAR
alpha = 0.9
q = quantile(fsimul, alpha) #VaR

#Aproximacion Normal
print(paste0("VaR ",alpha*100,"%: ",(VaR.N <- meanagg + sqrt(varagg) * qnorm(alpha)))) # VaR_alpha(S)
print(paste0("CVaR ",alpha*100,"%: ",(ES.N <- meanagg + sqrt(varagg) * dnorm(qnorm(alpha)) / (1-alpha))))

a <- VaR.N
b <- ES.N
plot(fsimul)
abline(v=c(a,b), col=c(colors[4], colors[2]), lty=c(1,1), lwd=c(2, 2))
text(paste0("VaR ",alpha*100,"%: ",round(a,2)), x=a,y=0.5,pos = 2, srt = 90)
text(paste0("CVaR ",alpha*100,"%: ",round(b,2)), x=b,y=0.5,pos = 2, srt = 90)


#Aproximacion Gamma Trasladada
shape <- (2/skewagg)^2
rate <- sqrt(shape/varagg)
k <- meanagg - shape/rate
print(paste0("VaR ",alpha*100,"%: ",(VaR.tg <- k + qgamma(alpha, shape = shape, rate = rate)))) # VaR_alpha(S)
print(paste0("CVaR ",alpha*100,"%: ",(ES.tg <- k + ((shape/rate) / (1-alpha)) *
                             pgamma(qgamma(alpha, shape = shape, rate = rate),
                                    shape = shape + 1, rate = rate, lower.tail = FALSE)) ))
a <- VaR.tg
b <- ES.tg
plot(fsimul)
abline(v=c(a,b), col=c(colors[4], colors[1]), lty=c(1,1), lwd=c(2, 2))
text(paste0("VaR ",alpha*100,"%: ",round(a,2)), x=a,y=0.5,pos = 2, srt = 90)
text(paste0("CVaR ",alpha*100,"%: ",round(b,2)), x=b,y=0.5,pos = 2, srt = 90)


