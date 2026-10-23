library(quantmod)
library(readr)  
library(dplyr) 
library(modeest)
library(readxl)
library(lubridate)
library(tidyverse)


# =====================================================================
#  LOG en la consola de GitHub Actions
#  ---------------------------------------------------------------------
#  R Markdown se queda con todo lo que se imprime con print()/cat() dentro
#  del documento, por eso no se ve en el log de Actions. Escribiendo en
#  stderr() si aparece, en vivo.
# =====================================================================
log_consola <- function(...) {
  cat(format(with_tz(Sys.time(), "America/Argentina/Buenos_Aires"), "%H:%M:%S"), "|", ..., "\n",
      file = stderr())
}

# Progreso con contador y tiempo estimado: [ 123/838 ] AAPL - Apple Inc. | faltan 715 | ~12 min
log_progreso <- function(i, n, inicio, texto) {
  transcurrido <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))
  restante <- if (i > 1) transcurrido / (i - 1) * (n - i + 1) else NA
  eta <- if (is.na(restante)) "calculando..." else if (restante < 60) sprintf("~%.0f seg", restante)
         else sprintf("~%.0f min", restante / 60)
  log_consola(sprintf("[ %*d/%d ] %s | faltan %d | %s", nchar(n), i, n, texto, n - i, eta))
}




fecha_hoy <- Sys.Date()
today <- as.Date(format(with_tz(fecha_hoy, tz = "America/Argentina/Buenos_Aires"), "%Y-%m-%d"))

# =====================================================================
#  Descarga desde Yahoo con reintentos
#  ---------------------------------------------------------------------
#  Con ~840 tickers, Yahoo a veces corta por exceso de pedidos ("Too Many
#  Requests" / 429) o falla la conexion un instante. En vez de perder ese
#  ticker, se reintenta hasta 3 veces esperando cada vez mas (5 s y 15 s).
#  Si despues del tercer intento sigue fallando, recien ahi va a la lista
#  de "Tickers con error".
# =====================================================================
PAUSA_ENTRE_TICKERS <- 0.5          # segundos entre una descarga y la siguiente
ESPERAS_REINTENTO   <- c(5, 15)     # segundos antes del 2do y del 3er intento

descargarYahoo = function(accion) {
  intentos <- length(ESPERAS_REINTENTO) + 1
  for (i in seq_len(intentos)) {
    resultado <- tryCatch(
      getSymbols(
        accion,
        src = 'yahoo',
        auto.assign = F,
        from = "2002-01-01",
        to = Sys.Date(),
        periodicity = "daily"
      ),
      error = function(e) e
    )
    if (!inherits(resultado, "error")) return(resultado)
    if (i < intentos) {
      log_consola("    reintento", i, "de", accion, "en", ESPERAS_REINTENTO[i], "s:",
                  conditionMessage(resultado))
      Sys.sleep(ESPERAS_REINTENTO[i])
    }
  }
  stop(conditionMessage(resultado))
}

getRawData = function(accion){

  raw_data = as.data.frame(descargarYahoo(accion))
  
  raw_data$Fecha <- as.Date(row.names(raw_data))
  
  colnames(raw_data) = c("Open", "High", "Low", "Close", "Volumen", "Ajustado", "Fecha")
  
  raw_data = raw_data %>% select("Ajustado", "Fecha", "Volumen")
  
  rownames(raw_data) = 1:nrow(raw_data)
  
  colnames(raw_data) = c("precio","fecha", "volumen")

  raw_data = na.omit(raw_data)

  # Si el workflow corre con Wall Street abierto, Yahoo devuelve la vela del
  # dia INCOMPLETA (precio y volumen a medias). Se descarta hasta el cierre.
  ahora_ny = with_tz(Sys.time(), "America/New_York")
  hoy_ny = as.Date(format(ahora_ny, "%Y-%m-%d"))
  minutos_ny = hour(ahora_ny) * 60 + minute(ahora_ny)
  if (nrow(raw_data) > 0 && max(raw_data$fecha) >= hoy_ny && minutos_ny < 16 * 60 + 30) {
    raw_data = raw_data[raw_data$fecha < hoy_ny, ]
  }

  return(raw_data)
}



getOrder = function(data){
  return(data[order(data$fecha), ])
}



getVariation = function(data) {
  ultimo_precio <- data$precio[nrow(data)]
  anteultimo_precio <- data$precio[nrow(data) - 1]
  variacion <- round(((ultimo_precio - anteultimo_precio) / anteultimo_precio) * 100,2)
  return(variacion)
}


# Variacion contra el precio de hace 1 ano CALENDARIO (365 dias, no 365 ruedas).
# Toma el ultimo cierre disponible en o antes de esa fecha. Si la accion cotiza
# hace menos de 1 ano (salida a bolsa reciente), se calcula desde su PRIMER
# precio disponible (ver columna FechaInicio).
getVariationInterAnual = function(data) {
  ultimo_precio <- data$precio[nrow(data)]
  fecha_ref <- max(data$fecha) - 365
  idx <- which(data$fecha <= fecha_ref)
  precio_ref <- if (length(idx) == 0) data$precio[1] else data$precio[max(idx)]
  variacion <- round(((ultimo_precio - precio_ref) / precio_ref) * 100,2)
  return(variacion)
}

getFechaUltimoMaximo = function(data){
  fila_max_precio <- which.max(data$precio)
  fecha_max_precio <- data$fecha[fila_max_precio]
  return(fecha_max_precio)
}



getDiasUtimoMaximo = function(fecha_ultimo_maximo, fecha_hoy) {
  return(as.integer(fecha_hoy - fecha_ultimo_maximo))
}


# Cuanto esta el precio DEBAJO de su maximo historico (0 = en maximos, -40 = 40% abajo).
getVariationUltimoMaximo = function(data) {
  precio_actual = data$precio[nrow(data)]
  ultimo_maximo = max(data$precio)
  variacion = round((precio_actual / ultimo_maximo - 1) * 100,2)
  return(variacion)
}


# Cuanto esta el precio POR ENCIMA de su minimo de las ultimas 52 semanas (252 ruedas).
getVariationUltimoMinimo = function(data) {
  precio_actual = data$precio[nrow(data)]
  minimo_52s = min(tail(data$precio, 252))
  variacion = round((precio_actual / minimo_52s - 1) * 100,2)
  return(variacion)
}


getUltVolumen = function(data) {
  volumen_actual = data$volumen[nrow(data)]
  return(volumen_actual)
}

getCierre = function(data) {
  ultimo_precio <- round(tail(data$precio, 1),2)
  return(ultimo_precio)
}

getUltimoPrecioMaximo = function(data) {
  precio_maximo <- round(max(data$precio),2)
  return(precio_maximo)
}



# Promedio del coeficiente de variacion (desvio / media * 100) del precio en
# 4 tramos SIN superposicion, contados en ruedas hacia atras:
# ultimas 60, de la 61 a la 90, de la 91 a la 150 y de la 151 a la 252 (1 ano).
# Si no hay historia para un tramo, se promedian los que existen (NA si ninguno).
getAnualVariationCoeff = function(data) {
  p <- data$precio
  n <- length(p)
  tramos <- list(c(1, 60), c(61, 90), c(91, 150), c(151, 252))
  cvs <- sapply(tramos, function(t) {
    if (n < t[2]) return(NA_real_)
    x <- p[(n - t[2] + 1):(n - t[1] + 1)]
    sd(x) / mean(x) * 100
  })
  if (all(is.na(cvs))) return(NA_real_)
  return(round(mean(cvs, na.rm = TRUE), 2))
}

# =====================================================================
#  getMedias200(): % contra la media de 200, Tendencia y Recomendacion
#  ---------------------------------------------------------------------
#  Usa el precio AJUSTADO ('precio' en getRawData) y medias moviles
#  SIMPLES de 50 y 200 ruedas (las mismas que se ven en TradingView en
#  velas diarias con "SMA 50" y "SMA 200").
#
#  PctMedia200 = (precio / SMA200 - 1) * 100
#  PendMedia200 = variacion % de la SMA200 en las ultimas 20 ruedas
#                 (> 0: la tendencia de fondo sube; < 0: se esta girando)
#
#  Tendencia (alineacion de medias):
#    1 - Alcista fuerte     : precio > SMA50 > SMA200
#    2 - Retroceso en alza  : SMA50 > SMA200 y precio <= SMA50
#    3 - Recuperacion       : precio > SMA50 y SMA50 <= SMA200
#    4 - Bajista            : precio <= SMA50 y SMA50 <= SMA200
#
#  Recomendacion (combina Tendencia + PctMedia200 + PendMedia200):
#    1 - Prioridad      : Retroceso en alza, PctMedia200 <= 10 y SMA200 subiendo
#    2 - Normal         : Alcista fuerte con PctMedia200 <= 10, Recuperacion,
#                         o Retroceso en alza con la SMA200 bajando
#    3 - Mitad          : PctMedia200 entre 10 y 20 (estirada)
#    4 - Esperar        : PctMedia200 > 20 (muy estirada, esperar retroceso)
#    5 - De a poco      : Bajista con PctMedia200 > -30 (escalera, revisar negocio)
#    6 - No promediar   : Bajista con PctMedia200 <= -30 (esperar senal de despegue)
#
#  Historia corta (salidas a bolsa recientes, ej. SPCX): si hay entre 50 y 199
#  ruedas, la "SMA200" se calcula con TODOS los precios desde la primera
#  cotizacion (y PendMedia200 queda vacia, por eso nunca da "1 - Prioridad").
#  Con menos de 50 ruedas: "Sin datos".
# =====================================================================
getMedias200 <- function(data, dias_pend = 20) {
  vacio <- data.frame(SMA50 = NA_real_, SMA200 = NA_real_, PctMedia200 = NA_real_,
                      PendMedia200 = NA_real_, Tendencia = "Sin datos",
                      Recomendacion = "Sin datos")
  precios <- data$precio[is.finite(data$precio)]
  n <- length(precios)
  if (n < 50) return(vacio)

  precio <- precios[n]
  sma50  <- mean(precios[(n - 49):n])
  sma200 <- mean(precios[max(1, n - 199):n])
  pct200 <- (precio / sma200 - 1) * 100

  pend200 <- NA_real_
  if (n >= 200 + dias_pend) {
    sma200_antes <- mean(precios[(n - 199 - dias_pend):(n - dias_pend)])
    pend200 <- (sma200 / sma200_antes - 1) * 100
  }

  tendencia <- if (sma50 > sma200) {
    if (precio > sma50) "1 - Alcista fuerte" else "2 - Retroceso en alza"
  } else {
    if (precio > sma50) "3 - Recuperacion" else "4 - Bajista"
  }

  recomendacion <- if (tendencia == "4 - Bajista") {
    if (pct200 <= -30) "6 - No promediar" else "5 - De a poco"
  } else if (pct200 > 20) {
    "4 - Esperar"
  } else if (pct200 > 10) {
    "3 - Mitad"
  } else if (tendencia == "2 - Retroceso en alza" && isTRUE(pend200 >= 0)) {
    "1 - Prioridad"
  } else {
    "2 - Normal"
  }

  data.frame(SMA50 = round(sma50, 2), SMA200 = round(sma200, 2),
             PctMedia200 = round(pct200, 1), PendMedia200 = round(pend200, 2),
             Tendencia = tendencia, Recomendacion = recomendacion)
}

# =====================================================================
#  getSenalGiro(): detecta si una accion castigada empieza a darse vuelta
#  ---------------------------------------------------------------------
#  Solo aplica a Tendencia "4 - Bajista" o "3 - Recuperacion" (en el resto
#  devuelve "No aplica"). Suma 1 punto por cada condicion que se cumple:
#    1. Minimos mas altos : minimo de las ultimas 20 ruedas > minimo de las 20 anteriores
#    2. SMA50 frena       : SMA50 de hoy >= SMA50 de hace 10 ruedas (dejo de caer)
#    3. Fuerza + volumen  : IndiceFuerza30 > 0 y FuerzaVol30 > 0 (entra dinero comprando)
#    4. Sobre la SMA50    : precio > SMA50 (primer cruce alcista)
#  SenalGiro:
#    "Giro confirmado" : condicion 4 + al menos 3 puntos
#    "Giro temprano"   : 2 puntos o mas (sin confirmar)
#    "Sin senal"       : 0 o 1 punto (sigue cayendo)
# =====================================================================
getSenalGiro <- function(data, tendencia, det30) {
  if (!tendencia %in% c("3 - Recuperacion", "4 - Bajista")) {
    return(data.frame(SenalGiro = "No aplica", PuntosGiro = NA_integer_))
  }
  p <- data$precio[is.finite(data$precio)]
  n <- length(p)
  if (n < 60) {
    return(data.frame(SenalGiro = "Sin datos", PuntosGiro = NA_integer_))
  }
  sma50_hoy   <- mean(p[(n - 49):n])
  sma50_antes <- mean(p[(n - 59):(n - 10)])
  c1 <- min(p[(n - 19):n]) > min(p[(n - 39):(n - 20)])
  c2 <- sma50_hoy >= sma50_antes
  c3 <- isTRUE(det30$indice_total > 0 && det30$aporte_vol > 0)
  c4 <- p[n] > sma50_hoy
  puntos <- as.integer(c1 + c2 + c3 + c4)
  senal <- if (c4 && puntos >= 3) "Giro confirmado" else if (puntos >= 2) "Giro temprano" else "Sin senal"
  data.frame(SenalGiro = senal, PuntosGiro = puntos)
}


# Ajusta la Recomendacion segun la senal de giro:
#   - Giro confirmado (y no mas de +10% sobre la SMA200) -> "2 - Normal (giro)"
#   - Giro temprano en una accion "6 - No promediar"     -> "5 - De a poco"
ajustarRecomendacionGiro <- function(recomendacion, senal, pct200) {
  if (senal == "Giro confirmado" && isTRUE(pct200 <= 10)) return("2 - Normal (giro)")
  if (senal == "Giro temprano" && recomendacion == "6 - No promediar") return("5 - De a poco")
  recomendacion
}


getVolumenTrendYear = function(data) {
  data <- tail(data, 365)
  model <- lm(volumen ~ fecha, data = data)
  pendiente <- round(coef(model)[2],2)
  return(pendiente)
}

getVolumenTrend90Days = function(data) {
  data <- tail(data, 90)
  model <- lm(volumen ~ fecha, data = data)
  pendiente <- round(coef(model)[2],2)
  return(pendiente)
}


# =====================================================================
#  getIndiceFuerzaDetalle(): fuerza relativa Precio + Volumen, POR ACCION
#  ---------------------------------------------------------------------
#  Se calcula desde el 'data' que ya esta en memoria (mismo criterio que
#  getVolumenTrend90Days: tail(data, dias) = ultimas N ruedas).
#  NO vuelve a descargar de Yahoo -> cero descargas extra en el loop.
#
#  indice = t_precio + w * signo(t_precio) * t_volumen
#    t_precio  = t-valor de la pendiente de lm(log(precio)  ~ tiempo)
#    t_volumen = t-valor de la pendiente de lm(log(volumen) ~ tiempo)
#
#  Devuelve un data.frame de 1 fila con la DESCOMPOSICION:
#    t_precio     -> participacion del precio (aporte al indice)
#    aporte_vol   -> participacion del volumen (= w * signo(t_precio) * t_volumen)
#    indice_total -> t_precio + aporte_vol  (los dos aportes SUMAN el indice)
#
#  Como todas las acciones usan la misma cantidad de ruedas (dias), los
#  t-valores (adimensionales) son directamente comparables entre acciones.
#
#  Mas alto -> mas traccion Precio+Volumen al ALZA (mayor fuerza relativa)
#  Mas bajo -> mas traccion Precio+Volumen a la BAJA
# =====================================================================
getIndiceFuerzaDetalle <- function(data, dias = 90, vol_weight = 0.5, min_obs = 10) {
  data <- tail(data, dias)
  
  close  <- data$precio     # 'precio' ya es el Ajustado en getRawData
  volume <- data$volumen
  
  ok     <- is.finite(close) & is.finite(volume) & close > 0 & volume > 0
  close  <- close[ok]
  volume <- volume[ok]
  n      <- length(close)
  if (n < min_obs) {
    return(data.frame(t_precio = NA_real_,
                      aporte_vol = NA_real_,
                      indice_total = NA_real_))
  }
  
  tt <- seq_len(n)   # eje temporal (indice de ruedas)
  
  # t-valor de la pendiente; safe_tval evita NaN si la serie es constante
  safe_tval <- function(y) {
    out <- tryCatch(summary(lm(y ~ tt))$coefficients["tt", "t value"],
                    error = function(e) NA_real_)
    if (!is.finite(out)) 0 else out
  }
  
  t_precio   <- safe_tval(log(close))
  t_volumen  <- safe_tval(log(volume))
  aporte_vol <- vol_weight * sign(t_precio) * t_volumen
  
  data.frame(
    t_precio     = round(t_precio, 3),
    aporte_vol   = round(aporte_vol, 3),
    indice_total = round(t_precio + aporte_vol, 3)
  )
}


# Wrapper: devuelve solo el numero del indice (usa la misma cuenta de arriba)
getIndiceFuerza <- function(data, dias = 90, vol_weight = 0.5, min_obs = 10) {
  getIndiceFuerzaDetalle(data, dias, vol_weight, min_obs)$indice_total
}


# Volumen promedio del ultimo mes (21 ruedas).
getVolumenPromedioMensual = function(data) {
  data <- tail(data, 21)
  promedio <- mean(data$volumen)
  promedio <- round(promedio,2)
  return(promedio)
}



# Correlacion de los RETORNOS diarios (log) de la accion contra el S&P 500,
# alineados POR FECHA, en las ultimas ~3 anos (750 ruedas en comun).
# (Correlacionar precios da valores enganosos: dos cosas que suben "correlacionan".)
# NA si hay menos de 120 ruedas en comun.
getCorr = function(data_uno, data_dos, ruedas = 750, min_obs = 120){
  m <- merge(data_uno[, c("fecha", "precio")], data_dos[, c("fecha", "precio")],
             by = "fecha", suffixes = c("", "_sp"))
  m <- tail(m[order(m$fecha), ], ruedas + 1)
  if (nrow(m) < min_obs + 1) return(NA_real_)
  r1 <- diff(log(m$precio))
  r2 <- diff(log(m$precio_sp))
  return(round(cor(r1, r2), 2))
}







# % del precio actual contra el maximo de las ultimas 52 semanas (252 ruedas).
# 0 = en maximos del año; -20 = 20% abajo del pico del ultimo año.
getPctMax52 = function(data) {
  data <- getOrder(data)
  ult  <- tail(data$precio, 252)
  round((tail(ult, 1) / max(ult, na.rm = TRUE) - 1) * 100, 2)
}


# =====================================================================
#  INDICE DE RUPTURA (caida fuerte -> base -> descompresion)
#  ---------------------------------------------------------------------
#  Busca acciones que cayeron fuerte, armaron una base (el precio se
#  comprime en un rango mientras alguien acumula) y rompen el techo de esa
#  base con volumen. Un solo numero de 0 a 100: cuanto mas alto, mejor
#  momento para entrar.
#
#    0      No aplica: no cayo fuerte, sigue cayendo (piso hace < 30 ruedas)
#           o ya disparo (mas de 20% arriba del techo de la base: tarde para entrar)
#    1-39   Formando base: cayo 35%+ y dejo de hacer minimos, pero todavia
#           oscila mucho o no se ve acumulacion
#    40-69  Base lista: rango apretado (<= 25%), con acumulacion y el precio
#           en el tercio de arriba del rango
#    70-100 Ruptura: en las ultimas 10 ruedas cerro arriba del techo de la
#           base con volumen >= 1,5 veces el promedio, y sigue arriba
#
#  Dentro de cada fase suma calidad: caida previa mas profunda, base mas
#  apretada, mas acumulacion (volumen de dias de suba vs dias de baja),
#  volatilidad que se contrae y, en la ruptura, mas volumen y que sea
#  FRESCA (una ruptura vieja o que ya se escapo mucho del techo puntua menos).
# =====================================================================
entre01 <- function(x) { x <- ifelse(is.na(x) | is.nan(x), 0, x); pmin(pmax(x, 0), 1) }

getIndiceRuptura = function(data) {
  data <- getOrder(data)
  if (nrow(data) < 60) return(0)
  w <- tail(data, 252)                          # ultimo año (o desde que cotiza)
  p <- w$precio; v <- w$volumen; m <- length(p)
  v[is.na(v)] <- 0

  # 1) Caida fuerte: del pico (antes del piso) al piso
  i_min <- which.min(p)
  if (i_min < 2) return(0)                      # el piso es el primer dia: no hubo caida
  caida <- 1 - p[i_min] / max(p[1:i_min])
  if (caida < 0.35) return(0)
  if (m - i_min < 30) return(0)                 # piso muy reciente: sigue cayendo

  # 2) Base: desde el piso (como mucho las ultimas 60 ruedas), sin las ultimas 10
  base_ini <- max(i_min, m - 70)
  base_fin <- m - 10
  if (base_fin - base_ini < 15) return(0)
  techo <- max(p[base_ini:base_fin]); piso <- min(p[base_ini:base_fin])
  ancho <- techo / piso - 1

  # Acumulacion: volumen promedio de los dias de suba / de los dias de baja (desde el piso)
  ret_b <- diff(p[i_min:m]); vol_b <- v[(i_min + 1):m]
  acum <- if (sum(ret_b > 0) > 0 && sum(ret_b < 0) > 0 && mean(vol_b[ret_b < 0]) > 0)
            mean(vol_b[ret_b > 0]) / mean(vol_b[ret_b < 0]) else NA

  # Contraccion de la volatilidad: ultimas 20 ruedas vs la caida
  r <- diff(log(p))
  ini_caida <- which.max(p[1:i_min])
  vol_caida <- if (i_min - ini_caida >= 5) sd(r[ini_caida:(i_min - 1)]) else NA
  contraccion <- 1 - sd(tail(r, 20)) / vol_caida   # 0.5 = la volatilidad bajo a la mitad

  q_caida   <- entre01((caida - 0.35) / 0.25)       # 35% -> 0, 60%+ -> 1
  q_ancho   <- entre01((0.25 - ancho) / 0.15)       # 25% -> 0, 10% o menos -> 1
  q_acum    <- entre01((acum - 1) / 0.5)            # 1,0 -> 0, 1,5+ -> 1
  q_contrac <- entre01(contraccion / 0.5)

  # 3) Ruptura en las ultimas 10 ruedas: cierre arriba del techo con volumen
  k_rup <- NA; vol_rup <- NA
  for (k in (m - 9):m) {
    prom50 <- mean(v[max(1, k - 50):(k - 1)])
    if (p[k] > techo && prom50 > 0 && v[k] >= 1.5 * prom50) { k_rup <- k; vol_rup <- v[k] / prom50; break }
  }
  if (!is.na(k_rup) && p[m] > techo) {
    extension <- p[m] / techo - 1
    if (extension > 0.20) return(0)                               # ya disparo: tarde para entrar
    q_vol    <- entre01((vol_rup - 1.5) / 1.5)                    # 1,5x -> 0, 3x+ -> 1
    q_fresca <- entre01(1 - (m - k_rup) / 10) *                   # hoy -> 1, hace 10 ruedas -> 0
                entre01(1 - (extension - 0.05) / 0.15)            # hasta 5% arriba -> 1, 20% -> 0
    calidad <- 0.35 * q_vol + 0.25 * q_ancho + 0.20 * q_acum + 0.20 * q_caida
    # La frescura multiplica: una ruptura vieja o alejada del techo queda cerca de 70
    return(round(70 + 30 * (0.25 + 0.75 * q_fresca) * calidad))
  }

  # Base lista: apretada, con acumulacion, precio en el tercio de arriba
  if (ancho <= 0.25 && !is.na(acum) && acum >= 1.1 && p[m] >= piso + 2/3 * (techo - piso)) {
    return(round(40 + 29 * (0.35 * q_ancho + 0.30 * q_acum + 0.20 * q_contrac + 0.15 * q_caida)))
  }

  # Formando base
  round(1 + 38 * (0.30 * q_ancho + 0.30 * q_acum + 0.20 * q_contrac + 0.20 * q_caida))
}


getData = function(data, especie, accion_completa, sp500) {

  data <- getOrder(data)

  fecha_ultimo_maximo <- getFechaUltimoMaximo(data)

  # % contra la media de 200, Tendencia y Recomendacion
  med <- getMedias200(data)

  # Descomposicion del indice de fuerza para cada ventana (una sola regresion c/u)
  det7   <- getIndiceFuerzaDetalle(data, 7, min_obs = 5)
  det30  <- getIndiceFuerzaDetalle(data, 30)
  det90  <- getIndiceFuerzaDetalle(data, 90)
  det365 <- getIndiceFuerzaDetalle(data, 365)

  # Senal de giro (solo para tendencia Bajista o Recuperacion) y ajuste de la recomendacion
  giro <- getSenalGiro(data, med$Tendencia, det30)
  reco <- ajustarRecomendacionGiro(med$Recomendacion, giro$SenalGiro, med$PctMedia200)
  
  df <- data.frame(
    Especie = especie,
    Nombre = accion_completa,
    Cierre = getCierre(data),
    UltimoPrecioMaximo = getUltimoPrecioMaximo(data),
    Variacion = getVariation(data),
    Volumen = getUltVolumen(data),
    InterAnual = getVariationInterAnual(data),
    PorcentajeRespectoAlMax = getVariationUltimoMaximo(data),
    PorcentajeRespectoAlMin = getVariationUltimoMinimo(data),
    PctMax52 = getPctMax52(data),
    DiasUltMax = getDiasUtimoMaximo(
      fecha_ultimo_maximo, today
    ),
    FechaUltMax = fecha_ultimo_maximo,
    FechaInicio = min(data$fecha),
    PctMedia200 = med$PctMedia200,
    PendMedia200 = med$PendMedia200,
    Tendencia = med$Tendencia,
    Recomendacion = reco,
    SenalGiro = giro$SenalGiro,
    PuntosGiro = giro$PuntosGiro,
    IndiceRuptura = tryCatch(getIndiceRuptura(data), error = function(e) 0),
    SMA50 = med$SMA50,
    SMA200 = med$SMA200,
    CoefVariationPromedioAnual = getAnualVariationCoeff(data),
    VolumenPromedioMensual = getVolumenPromedioMensual(data),
    VolumenTrendYear = getVolumenTrendYear(data),
    VolumenTrend90Days = getVolumenTrend90Days(data),
    CorrSp500 = getCorr(data, sp500),
    IndiceFuerza7   = det7$indice_total,
    FuerzaPrecio7   = det7$t_precio,
    FuerzaVol7      = det7$aporte_vol,
    IndiceFuerza30  = det30$indice_total,
    FuerzaPrecio30  = det30$t_precio,
    FuerzaVol30     = det30$aporte_vol,
    IndiceFuerza90  = det90$indice_total,
    FuerzaPrecio90  = det90$t_precio,
    FuerzaVol90     = det90$aporte_vol,
    IndiceFuerza365 = det365$indice_total,
    FuerzaPrecio365 = det365$t_precio,
    FuerzaVol365    = det365$aporte_vol
    
  )
  
  rownames(df) = 1
  
  return(df)
}




datos <- list(
  QNT = "Quantinuum",
  XP = "XP Inc.",
  QUBT = "Quantum Computing Inc.",
  BBAI = "BigBear.ai Holdings, Inc.",
  RDW = "Redwire Corporation",
  PL = "Planet Labs PBC",
  NNE = "NANO Nuclear Energy Inc.",
  ONON = "On Holding AG",
  CAVA = "CAVA Group, Inc.",
  CELH = "Celsius Holdings, Inc.",
  ONDS = "Ondas Holdings Inc.",
  SATL = "Satellogic Inc.",
  IREN = "IREN Limited",
  BMNR = "BitMine Immersion Technologies, Inc.",
  CLS = "Celestica Inc.",
  NTRA = "Natera, Inc.",
  BB = "BlackBerry Limited",
  FNMA = "Federal National Mortgage Association (Fannie Mae)",
  FMCC = "Federal Home Loan Mortgage Corporation (Freddie Mac)",
  AGRO = "Adecoagro S.A.",
  LAR = "Lithium Argentina AG",
  LND = "BrasilAgro - Companhia Brasileira de Propriedades Agricolas",
  GPRK = "GeoPark Limited",
  MUX = "McEwen Inc.",
  CDE = "Coeur Mining, Inc.",
  HMY = "Harmony Gold Mining Company Limited",
  NG = "NovaGold Resources Inc.",
  GLNG = "Golar LNG Limited",
  JMIA = "Jumia Technologies AG",
  JOYY = "JOYY Inc.",
  VIV = "Telefonica Brasil S.A.",
  TIMB = "TIM S.A.",
  SBS = "Companhia de Saneamento Basico do Estado de Sao Paulo",
  ELP = "Companhia Paranaense de Energia - COPEL",
  EBR = "Centrais Eletricas Brasileiras S.A. - Eletrobras",
  GGB = "Gerdau S.A.",
  SID = "Companhia Siderurgica Nacional",
  ERJ = "Embraer S.A.",
  BRFS = "BRF S.A.",
  UGP = "Ultrapar Participacoes S.A.",
  SUZ = "Suzano S.A.",
  PAGS = "PagSeguro Digital Ltd.",
  STNE = "StoneCo Ltd.",
  BSBR = "Banco Santander (Brasil) S.A.",
  BAK = "Braskem S.A.",
  `AKO-B` = "Embotelladora Andina S.A.",
  CX = "CEMEX, S.A.B. de C.V.",
  FMX = "Fomento Economico Mexicano, S.A.B. de C.V. (FEMSA)",
  AMX = "America Movil, S.A.B. de C.V.",
  TV = "Grupo Televisa, S.A.B.",
  ASR = "Grupo Aeroportuario del Sureste, S.A.B. de C.V.",
  PAC = "Grupo Aeroportuario del Pacifico, S.A.B. de C.V.",
  HDB = "HDFC Bank Limited",
  KB = "KB Financial Group Inc.",
  KEP = "Korea Electric Power Corporation",
  MUFG = "Mitsubishi UFJ Financial Group, Inc.",
  MFG = "Mizuho Financial Group, Inc.",
  HMC = "Honda Motor Co., Ltd.",
  NTES = "NetEase, Inc.",
  TCOM = "Trip.com Group Limited",
  E = "Eni S.p.A.",
  AEG = "Aegon Ltd.",
  ING = "ING Groep N.V.",
  NGG = "National Grid plc",
  PHG = "Koninklijke Philips N.V.",
  STLA = "Stellantis N.V.",
  PSO = "Pearson plc",
  GT = "The Goodyear Tire & Rubber Company",
  HOG = "Harley-Davidson, Inc.",
  AAP = "Advance Auto Parts, Inc.",
  ANF = "Abercrombie & Fitch Co.",
  URBN = "Urban Outfitters, Inc.",
  ETSY = "Etsy, Inc.",
  YELP = "Yelp Inc.",
  TRIP = "Tripadvisor, Inc.",
  CAR = "Avis Budget Group, Inc.",
  PBI = "Pitney Bowes Inc.",
  ACWI = "iShares MSCI ACWI ETF",
  EFA = "iShares MSCI EAFE ETF",
  VEA = "Vanguard FTSE Developed Markets ETF",
  IEMG = "iShares Core MSCI Emerging Markets ETF",
  IEUR = "iShares Core MSCI Europe ETF",
  EWJ = "iShares MSCI Japan ETF",
  EWY = "iShares MSCI South Korea ETF",
  FXI = "iShares China Large-Cap ETF",
  ILF = "iShares Latin America 40 ETF",
  IJH = "iShares Core S&P Mid-Cap ETF",
  IVE = "iShares S&P 500 Value ETF",
  IVW = "iShares S&P 500 Growth ETF",
  RSP = "Invesco S&P 500 Equal Weight ETF",
  VIG = "Vanguard Dividend Appreciation ETF",
  SPHQ = "Invesco S&P 500 Quality ETF",
  ESGU = "iShares ESG Aware MSCI USA ETF",
  XLB = "Materials Select Sector SPDR",
  XLC = "Communication Services Select Sector SPDR",
  XLRE = "Real Estate Select Sector SPDR",
  XME = "SPDR S&P Metals & Mining ETF",
  COPX = "Global X Copper Miners ETF",
  GDX = "VanEck Gold Miners ETF",
  IBB = "iShares Biotechnology ETF",
  ICLN = "iShares Global Clean Energy ETF",
  CIBR = "First Trust NASDAQ Cybersecurity ETF",
  USO = "United States Oil Fund",
  CORN = "Teucrium Corn Fund",
  SOYB = "Teucrium Soybean Fund",
  GSG = "iShares S&P GSCI Commodity-Indexed Trust",
  AOS = "A. O. Smith",
  AES = "AES Corporation",
  AFL = "Aflac",
  A = "Agilent Technologies",
  APD = "Air Products",
  AKAM = "Akamai Technologies",
  ARE = "Alexandria Real Estate Equities",
  ALGN = "Align Technology",
  ALLE = "Allegion",
  LNT = "Alliant Energy",
  ALL = "Allstate",
  AEE = "Ameren",
  AEP = "American Electric Power",
  AIG = "American International Group",
  AWK = "American Water Works",
  AMP = "Ameriprise Financial",
  AME = "Ametek",
  APH = "Amphenol",
  AON = "Aon plc",
  APA = "APA Corporation",
  APTV = "Aptiv",
  ACGL = "Arch Capital Group",
  ADM = "Archer Daniels Midland",
  ARES = "Ares Management",
  AJG = "Arthur J. Gallagher & Co.",
  AIZ = "Assurant",
  ATO = "Atmos Energy",
  ADSK = "Autodesk",
  ADP = "Automatic Data Processing",
  AZO = "AutoZone",
  AVY = "Avery Dennison",
  BALL = "Ball Corporation",
  BAX = "Baxter International",
  BBY = "Best Buy",
  TECH = "Bio-Techne",
  BE = "Bloom Energy",
  BNY = "BNY Mellon",
  BR = "Broadridge Financial Solutions",
  BRO = "Brown & Brown",
  `BF-B` = "Brown–Forman",
  BG = "Bunge Global",
  BXP = "BXP, Inc.",
  CHRW = "C.H. Robinson",
  CPT = "Camden Property Trust",
  CAH = "Cardinal Health",
  CARR = "Carrier Global",
  CASY = "Casey's",
  CBOE = "Cboe Global Markets",
  CBRE = "CBRE Group",
  CDW = "CDW Corporation",
  COR = "Cencora",
  CNC = "Centene Corporation",
  CNP = "CenterPoint Energy",
  CF = "CF Industries",
  CRL = "Charles River Laboratories",
  CHD = "Church & Dwight",
  CINF = "Cincinnati Financial",
  CTAS = "Cintas",
  CLX = "Clorox",
  CMS = "CMS Energy",
  FIX = "Comfort Systems USA",
  ED = "Consolidated Edison",
  STZ = "Constellation Brands",
  COO = "Cooper Companies (The)",
  CPRT = "Copart",
  GLW = "Corning Inc.",
  CPAY = "Corpay",
  CSGP = "CoStar Group",
  CRH = "CRH plc",
  CCI = "Crown Castle",
  CMI = "Cummins",
  DRI = "Darden Restaurants",
  DVA = "DaVita",
  DECK = "Deckers Brands",
  FANG = "Diamondback Energy",
  DG = "Dollar General",
  DLTR = "Dollar Tree",
  D = "Dominion Energy",
  DPZ = "Domino's",
  DOV = "Dover Corporation",
  DOW = "Dow Inc.",
  DHI = "D. R. Horton",
  DTE = "DTE Energy",
  DD = "DuPont",
  ECHO = "EchoStar",
  ECL = "Ecolab",
  EIX = "Edison International",
  EME = "Emcor",
  EMR = "Emerson Electric",
  ETR = "Entergy",
  EQT = "EQT Corporation",
  EFX = "Equifax",
  ERIE = "Erie Indemnity",
  ESS = "Essex Property Trust",
  EL = "Estée Lauder Companies (The)",
  EG = "Everest Group",
  EVRG = "Evergy",
  P = "Everpure",
  ES = "Eversource Energy",
  EXC = "Exelon",
  EXPE = "Expedia Group",
  EXPD = "Expeditors International",
  EXR = "Extra Space Storage",
  FFIV = "F5, Inc.",
  FDS = "FactSet",
  FICO = "Fair Isaac",
  FAST = "Fastenal",
  FRT = "Federal Realty Investment Trust",
  FDXF = "FedEx Freight",
  FERG = "Ferguson Enterprises",
  FIS = "Fidelity National Information Services",
  FSLR = "First Solar",
  FE = "FirstEnergy",
  FISV = "Fiserv",
  FLEX = "Flex Ltd.",
  FTV = "Fortive",
  FOXA = "Fox Corporation (Class A)",
  FOX = "Fox Corporation (Class B)",
  BEN = "Franklin Resources",
  GRMN = "Garmin",
  IT = "Gartner",
  GEHC = "GE HealthCare",
  GEN = "Gen Digital",
  GNRC = "Generac",
  GIS = "General Mills",
  GPC = "Genuine Parts Company",
  GPN = "Global Payments",
  GL = "Globe Life",
  GDDY = "GoDaddy",
  HIG = "Hartford (The)",
  HAS = "Hasbro",
  HSIC = "Henry Schein",
  HLT = "Hilton Worldwide",
  HONA = "Honeywell Aerospace",
  HRL = "Hormel Foods",
  HUBB = "Hubbell Incorporated",
  HUM = "Humana",
  HII = "Huntington Ingalls Industries",
  IEX = "IDEX Corporation",
  IDXX = "Idexx Laboratories",
  ITW = "Illinois Tool Works",
  ILMN = "Illumina, Inc.",
  INCY = "Incyte",
  IR = "Ingersoll Rand",
  PODD = "Insulet Corporation",
  IBKR = "Interactive Brokers",
  IFF = "International Flavors & Fragrances",
  IP = "International Paper",
  IVZ = "Invesco",
  INVH = "Invitation Homes",
  IQV = "IQVIA",
  IRM = "Iron Mountain",
  JBHT = "J.B. Hunt",
  JBL = "Jabil",
  JKHY = "Jack Henry & Associates",
  J = "Jacobs Solutions",
  KEYS = "Keysight Technologies",
  KMB = "Kimberly-Clark",
  KHC = "Kraft Heinz",
  KR = "Kroger",
  LH = "Labcorp",
  LVS = "Las Vegas Sands",
  LDOS = "Leidos",
  LEN = "Lennar",
  LII = "Lennox International",
  LYV = "Live Nation Entertainment",
  L = "Loews Corporation",
  LYB = "LyondellBasell",
  MTB = "M&T Bank",
  MRSH = "Marsh McLennan",
  MLM = "Martin Marietta Materials",
  MAS = "Masco",
  MKC = "McCormick & Company",
  MET = "MetLife",
  MTD = "Mettler Toledo",
  MGM = "MGM Resorts",
  MAA = "Mid-America Apartment Communities",
  MSI = "Motorola Solutions",
  MSCI = "MSCI",
  NDAQ = "Nasdaq, Inc.",
  NTAP = "NetApp",
  NWSA = "News Corp (Class A)",
  NWS = "News Corp (Class B)",
  NI = "NiSource",
  NDSN = "Nordson Corporation",
  NSC = "Norfolk Southern",
  NTRS = "Northern Trust",
  NVR = "NVR, Inc.",
  ORLY = "O'Reilly Automotive",
  ODFL = "Old Dominion",
  OMC = "Omnicom Group",
  OKE = "Oneok",
  OTIS = "Otis Worldwide",
  PCAR = "Paccar",
  PKG = "Packaging Corporation of America",
  PH = "Parker Hannifin",
  PAYX = "Paychex",
  PNR = "Pentair",
  PNW = "Pinnacle West Capital",
  PNC = "PNC Financial Services",
  PPG = "PPG Industries",
  PPL = "PPL Corporation",
  PFG = "Principal Financial Group",
  PRU = "Prudential Financial",
  PEG = "Public Service Enterprise Group",
  PTC = "PTC Inc.",
  PSA = "Public Storage",
  PHM = "PulteGroup",
  DGX = "Quest Diagnostics",
  Q = "Qnity Electronics",
  RL = "Ralph Lauren Corporation",
  RJF = "Raymond James Financial",
  REG = "Regency Centers",
  RSG = "Republic Services",
  RMD = "ResMed|",
  RVTY = "Revvity",
  ROK = "Rockwell Automation",
  ROL = "Rollins, Inc.",
  ROP = "Roper Technologies",
  ROST = "Ross Stores",
  RCL = "Royal Caribbean Group",
  SBAC = "SBA Communications",
  SRE = "Sempra",
  SKYD = "Skydance Corporation",
  SWKS = "Skyworks Solutions",
  SJM = "J.M. Smucker Company (The)",
  SW = "Smurfit Westrock",
  SNA = "Snap-on",
  SOLV = "Solventum",
  SWK = "Stanley Black & Decker",
  STT = "State Street Corporation",
  STLD = "Steel Dynamics",
  STE = "Steris",
  SYY = "Sysco",
  TROW = "T. Rowe Price",
  TPR = "Tapestry, Inc.",
  TRGP = "Targa Resources",
  TEL = "TE Connectivity",
  TDY = "Teledyne Technologies",
  TPL = "Texas Pacific Land Corporation",
  TXT = "Textron",
  TKO = "TKO Group Holdings",
  TSCO = "Tractor Supply",
  TT = "Trane Technologies",
  TDG = "TransDigm Group",
  TRV = "Travelers Companies (The)",
  TRMB = "Trimble Inc.",
  TYL = "Tyler Technologies",
  TSN = "Tyson Foods",
  UDR = "UDR, Inc.",
  ULTA = "Ulta Beauty",
  URI = "United Rentals",
  UHS = "Universal Health Services",
  VEEV = "Veeva Systems",
  VTR = "Ventas",
  VLTO = "Veralto",
  VRSN = "Verisign",
  VRSK = "Verisk Analytics",
  VICI = "Vici Properties",
  VMRK = "Vivmark Residential",
  VMC = "Vulcan Materials Company",
  VYLR = "Vylor",
  WRB = "W. R. Berkley Corporation",
  GWW = "W. W. Grainger",
  WAB = "Wabtec",
  WAT = "Waters Corporation",
  WEC = "WEC Energy Group",
  WELL = "Welltower",
  WST = "West Pharmaceutical Services",
  WY = "Weyerhaeuser",
  WSM = "Williams-Sonoma, Inc.",
  WTW = "Willis Towers Watson",
  WYNN = "Wynn Resorts",
  XEL = "Xcel Energy",
  XYL = "Xylem Inc.",
  YUM = "Yum! Brands",
  ZBRA = "Zebra Technologies",
  ZBH = "Zimmer Biomet",
  SOXX = "iShares Semiconductor ETF",
  SMH = "VanEck Semiconductor ETF",
  DIA = "SPDR Dow Jones Industrial Average ETF",
  IWM = "iShares Russell 2000 ETF",
  VOO = "Vanguard S&P 500 ETF",
  XLK = "Technology Select Sector SPDR",
  XLF = "Financial Select Sector SPDR",
  XLE = "Energy Select Sector SPDR",
  XLI = "Industrial Select Sector SPDR",
  XLY = "Consumer Discretionary Select Sector SPDR",
  XLP = "Consumer Staples Select Sector SPDR",
  XLU = "Utilities Select Sector SPDR",
  ARKK = "ARK Innovation ETF",
  EEM = "iShares MSCI Emerging Markets ETF",
  TLT = "iShares 20+ Year Treasury Bond ETF",
  SLV = "iShares Silver Trust",
  URA = "Global X Uranium ETF",
  ITA = "iShares U.S. Aerospace & Defense ETF",
  MRNA = "Moderna, Inc.",
  SYK = "Stryker Corporation",
  BDX = "Becton, Dickinson and Company",
  ELV = "Elevance Health, Inc.",
  CI = "The Cigna Group",
  HCA = "HCA Healthcare, Inc.",
  MCK = "McKesson Corporation",
  ZTS = "Zoetis Inc.",
  BIIB = "Biogen Inc.",
  DXCM = "DexCom, Inc.",
  EW = "Edwards Lifesciences Corporation",
  HIMS = "Hims & Hers Health, Inc.",
  NVS = "Novartis AG",
  SNY = "Sanofi",
  GSK = "GSK plc",
  ACN = "Accenture plc",
  SAP = "SAP SE",
  WDAY = "Workday, Inc.",
  FTNT = "Fortinet, Inc.",
  ZM = "Zoom Communications, Inc.",
  TWLO = "Twilio Inc.",
  OKTA = "Okta, Inc.",
  TEAM = "Atlassian Corporation",
  NBIS = "Nebius Group N.V.",
  SOUN = "SoundHound AI, Inc.",
  QBTS = "D-Wave Quantum Inc.",
  CIEN = "Ciena Corporation",
  STX = "Seagate Technology Holdings plc",
  WDC = "Western Digital Corporation",
  SNDK = "Sandisk Corporation",
  NXPI = "NXP Semiconductors N.V.",
  MCHP = "Microchip Technology Incorporated",
  MPWR = "Monolithic Power Systems, Inc.",
  STM = "STMicroelectronics N.V.",
  GFS = "GlobalFoundries Inc.",
  AMKR = "Amkor Technology, Inc.",
  LULU = "Lululemon Athletica Inc.",
  CMG = "Chipotle Mexican Grill, Inc.",
  BKNG = "Booking Holdings Inc.",
  MAR = "Marriott International, Inc.",
  TGT = "Target Corporation",
  LOW = "Lowe's Companies, Inc.",
  TJX = "The TJX Companies, Inc.",
  RACE = "Ferrari N.V.",
  ARCO = "Arcos Dorados Holdings Inc.",
  TM = "Toyota Motor Corporation",
  SONY = "Sony Group Corporation",
  DUOL = "Duolingo, Inc.",
  PM = "Philip Morris International Inc.",
  MDLZ = "Mondelez International, Inc.",
  MNST = "Monster Beverage Corporation",
  HSY = "The Hershey Company",
  BUD = "Anheuser-Busch InBev SA/NV",
  DEO = "Diageo plc",
  UL = "Unilever PLC",
  AXP = "American Express Company",
  COF = "Capital One Financial Corporation",
  SPGI = "S&P Global Inc.",
  MCO = "Moody's Corporation",
  ICE = "Intercontinental Exchange, Inc.",
  CME = "CME Group Inc.",
  BX = "Blackstone Inc.",
  KKR = "KKR & Co. Inc.",
  APO = "Apollo Global Management, Inc.",
  PGR = "The Progressive Corporation",
  CB = "Chubb Limited",
  HSBC = "HSBC Holdings plc",
  SAN = "Banco Santander, S.A.",
  BBVA = "Banco Bilbao Vizcaya Argentaria, S.A.",
  UBS = "UBS Group AG",
  GD = "General Dynamics Corporation",
  NOC = "Northrop Grumman Corporation",
  LHX = "L3Harris Technologies, Inc.",
  HWM = "Howmet Aerospace Inc.",
  AXON = "Axon Enterprise, Inc.",
  PWR = "Quanta Services, Inc.",
  FDX = "FedEx Corporation",
  UNP = "Union Pacific Corporation",
  WM = "Waste Management, Inc.",
  MMM = "3M Company",
  KTOS = "Kratos Defense & Security Solutions, Inc.",
  JOBY = "Joby Aviation, Inc.",
  ACHR = "Archer Aviation Inc.",
  LUNR = "Intuitive Machines, Inc.",
  EOG = "EOG Resources, Inc.",
  PSX = "Phillips 66",
  MPC = "Marathon Petroleum Corporation",
  VLO = "Valero Energy Corporation",
  SHEL = "Shell plc",
  TTE = "TotalEnergies SE",
  EQNR = "Equinor ASA",
  LNG = "Cheniere Energy, Inc.",
  LEU = "Centrus Energy Corp.",
  LIN = "Linde plc",
  SHW = "The Sherwin-Williams Company",
  SCCO = "Southern Copper Corporation",
  AA = "Alcoa Corporation",
  NUE = "Nucor Corporation",
  MP = "MP Materials Corp.",
  RIO = "Rio Tinto Group",
  BHP = "BHP Group Limited",
  AEM = "Agnico Eagle Mines Limited",
  WPM = "Wheaton Precious Metals Corp.",
  PAAS = "Pan American Silver Corp.",
  ALB = "Albemarle Corporation",
  SQM = "Sociedad Quimica y Minera de Chile S.A.",
  DUK = "Duke Energy Corporation",
  SO = "The Southern Company",
  NRG = "NRG Energy, Inc.",
  TLN = "Talen Energy Corporation",
  PLD = "Prologis, Inc.",
  AMT = "American Tower Corporation",
  EQIX = "Equinix, Inc.",
  DLR = "Digital Realty Trust, Inc.",
  SPG = "Simon Property Group, Inc.",
  CHTR = "Charter Communications, Inc.",
  TTWO = "Take-Two Interactive Software, Inc.",
  ROKU = "Roku, Inc.",
  BILI = "Bilibili Inc.",
  CLSK = "CleanSpark, Inc.",
  HUT = "Hut 8 Corp.",
  AAL = "American Airlines Group Inc.",
  AAPL = "Apple Inc.",
  ABEV = "Ambev S.A.",
  AFRM = "Affirm Holdings, Inc.",
  AGL = "agilon health, inc.",
  AGNC = "AGNC Investment Corp.",
  AI = "C3.ai, Inc.",
  ALIT = "Alight, Inc.",
  AMCR = "Amcor plc",
  AMD = "Advanced Micro Devices, Inc.",
  AMZN = "Amazon.com, Inc.",
  ARDX = "Ardelyx, Inc.",
  BA = "The Boeing Company",
  BABA = "Alibaba Group Holding Limited",
  BAC = "Bank of America Corporation",
  BBD = "Banco Bradesco S.A.",
  BCS = "Barclays PLC",
  BEKE = "KE Holdings Inc.",
  BIDU = "Baidu, Inc.",
  BK = "The Bank of New York Mellon Corporation",
  BKR = "Baker Hughes Company",
  BMY = "Bristol-Myers Squibb Company",
  BP = "BP p.l.c.",
  BSX = "Boston Scientific Corporation",
  BTE = "Baytex Energy Corp.",
  BTG = "B2Gold Corp.",
  C = "Citigroup Inc.",
  CCJ = "Cameco Corporation",
  CCL = "Carnival Corporation & plc",
  CFG = "Citizens Financial Group, Inc.",
  CHWY = "Chewy, Inc.",
  CL = "Colgate-Palmolive Company",
  CLF = "Cleveland-Cliffs Inc.",
  CMCSA = "Comcast Corporation",
  CNH = "CNH Industrial N.V.",            # antes CNHI
  COIN = "Coinbase Global, Inc.",
  COP = "ConocoPhillips",
  COTY = "Coty Inc.",
  CPNG = "Coupang, Inc.",
  CSCO = "Cisco Systems, Inc.",
  CSX = "CSX Corporation",
  CTSH = "Cognizant Technology Solutions Corporation",
  CVE = "Cenovus Energy Inc.",
  CVNA = "Carvana Co.",
  CVS = "CVS Health Corporation",
  CVX = "Chevron Corporation",
  DAL = "Delta Air Lines, Inc.",
  DB = "Deutsche Bank Aktiengesellschaft",
  DIDIY = "DiDi Global Inc.",
  DIS = "The Walt Disney Company",
  DKNG = "DraftKings Inc.",
  DNA = "Ginkgo Bioworks Holdings, Inc.",
  DOCU = "DocuSign, Inc.",
  DVN = "Devon Energy Corporation",
  EBAY = "eBay Inc.",
  ELAN = "Elanco Animal Health Incorporated",
  EPD = "Enterprise Products Partners L.P.",
  ERIC = "Telefonaktiebolaget LM Ericsson (publ)",
  ET = "Energy Transfer LP",
  F = "Ford Motor Company",
  FCX = "Freeport-McMoRan Inc.",
  FHN = "First Horizon Corporation",
  FITB = "Fifth Third Bancorp",
  GFI = "Gold Fields Limited",
  GILD = "Gilead Sciences, Inc.",
  GM = "General Motors Company",
  GOLD = "Barrick Gold Corporation",
  GOOG = "Alphabet Inc.",
  GOOGL = "Alphabet Inc.",
  GAP = "The Gap, Inc.",                  # antes GPS
  GRAB = "Grab Holdings Limited",
  GRFS = "Grifols, S.A.",
  HAL = "Halliburton Company",
  HBAN = "Huntington Bancshares Incorporated",
  HL = "Hecla Mining Company",
  HOOD = "Robinhood Markets, Inc.",
  HPE = "Hewlett Packard Enterprise Company",
  HPQ = "HP Inc.",
  HST = "Host Hotels & Resorts, Inc.",
  HTZ = "Hertz Global Holdings, Inc.",
  IBN = "ICICI Bank Limited",
  INFY = "Infosys Limited",
  INTC = "Intel Corporation",
  IQ = "iQIYI, Inc.",
  ITUB = "Itaú Unibanco Holding S.A.",
  JCI = "Johnson Controls International plc",
  JD = "JD.com, Inc.",
  JPM = "JPMorgan Chase & Co.",
  KDP = "Keurig Dr Pepper Inc.",
  KEY = "KeyCorp",
  KGC = "Kinross Gold Corporation",
  KIM = "Kimco Realty Corporation",
  KMI = "Kinder Morgan, Inc.",
  KO = "The Coca-Cola Company",
  KSS = "Kohl's Corporation",
  KVUE = "Kenvue Inc.",
  LCID = "Lucid Group, Inc.",
  LI = "Li Auto Inc.",
  LUV = "Southwest Airlines Co.",
  LYFT = "Lyft, Inc.",
  LYG = "Lloyds Banking Group plc",
  M = "Macy's, Inc.",
  MARA = "Marathon Digital Holdings, Inc.",
  MBLY = "Mobileye Global Inc.",
  MDT = "Medtronic plc",
  META = "Meta Platforms, Inc.",
  MO = "Altria Group, Inc.",
  MOS = "The Mosaic Company",
  MPW = "Medical Properties Trust, Inc.",
  MRK = "Merck & Co., Inc.",
  MRVL = "Marvell Technology, Inc.",
  MS = "Morgan Stanley",
  MSFT = "Microsoft Corporation",
  MU = "Micron Technology, Inc.",
  NCLH = "Norwegian Cruise Line Holdings Ltd.",
  NEE = "NextEra Energy, Inc.",
  NEM = "Newmont Corporation",
  NFLX = "Netflix, Inc.",
  NIO = "NIO Inc.",
  NKE = "NIKE, Inc.",
  NMR = "Nomura Holdings, Inc.",
  NOK = "Nokia Oyj",
  NOV = "NOV Inc.",
  NU = "Nu Holdings Ltd.",
  NVDA = "NVIDIA Corporation",
  NXE = "NexGen Energy Ltd.",
  FLG = "Flagstar Financial, Inc.",       # antes NYCB
  O = "Realty Income Corporation",
  ON = "ON Semiconductor Corporation",
  OPEN = "Opendoor Technologies Inc.",
  ORCL = "Oracle Corporation",
  OSCR = "Oscar Health, Inc.",
  OXY = "Occidental Petroleum Corporation",
  PSKY = "Paramount Skydance Corporation", # antes PARA
  PATH = "UiPath Inc.",
  PBR = "Petróleo Brasileiro S.A. - Petrobras",
  PCG = "PG&E Corporation",
  PDD = "PDD Holdings Inc.",
  DOC = "Healthpeak Properties, Inc.",    # antes PEAK
  PENN = "PENN Entertainment, Inc.",
  PFE = "Pfizer Inc.",
  PG = "The Procter & Gamble Company",
  PLTR = "Palantir Technologies Inc.",
  PLUG = "Plug Power Inc.",
  PR = "Permian Resources Corporation",
  PSNY = "Polestar Automotive Holding UK PLC",
  PTON = "Peloton Interactive, Inc.",
  PYPL = "PayPal Holdings, Inc.",
  QCOM = "QUALCOMM Incorporated",
  QS = "QuantumScape Corporation",
  RBLX = "Roblox Corporation",
  RF = "Regions Financial Corporation",
  RIG = "Transocean Ltd.",
  RIOT = "Riot Platforms, Inc.",
  RIVN = "Rivian Automotive, Inc.",
  RTX = "RTX Corporation",
  RUN = "Sunrun Inc.",
  RXRX = "Recursion Pharmaceuticals, Inc.",
  S = "SentinelOne, Inc.",
  SATS = "EchoStar Corporation",
  SBSW = "Sibanye Stillwater Limited",
  SBUX = "Starbucks Corporation",
  SCHW = "The Charles Schwab Corporation",
  SE = "Sea Limited",
  SHOP = "Shopify Inc.",
  SIRI = "Sirius XM Holdings Inc.",
  SLB = "Schlumberger Limited",
  SNAP = "Snap Inc.",
  SOFI = "SoFi Technologies, Inc.",
  XYZ = "Block, Inc.",                    # antes SQ
  EXE = "Expand Energy Corporation",      # antes SWN (fusion con Chesapeake)
  SYF = "Synchrony Financial",
  T = "AT&T Inc.",
  TEVA = "Teva Pharmaceutical Industries Limited",
  TFC = "Truist Financial Corporation",
  TGTX = "TG Therapeutics, Inc.",
  TME = "Tencent Music Entertainment Group",
  TOST = "Toast, Inc.",
  TSLA = "Tesla, Inc.",
  TSM = "Taiwan Semiconductor Manufacturing Company Limited",
  TTD = "The Trade Desk, Inc.",
  U = "Unity Software Inc.",
  UAA = "Under Armour, Inc.",
  UAL = "United Airlines Holdings, Inc.",
  UBER = "Uber Technologies, Inc.",
  UEC = "Uranium Energy Corp.",
  UNH = "UnitedHealth Group Incorporated",
  UPST = "Upstart Holdings, Inc.",
  USB = "U.S. Bancorp",
  VALE = "Vale S.A.",
  VFC = "V.F. Corporation",
  VOD = "Vodafone Group Public Limited Company",
  VTRS = "Viatris Inc.",
  VZ = "Verizon Communications Inc.",
  WBD = "Warner Bros. Discovery, Inc.",
  WFC = "Wells Fargo & Company",
  WIT = "Wipro Limited",
  WMB = "The Williams Companies, Inc.",
  XOM = "Exxon Mobil Corporation",
  XPEV = "XPeng Inc.",
  YPF = "YPF",
  GTM = "ZoomInfo Technologies Inc.",     # antes ZI
  BBAR = "Bbva Banco Frances",
  BMA = "Banco Macro",
  CEPU = "Central Puerto",
  CRESY = "Cresud",
  EDN = "Edenor",
  GGAL = "Galicia",
  IRS = "IRSA",
  LOMA = "LOMA",
  PAM = "Pampa Energia",
  SUPV = "Supervielle",
  TEO = "Telecom",
  TGS = "TGS",
  TS = "Tenaris",
  # --- Mega-caps que faltan ---
  V = "Visa Inc.",
  MA = "Mastercard Incorporated",
  WMT = "Walmart Inc.",
  JNJ = "Johnson & Johnson",
  LLY = "Eli Lilly and Company",
  AVGO = "Broadcom Inc.",
  `BRK-B` = "Berkshire Hathaway Inc.",   # ojo: en Yahoo lleva guion, por eso los backticks
  HD = "The Home Depot, Inc.",
  COST = "Costco Wholesale Corporation",
  MCD = "McDonald's Corporation",
  PEP = "PepsiCo, Inc.",
  ABBV = "AbbVie Inc.",
  ABT = "Abbott Laboratories",
  AMGN = "Amgen Inc.",
  TMO = "Thermo Fisher Scientific Inc.",
  GS = "The Goldman Sachs Group, Inc.",
  BLK = "BlackRock, Inc.",
  CAT = "Caterpillar Inc.",
  HON = "Honeywell International Inc.",
  GE = "GE Aerospace",
  LMT = "Lockheed Martin Corporation",
  DE = "Deere & Company",
  UPS = "United Parcel Service, Inc.",
  TMUS = "T-Mobile US, Inc.",
  
  # --- Software / semis / tech que faltan ---
  CRM = "Salesforce, Inc.",
  ADBE = "Adobe Inc.",
  NOW = "ServiceNow, Inc.",
  TXN = "Texas Instruments Incorporated",
  AMAT = "Applied Materials, Inc.",
  IBM = "International Business Machines Corporation",
  ARM = "Arm Holdings plc",
  SMCI = "Super Micro Computer, Inc.",
  DELL = "Dell Technologies Inc.",
  CRWD = "CrowdStrike Holdings, Inc.",
  PANW = "Palo Alto Networks, Inc.",
  SNOW = "Snowflake Inc.",
  NET = "Cloudflare, Inc.",
  
  # --- Growth / consumo digital ---
  ABNB = "Airbnb, Inc.",
  DASH = "DoorDash, Inc.",
  SPOT = "Spotify Technology S.A.",
  RDDT = "Reddit, Inc.",
  PINS = "Pinterest, Inc.",
  MSTR = "Strategy (MicroStrategy Incorporated)",
  
  # --- LatAm / Argentina (dado tu enfoque) ---
  MELI = "MercadoLibre, Inc.",
  GLOB = "Globant S.A.",
  VIST = "Vista Energy, S.A.B. de C.V.",
  BIOX = "Bioceres Crop Solutions Corp.",
  CAAP = "Corporación América Airports S.A.",
  TX = "Ternium",

  # --- Agregados: energia, infraestructura IA, salud y otros ---
  CEG = "Constellation Energy Corporation",
  GEV = "GE Vernova Inc.",
  ETN = "Eaton Corporation plc",
  VRT = "Vertiv Holdings Co.",
  ANET = "Arista Networks, Inc.",
  TER = "Teradyne, Inc.",
  ISRG = "Intuitive Surgical, Inc.",
  NVO = "Novo Nordisk A/S",
  AZN = "AstraZeneca PLC",
  RKLB = "Rocket Lab Corporation",
  ASTS = "AST SpaceMobile, Inc.",
  OKLO = "Oklo Inc.",
  RGTI = "Rigetti Computing, Inc.",

  # --- ETFs (indices, sector, oro, Brasil y cripto) ---
  SPY = "SPDR S&P 500 ETF (referencia)",
  QQQ = "Invesco QQQ - Nasdaq 100 (referencia)",
  XLV = "Health Care Select Sector SPDR ETF",
  GLD = "SPDR Gold Shares",
  EWZ = "iShares MSCI Brazil ETF",
  IBIT = "iShares Bitcoin Trust ETF",
  ETHA = "iShares Ethereum Trust ETF",

  # --- Infraestructura de IA: conectividad, optica y semis ---
  CRDO = "Credo Technology Group",
  ALAB = "Astera Labs, Inc.",
  LITE = "Lumentum Holdings Inc.",
  COHR = "Coherent Corp.",
  ASML = "ASML Holding N.V.",
  LRCX = "Lam Research Corporation",
  KLAC = "KLA Corporation",
  SNPS = "Synopsys, Inc.",
  CDNS = "Cadence Design Systems, Inc.",
  ADI = "Analog Devices, Inc.",
  CRWV = "CoreWeave, Inc.",

  # --- Software ---
  INTU = "Intuit Inc.",
  APP = "AppLovin Corporation",
  DDOG = "Datadog, Inc.",
  ZS = "Zscaler, Inc.",
  MDB = "MongoDB, Inc.",
  FIG = "Figma, Inc.",

  # --- Energia nuclear y electricidad ---
  VST = "Vistra Corp.",
  SMR = "NuScale Power Corporation",

  # --- Espacio ---
  SPCX = "SpaceX (Space Exploration Technologies Corp.)",

  # --- Salud ---
  VRTX = "Vertex Pharmaceuticals Incorporated",
  REGN = "Regeneron Pharmaceuticals, Inc.",
  DHR = "Danaher Corporation",
  TEM = "Tempus AI, Inc.",

  # --- Robotica ---
  SYM = "Symbotic Inc.",
  HSAI = "Hesai Group",

  # --- Computacion cuantica ---
  IONQ = "IonQ, Inc.",

  # --- Fintech / salidas a bolsa recientes ---
  CRCL = "Circle Internet Group, Inc.",
  ETOR = "eToro Group Ltd.",
  KLAR = "Klarna Group plc",

  # --- Litio ---
  LAC = "Lithium Americas Corp."
)


# Arma la tabla final. Los tickers que fallan (deslistados, sin datos, etc.)
# ya no se pierden en silencio: quedan en 'errores_tickers' y se muestran
# en la pestana Referencias del dashboard.
crear_dataframe <- function(acciones, sp500, min_ruedas = 2) {
  dataframe_final <- data.frame()
  errores <- data.frame(Especie = character(), Nombre = character(), Error = character())
  historia <- list()   # precios de las ultimas 760 ruedas, para la matriz de correlaciones

  n_total <- length(acciones)
  inicio  <- Sys.time()
  log_consola("Bajando precios de Yahoo para", n_total, "tickers")

  for (i in seq_along(acciones)) {
    accion <- names(acciones)[i]
    accion_completa = acciones[[accion]]
    log_progreso(i, n_total, inicio, paste(accion, "-", accion_completa))
    Sys.sleep(PAUSA_ENTRE_TICKERS)   # pausa para no saturar a Yahoo

    tryCatch({
      raw_data = getRawData(accion)
      if (nrow(raw_data) < min_ruedas) stop(paste("menos de", min_ruedas, "ruedas de historia"))
      data = getData(raw_data, accion, accion_completa, sp500)

      dataframe_final <- rbind(dataframe_final, data)
      historia[[accion]] <- tail(getOrder(raw_data)[, c("fecha", "precio")], 760)

    }, error = function(e) {
      log_consola("    ERROR en", accion, ":", conditionMessage(e))
      errores <<- rbind(errores, data.frame(Especie = accion, Nombre = accion_completa,
                                            Error = conditionMessage(e)))
    })
  }

  log_consola("Precios listos:", nrow(dataframe_final), "tickers OK,", nrow(errores), "con error. Tardo",
              round(as.numeric(difftime(Sys.time(), inicio, units = "mins")), 1), "min")
  rownames(dataframe_final) <- NULL
  errores_tickers <<- errores
  precios_hist <<- historia
  return(dataframe_final)
}


# =====================================================================
#  calcularCorrResumen(): correlacion de TODAS las acciones contra todas
#  ---------------------------------------------------------------------
#  Igual criterio que CorrSp500: retornos diarios (log) alineados por fecha,
#  ultimas ~3 anos (750 ruedas). Un par solo cuenta si tiene al menos
#  120 ruedas en comun. Para cada accion devuelve:
#    MasParecida / CorrMasParecida : la accion con la que mas se mueve igual
#    MasDistinta / CorrMasDistinta : la accion con la que menos se parece
# =====================================================================
calcularCorrResumen <- function(precios_hist, nombres, ruedas = 750, min_obs = 120) {
  vacio <- data.frame(Especie = character(), Nombre = character(),
                      MasParecida = character(), CorrMasParecida = numeric(),
                      MasDistinta = character(), CorrMasDistinta = numeric())
  if (length(precios_hist) < 2) return(vacio)

  retornos <- do.call(rbind, lapply(names(precios_hist), function(tk) {
    d <- tail(precios_hist[[tk]], ruedas + 1)
    if (nrow(d) < 2) return(NULL)
    data.frame(fecha = d$fecha[-1], r = diff(log(d$precio)), tk = tk)
  }))
  ancho <- tidyr::pivot_wider(retornos, names_from = tk, values_from = r)
  ancho <- tail(ancho[order(ancho$fecha), ], ruedas)
  M <- as.matrix(ancho[, -1])

  C <- suppressWarnings(cor(M, use = "pairwise.complete.obs"))
  N <- crossprod(!is.na(M))          # ruedas en comun de cada par
  C[N < min_obs] <- NA
  diag(C) <- NA

  elegir <- function(x, f) {
    if (all(is.na(x))) return(c(NA, NA))
    i <- f(x)
    c(names(x)[i], round(x[i], 2))
  }
  max_par <- t(apply(C, 1, elegir, f = which.max))
  min_par <- t(apply(C, 1, elegir, f = which.min))

  data.frame(
    Especie = rownames(C),
    Nombre = unname(unlist(nombres[rownames(C)])),
    MasParecida = max_par[, 1],
    CorrMasParecida = as.numeric(max_par[, 2]),
    MasDistinta = min_par[, 1],
    CorrMasDistinta = as.numeric(min_par[, 2]),
    row.names = NULL
  )
}


# =====================================================================
#  PRUEBA RAPIDA DEL INDICE PARA UNA ACCION (reutiliza getRawData,
#  sin descargas extra: baja una vez y calcula las dos ventanas)
# =====================================================================
# raw_csx <- getOrder(getRawData("CSX"))
# getIndiceFuerza(raw_csx, 90)              # solo el numero
# getIndiceFuerzaDetalle(raw_csx, 90)       # descomposicion: t_precio | aporte_vol | indice_total
# getIndiceFuerzaDetalle(raw_csx, 365)



# =====================================================================
#  RUBRO de cada accion (clasificacion GICS de S&P / MSCI, agrupada en 14
#  rubros). Si agregas un ticker nuevo a 'datos', agregalo aca tambien;
#  si falta, aparece como "Otro".
# =====================================================================
rubros <- c(
  QNT = "Tecnologia",
  XP = "Financiero",
  QUBT = "Tecnologia",
  BBAI = "Tecnologia",
  RDW = "Industria",
  PL = "Industria",
  NNE = "Servicios publicos",
  ONON = "Consumo discrecional",
  CAVA = "Consumo discrecional",
  CELH = "Consumo basico",
  ONDS = "Tecnologia",
  SATL = "Industria",
  IREN = "Cripto",
  BMNR = "Cripto",
  CLS = "Tecnologia",
  NTRA = "Salud",
  BB = "Tecnologia",
  FNMA = "Financiero",
  FMCC = "Financiero",
  AGRO = "Consumo basico",
  LAR = "Materiales y mineria",
  LND = "Consumo basico",
  GPRK = "Energia",
  MUX = "Materiales y mineria",
  CDE = "Materiales y mineria",
  HMY = "Materiales y mineria",
  NG = "Materiales y mineria",
  GLNG = "Energia",
  JMIA = "Consumo discrecional",
  JOYY = "Comunicacion e internet",
  VIV = "Comunicacion e internet",
  TIMB = "Comunicacion e internet",
  SBS = "Servicios publicos",
  ELP = "Servicios publicos",
  EBR = "Servicios publicos",
  GGB = "Materiales y mineria",
  SID = "Materiales y mineria",
  ERJ = "Industria",
  BRFS = "Consumo basico",
  UGP = "Energia",
  SUZ = "Materiales y mineria",
  PAGS = "Financiero",
  STNE = "Financiero",
  BSBR = "Financiero",
  BAK = "Materiales y mineria",
  `AKO-B` = "Consumo basico",
  CX = "Materiales y mineria",
  FMX = "Consumo basico",
  AMX = "Comunicacion e internet",
  TV = "Comunicacion e internet",
  ASR = "Industria",
  PAC = "Industria",
  HDB = "Financiero",
  KB = "Financiero",
  KEP = "Servicios publicos",
  MUFG = "Financiero",
  MFG = "Financiero",
  HMC = "Consumo discrecional",
  NTES = "Comunicacion e internet",
  TCOM = "Consumo discrecional",
  E = "Energia",
  AEG = "Financiero",
  ING = "Financiero",
  NGG = "Servicios publicos",
  PHG = "Salud",
  STLA = "Consumo discrecional",
  PSO = "Consumo discrecional",
  GT = "Consumo discrecional",
  HOG = "Consumo discrecional",
  AAP = "Consumo discrecional",
  ANF = "Consumo discrecional",
  URBN = "Consumo discrecional",
  ETSY = "Consumo discrecional",
  YELP = "Comunicacion e internet",
  TRIP = "Comunicacion e internet",
  CAR = "Industria",
  PBI = "Industria",
  ACWI = "ETF",
  EFA = "ETF",
  VEA = "ETF",
  IEMG = "ETF",
  IEUR = "ETF",
  EWJ = "ETF",
  EWY = "ETF",
  FXI = "ETF",
  ILF = "ETF",
  IJH = "ETF",
  IVE = "ETF",
  IVW = "ETF",
  RSP = "ETF",
  VIG = "ETF",
  SPHQ = "ETF",
  ESGU = "ETF",
  XLB = "ETF",
  XLC = "ETF",
  XLRE = "ETF",
  XME = "ETF",
  COPX = "ETF",
  GDX = "ETF",
  IBB = "ETF",
  ICLN = "ETF",
  CIBR = "ETF",
  USO = "ETF",
  CORN = "ETF",
  SOYB = "ETF",
  GSG = "ETF",
  AOS = "Industria",
  AES = "Servicios publicos",
  AFL = "Financiero",
  A = "Salud",
  APD = "Materiales y mineria",
  AKAM = "Tecnologia",
  ARE = "Inmobiliario",
  ALGN = "Salud",
  ALLE = "Industria",
  LNT = "Servicios publicos",
  ALL = "Financiero",
  AEE = "Servicios publicos",
  AEP = "Servicios publicos",
  AIG = "Financiero",
  AWK = "Servicios publicos",
  AMP = "Financiero",
  AME = "Industria",
  APH = "Tecnologia",
  AON = "Financiero",
  APA = "Energia",
  APTV = "Consumo discrecional",
  ACGL = "Financiero",
  ADM = "Consumo basico",
  ARES = "Financiero",
  AJG = "Financiero",
  AIZ = "Financiero",
  ATO = "Servicios publicos",
  ADSK = "Tecnologia",
  ADP = "Industria",
  AZO = "Consumo discrecional",
  AVY = "Materiales y mineria",
  BALL = "Materiales y mineria",
  BAX = "Salud",
  BBY = "Consumo discrecional",
  TECH = "Salud",
  BE = "Industria",
  BNY = "Financiero",
  BR = "Industria",
  BRO = "Financiero",
  `BF-B` = "Consumo basico",
  BG = "Consumo basico",
  BXP = "Inmobiliario",
  CHRW = "Industria",
  CPT = "Inmobiliario",
  CAH = "Salud",
  CARR = "Industria",
  CASY = "Consumo basico",
  CBOE = "Financiero",
  CBRE = "Inmobiliario",
  CDW = "Tecnologia",
  COR = "Salud",
  CNC = "Salud",
  CNP = "Servicios publicos",
  CF = "Materiales y mineria",
  CRL = "Salud",
  CHD = "Consumo basico",
  CINF = "Financiero",
  CTAS = "Industria",
  CLX = "Consumo basico",
  CMS = "Servicios publicos",
  FIX = "Industria",
  ED = "Servicios publicos",
  STZ = "Consumo basico",
  COO = "Salud",
  CPRT = "Industria",
  GLW = "Tecnologia",
  CPAY = "Financiero",
  CSGP = "Inmobiliario",
  CRH = "Materiales y mineria",
  CCI = "Inmobiliario",
  CMI = "Industria",
  DRI = "Consumo discrecional",
  DVA = "Salud",
  DECK = "Consumo discrecional",
  FANG = "Energia",
  DG = "Consumo basico",
  DLTR = "Consumo basico",
  D = "Servicios publicos",
  DPZ = "Consumo discrecional",
  DOV = "Industria",
  DOW = "Materiales y mineria",
  DHI = "Consumo discrecional",
  DTE = "Servicios publicos",
  DD = "Industria",
  ECHO = "Comunicacion e internet",
  ECL = "Materiales y mineria",
  EIX = "Servicios publicos",
  EME = "Industria",
  EMR = "Industria",
  ETR = "Servicios publicos",
  EQT = "Energia",
  EFX = "Industria",
  ERIE = "Financiero",
  ESS = "Inmobiliario",
  EL = "Consumo basico",
  EG = "Financiero",
  EVRG = "Servicios publicos",
  P = "Tecnologia",
  ES = "Servicios publicos",
  EXC = "Servicios publicos",
  EXPE = "Consumo discrecional",
  EXPD = "Industria",
  EXR = "Inmobiliario",
  FFIV = "Tecnologia",
  FDS = "Financiero",
  FICO = "Tecnologia",
  FAST = "Industria",
  FRT = "Inmobiliario",
  FDXF = "Industria",
  FERG = "Industria",
  FIS = "Financiero",
  FSLR = "Semiconductores",
  FE = "Servicios publicos",
  FISV = "Financiero",
  FLEX = "Tecnologia",
  FTV = "Industria",
  FOXA = "Comunicacion e internet",
  FOX = "Comunicacion e internet",
  BEN = "Financiero",
  GRMN = "Consumo discrecional",
  IT = "Tecnologia",
  GEHC = "Salud",
  GEN = "Tecnologia",
  GNRC = "Industria",
  GIS = "Consumo basico",
  GPC = "Consumo discrecional",
  GPN = "Financiero",
  GL = "Financiero",
  GDDY = "Tecnologia",
  HIG = "Financiero",
  HAS = "Consumo discrecional",
  HSIC = "Salud",
  HLT = "Consumo discrecional",
  HONA = "Industria",
  HRL = "Consumo basico",
  HUBB = "Industria",
  HUM = "Salud",
  HII = "Industria",
  IEX = "Industria",
  IDXX = "Salud",
  ITW = "Industria",
  ILMN = "Salud",
  INCY = "Salud",
  IR = "Industria",
  PODD = "Salud",
  IBKR = "Financiero",
  IFF = "Materiales y mineria",
  IP = "Materiales y mineria",
  IVZ = "Financiero",
  INVH = "Inmobiliario",
  IQV = "Salud",
  IRM = "Inmobiliario",
  JBHT = "Industria",
  JBL = "Tecnologia",
  JKHY = "Financiero",
  J = "Industria",
  KEYS = "Tecnologia",
  KMB = "Consumo basico",
  KHC = "Consumo basico",
  KR = "Consumo basico",
  LH = "Salud",
  LVS = "Consumo discrecional",
  LDOS = "Industria",
  LEN = "Consumo discrecional",
  LII = "Industria",
  LYV = "Comunicacion e internet",
  L = "Financiero",
  LYB = "Materiales y mineria",
  MTB = "Financiero",
  MRSH = "Financiero",
  MLM = "Materiales y mineria",
  MAS = "Industria",
  MKC = "Consumo basico",
  MET = "Financiero",
  MTD = "Salud",
  MGM = "Consumo discrecional",
  MAA = "Inmobiliario",
  MSI = "Tecnologia",
  MSCI = "Financiero",
  NDAQ = "Financiero",
  NTAP = "Tecnologia",
  NWSA = "Comunicacion e internet",
  NWS = "Comunicacion e internet",
  NI = "Servicios publicos",
  NDSN = "Industria",
  NSC = "Industria",
  NTRS = "Financiero",
  NVR = "Consumo discrecional",
  ORLY = "Consumo discrecional",
  ODFL = "Industria",
  OMC = "Comunicacion e internet",
  OKE = "Energia",
  OTIS = "Industria",
  PCAR = "Industria",
  PKG = "Materiales y mineria",
  PH = "Industria",
  PAYX = "Industria",
  PNR = "Industria",
  PNW = "Servicios publicos",
  PNC = "Financiero",
  PPG = "Materiales y mineria",
  PPL = "Servicios publicos",
  PFG = "Financiero",
  PRU = "Financiero",
  PEG = "Servicios publicos",
  PTC = "Tecnologia",
  PSA = "Inmobiliario",
  PHM = "Consumo discrecional",
  DGX = "Salud",
  Q = "Semiconductores",
  RL = "Consumo discrecional",
  RJF = "Financiero",
  REG = "Inmobiliario",
  RSG = "Industria",
  RMD = "Salud",
  RVTY = "Salud",
  ROK = "Industria",
  ROL = "Industria",
  ROP = "Tecnologia",
  ROST = "Consumo discrecional",
  RCL = "Consumo discrecional",
  SBAC = "Inmobiliario",
  SRE = "Servicios publicos",
  SKYD = "Comunicacion e internet",
  SWKS = "Semiconductores",
  SJM = "Consumo basico",
  SW = "Materiales y mineria",
  SNA = "Industria",
  SOLV = "Salud",
  SWK = "Industria",
  STT = "Financiero",
  STLD = "Materiales y mineria",
  STE = "Salud",
  SYY = "Consumo basico",
  TROW = "Financiero",
  TPR = "Consumo discrecional",
  TRGP = "Energia",
  TEL = "Tecnologia",
  TDY = "Tecnologia",
  TPL = "Energia",
  TXT = "Industria",
  TKO = "Comunicacion e internet",
  TSCO = "Consumo discrecional",
  TT = "Industria",
  TDG = "Industria",
  TRV = "Financiero",
  TRMB = "Tecnologia",
  TYL = "Tecnologia",
  TSN = "Consumo basico",
  UDR = "Inmobiliario",
  ULTA = "Consumo discrecional",
  URI = "Industria",
  UHS = "Salud",
  VEEV = "Salud",
  VTR = "Inmobiliario",
  VLTO = "Industria",
  VRSN = "Tecnologia",
  VRSK = "Industria",
  VICI = "Inmobiliario",
  VMRK = "Inmobiliario",
  VMC = "Materiales y mineria",
  VYLR = "Consumo basico",
  WRB = "Financiero",
  GWW = "Industria",
  WAB = "Industria",
  WAT = "Salud",
  WEC = "Servicios publicos",
  WELL = "Inmobiliario",
  WST = "Salud",
  WY = "Inmobiliario",
  WSM = "Consumo discrecional",
  WTW = "Financiero",
  WYNN = "Consumo discrecional",
  XEL = "Servicios publicos",
  XYL = "Industria",
  YUM = "Consumo discrecional",
  ZBRA = "Tecnologia",
  ZBH = "Salud",
  SOXX = "ETF",
  SMH = "ETF",
  DIA = "ETF",
  IWM = "ETF",
  VOO = "ETF",
  XLK = "ETF",
  XLF = "ETF",
  XLE = "ETF",
  XLI = "ETF",
  XLY = "ETF",
  XLP = "ETF",
  XLU = "ETF",
  ARKK = "ETF",
  EEM = "ETF",
  TLT = "ETF",
  SLV = "ETF",
  URA = "ETF",
  ITA = "ETF",
  MRNA = "Salud",
  SYK = "Salud",
  BDX = "Salud",
  ELV = "Salud",
  CI = "Salud",
  HCA = "Salud",
  MCK = "Salud",
  ZTS = "Salud",
  BIIB = "Salud",
  DXCM = "Salud",
  EW = "Salud",
  HIMS = "Salud",
  NVS = "Salud",
  SNY = "Salud",
  GSK = "Salud",
  ACN = "Tecnologia",
  SAP = "Tecnologia",
  WDAY = "Tecnologia",
  FTNT = "Tecnologia",
  ZM = "Tecnologia",
  TWLO = "Tecnologia",
  OKTA = "Tecnologia",
  TEAM = "Tecnologia",
  NBIS = "Tecnologia",
  SOUN = "Tecnologia",
  QBTS = "Tecnologia",
  CIEN = "Tecnologia",
  STX = "Tecnologia",
  WDC = "Tecnologia",
  SNDK = "Tecnologia",
  NXPI = "Semiconductores",
  MCHP = "Semiconductores",
  MPWR = "Semiconductores",
  STM = "Semiconductores",
  GFS = "Semiconductores",
  AMKR = "Semiconductores",
  LULU = "Consumo discrecional",
  CMG = "Consumo discrecional",
  BKNG = "Consumo discrecional",
  MAR = "Consumo discrecional",
  TGT = "Consumo discrecional",
  LOW = "Consumo discrecional",
  TJX = "Consumo discrecional",
  RACE = "Consumo discrecional",
  ARCO = "Consumo discrecional",
  TM = "Consumo discrecional",
  SONY = "Consumo discrecional",
  DUOL = "Consumo discrecional",
  PM = "Consumo basico",
  MDLZ = "Consumo basico",
  MNST = "Consumo basico",
  HSY = "Consumo basico",
  BUD = "Consumo basico",
  DEO = "Consumo basico",
  UL = "Consumo basico",
  AXP = "Financiero",
  COF = "Financiero",
  SPGI = "Financiero",
  MCO = "Financiero",
  ICE = "Financiero",
  CME = "Financiero",
  BX = "Financiero",
  KKR = "Financiero",
  APO = "Financiero",
  PGR = "Financiero",
  CB = "Financiero",
  HSBC = "Financiero",
  SAN = "Financiero",
  BBVA = "Financiero",
  UBS = "Financiero",
  GD = "Industria",
  NOC = "Industria",
  LHX = "Industria",
  HWM = "Industria",
  AXON = "Industria",
  PWR = "Industria",
  FDX = "Industria",
  UNP = "Industria",
  WM = "Industria",
  MMM = "Industria",
  KTOS = "Industria",
  JOBY = "Industria",
  ACHR = "Industria",
  LUNR = "Industria",
  EOG = "Energia",
  PSX = "Energia",
  MPC = "Energia",
  VLO = "Energia",
  SHEL = "Energia",
  TTE = "Energia",
  EQNR = "Energia",
  LNG = "Energia",
  LEU = "Energia",
  LIN = "Materiales y mineria",
  SHW = "Materiales y mineria",
  SCCO = "Materiales y mineria",
  AA = "Materiales y mineria",
  NUE = "Materiales y mineria",
  MP = "Materiales y mineria",
  RIO = "Materiales y mineria",
  BHP = "Materiales y mineria",
  AEM = "Materiales y mineria",
  WPM = "Materiales y mineria",
  PAAS = "Materiales y mineria",
  ALB = "Materiales y mineria",
  SQM = "Materiales y mineria",
  DUK = "Servicios publicos",
  SO = "Servicios publicos",
  NRG = "Servicios publicos",
  TLN = "Servicios publicos",
  PLD = "Inmobiliario",
  AMT = "Inmobiliario",
  EQIX = "Inmobiliario",
  DLR = "Inmobiliario",
  SPG = "Inmobiliario",
  CHTR = "Comunicacion e internet",
  TTWO = "Comunicacion e internet",
  ROKU = "Comunicacion e internet",
  BILI = "Comunicacion e internet",
  CLSK = "Cripto",
  HUT = "Cripto",
  # Tecnologia
  AAPL = "Tecnologia", AI = "Tecnologia", CSCO = "Tecnologia", CTSH = "Tecnologia", DOCU = "Tecnologia",
  ERIC = "Tecnologia", HPE = "Tecnologia", HPQ = "Tecnologia", INFY = "Tecnologia", MSFT = "Tecnologia",
  NOK = "Tecnologia", ORCL = "Tecnologia", PATH = "Tecnologia", PLTR = "Tecnologia", S = "Tecnologia",
  SHOP = "Tecnologia", U = "Tecnologia", WIT = "Tecnologia", GTM = "Tecnologia", CRM = "Tecnologia",
  ADBE = "Tecnologia", NOW = "Tecnologia", IBM = "Tecnologia", SMCI = "Tecnologia", DELL = "Tecnologia",
  CRWD = "Tecnologia", PANW = "Tecnologia", SNOW = "Tecnologia", NET = "Tecnologia", GLOB = "Tecnologia",
  ANET = "Tecnologia", RGTI = "Tecnologia", LITE = "Tecnologia", COHR = "Tecnologia", SNPS = "Tecnologia",
  CDNS = "Tecnologia", CRWV = "Tecnologia", INTU = "Tecnologia", APP = "Tecnologia", DDOG = "Tecnologia",
  ZS = "Tecnologia", MDB = "Tecnologia", FIG = "Tecnologia", IONQ = "Tecnologia",

  # Semiconductores
  AMD = "Semiconductores", INTC = "Semiconductores", MRVL = "Semiconductores", MU = "Semiconductores", NVDA = "Semiconductores",
  ON = "Semiconductores", QCOM = "Semiconductores", TSM = "Semiconductores", AVGO = "Semiconductores", TXN = "Semiconductores",
  AMAT = "Semiconductores", ARM = "Semiconductores", TER = "Semiconductores", CRDO = "Semiconductores", ALAB = "Semiconductores",
  ASML = "Semiconductores", LRCX = "Semiconductores", KLAC = "Semiconductores", ADI = "Semiconductores",

  # Comunicacion e internet
  BIDU = "Comunicacion e internet", CMCSA = "Comunicacion e internet", DIS = "Comunicacion e internet", GOOG = "Comunicacion e internet", GOOGL = "Comunicacion e internet",
  IQ = "Comunicacion e internet", META = "Comunicacion e internet", NFLX = "Comunicacion e internet", PSKY = "Comunicacion e internet", RBLX = "Comunicacion e internet",
  SATS = "Comunicacion e internet", SIRI = "Comunicacion e internet", SNAP = "Comunicacion e internet", T = "Comunicacion e internet", TME = "Comunicacion e internet",
  TTD = "Comunicacion e internet", VOD = "Comunicacion e internet", VZ = "Comunicacion e internet", WBD = "Comunicacion e internet", TEO = "Comunicacion e internet",
  TMUS = "Comunicacion e internet", SPOT = "Comunicacion e internet", RDDT = "Comunicacion e internet", PINS = "Comunicacion e internet", ASTS = "Comunicacion e internet",

  # Consumo discrecional
  AMZN = "Consumo discrecional", BABA = "Consumo discrecional", CCL = "Consumo discrecional", CHWY = "Consumo discrecional", CPNG = "Consumo discrecional",
  CVNA = "Consumo discrecional", DKNG = "Consumo discrecional", EBAY = "Consumo discrecional", F = "Consumo discrecional", GM = "Consumo discrecional",
  GAP = "Consumo discrecional", JD = "Consumo discrecional", KSS = "Consumo discrecional", LCID = "Consumo discrecional", LI = "Consumo discrecional",
  M = "Consumo discrecional", MBLY = "Consumo discrecional", NCLH = "Consumo discrecional", NIO = "Consumo discrecional", NKE = "Consumo discrecional",
  PDD = "Consumo discrecional", PENN = "Consumo discrecional", PSNY = "Consumo discrecional", PTON = "Consumo discrecional", QS = "Consumo discrecional",
  RIVN = "Consumo discrecional", SBUX = "Consumo discrecional", SE = "Consumo discrecional", TSLA = "Consumo discrecional", UAA = "Consumo discrecional",
  VFC = "Consumo discrecional", XPEV = "Consumo discrecional", HD = "Consumo discrecional", MCD = "Consumo discrecional", ABNB = "Consumo discrecional",
  DASH = "Consumo discrecional", MELI = "Consumo discrecional", HSAI = "Consumo discrecional",

  # Consumo basico
  ABEV = "Consumo basico", CL = "Consumo basico", COTY = "Consumo basico", KDP = "Consumo basico", KO = "Consumo basico",
  KVUE = "Consumo basico", MO = "Consumo basico", PG = "Consumo basico", CRESY = "Consumo basico", WMT = "Consumo basico",
  COST = "Consumo basico", PEP = "Consumo basico",

  # Salud
  AGL = "Salud", ARDX = "Salud", BMY = "Salud", BSX = "Salud", CVS = "Salud",
  DNA = "Salud", ELAN = "Salud", GILD = "Salud", GRFS = "Salud", MDT = "Salud",
  MRK = "Salud", OSCR = "Salud", PFE = "Salud", RXRX = "Salud", TEVA = "Salud",
  TGTX = "Salud", UNH = "Salud", VTRS = "Salud", JNJ = "Salud", LLY = "Salud",
  ABBV = "Salud", ABT = "Salud", AMGN = "Salud", TMO = "Salud", ISRG = "Salud",
  NVO = "Salud", AZN = "Salud", VRTX = "Salud", REGN = "Salud", DHR = "Salud",
  TEM = "Salud",

  # Financiero
  AFRM = "Financiero", AGNC = "Financiero", BAC = "Financiero", BBD = "Financiero", BCS = "Financiero",
  BK = "Financiero", C = "Financiero", CFG = "Financiero", COIN = "Financiero", DB = "Financiero",
  FHN = "Financiero", FITB = "Financiero", HBAN = "Financiero", HOOD = "Financiero", IBN = "Financiero",
  ITUB = "Financiero", JPM = "Financiero", KEY = "Financiero", LYG = "Financiero", MS = "Financiero",
  NMR = "Financiero", NU = "Financiero", FLG = "Financiero", PYPL = "Financiero", RF = "Financiero",
  SCHW = "Financiero", SOFI = "Financiero", XYZ = "Financiero", SYF = "Financiero", TFC = "Financiero",
  TOST = "Financiero", UPST = "Financiero", USB = "Financiero", WFC = "Financiero", BBAR = "Financiero",
  BMA = "Financiero", GGAL = "Financiero", SUPV = "Financiero", V = "Financiero", MA = "Financiero",
  `BRK-B` = "Financiero", GS = "Financiero", BLK = "Financiero", CRCL = "Financiero", ETOR = "Financiero",
  KLAR = "Financiero",

  # Energia
  BKR = "Energia", BP = "Energia", BTE = "Energia", CCJ = "Energia", COP = "Energia",
  CVE = "Energia", CVX = "Energia", DVN = "Energia", EPD = "Energia", ET = "Energia",
  HAL = "Energia", KMI = "Energia", NOV = "Energia", NXE = "Energia", OXY = "Energia",
  PBR = "Energia", PR = "Energia", RIG = "Energia", SLB = "Energia", EXE = "Energia",
  UEC = "Energia", WMB = "Energia", XOM = "Energia", YPF = "Energia", TGS = "Energia",
  TS = "Energia", VIST = "Energia",

  # Industria
  AAL = "Industria", ALIT = "Industria", BA = "Industria", CNH = "Industria", CSX = "Industria",
  DAL = "Industria", DIDIY = "Industria", GRAB = "Industria", HTZ = "Industria", JCI = "Industria",
  LUV = "Industria", LYFT = "Industria", PLUG = "Industria", RTX = "Industria", RUN = "Industria",
  UAL = "Industria", UBER = "Industria", CAT = "Industria", HON = "Industria", GE = "Industria",
  LMT = "Industria", DE = "Industria", UPS = "Industria", CAAP = "Industria", GEV = "Industria",
  ETN = "Industria", VRT = "Industria", RKLB = "Industria", SMR = "Industria", SPCX = "Industria",
  SYM = "Industria",

  # Materiales y mineria
  AMCR = "Materiales y mineria", BTG = "Materiales y mineria", CLF = "Materiales y mineria", FCX = "Materiales y mineria", GFI = "Materiales y mineria",
  GOLD = "Materiales y mineria", HL = "Materiales y mineria", KGC = "Materiales y mineria", MOS = "Materiales y mineria", NEM = "Materiales y mineria",
  SBSW = "Materiales y mineria", VALE = "Materiales y mineria", LOMA = "Materiales y mineria", BIOX = "Materiales y mineria", TX = "Materiales y mineria",
  LAC = "Materiales y mineria",

  # Servicios publicos
  NEE = "Servicios publicos", PCG = "Servicios publicos", CEPU = "Servicios publicos", EDN = "Servicios publicos", PAM = "Servicios publicos",
  CEG = "Servicios publicos", OKLO = "Servicios publicos", VST = "Servicios publicos",

  # Inmobiliario
  BEKE = "Inmobiliario", HST = "Inmobiliario", KIM = "Inmobiliario", MPW = "Inmobiliario", O = "Inmobiliario",
  OPEN = "Inmobiliario", DOC = "Inmobiliario", IRS = "Inmobiliario",

  # ETF
  SPY = "ETF", QQQ = "ETF", XLV = "ETF", GLD = "ETF", EWZ = "ETF",

  # Cripto
  MARA = "Cripto", RIOT = "Cripto", MSTR = "Cripto", IBIT = "Cripto", ETHA = "Cripto"
)



# =====================================================================
#  CRECIMIENTO DE VENTAS (balance trimestral)
#  ---------------------------------------------------------------------
#  Fuente: SEC EDGAR (data.sec.gov), el registro oficial donde las empresas
#  de EE.UU. presentan sus balances (formularios 10-Q y 10-K). Es gratis y
#  no necesita clave, pero la SEC pide un User-Agent con un mail de contacto.
#
#  Para cada empresa:
#   1. Se bajan TODOS sus datos en un solo pedido y se busca la linea de
#      ventas entre varios nombres posibles (cada rubro la llama distinto:
#      electricas, bancos, inmobiliarias, petroleras...).
#      Se bajan las ventas trimestrales. El 4to trimestre casi nunca viene
#      suelto (va dentro del balance anual), asi que se calcula como
#      anual - (T1 + T2 + T3).
#   2. Se calcula el crecimiento interanual de cada trimestre: ventas del
#      trimestre contra el mismo trimestre del año anterior.
#   3. Se miran los ultimos 4 crecimientos para ver si ACELERA o FRENA.
#
#  Las empresas que no presentan balances trimestrales en EE.UU. (la mayoria
#  de las extranjeras: brasileñas, argentinas, europeas) y los ETFs quedan
#  como "Sin datos".
#
#  Los balances salen cada 3 meses, asi que no hace falta bajarlos todos los
#  dias: se guardan en _cache/ventas_trimestrales.csv y se vuelven a bajar
#  cada VENTAS_DIAS_CACHE dias (el workflow sube ese archivo al repo).
# =====================================================================
SEC_USER_AGENT    <- "app_financiera maxigaloto@hotmail.com"
VENTAS_CACHE      <- "_cache/ventas_trimestrales.csv"
VENTAS_DIAS_CACHE <- 7       # cada cuantos dias se vuelven a bajar los balances
VENTAS_VERSION    <- 2       # si se cambia el calculo, subir este numero: se vuelve a bajar todo
VENTAS_MAX_DIAS   <- 400     # si el ultimo trimestre es mas viejo que esto, no se usa
SEC_PAUSA         <- 0.15    # segundos entre pedidos (la SEC permite hasta 10 por segundo)

# Cada empresa nombra sus ventas a su manera. Se prueban todos estos nombres
# y se queda con la serie mas reciente (si empatan, la mas grande = el total).
SEC_CONCEPTOS_VENTAS <- c(
  "Revenues",                                              # la mayoria
  "RevenueFromContractWithCustomerExcludingAssessedTax",   # norma contable nueva (2018+)
  "RevenueFromContractWithCustomerIncludingAssessedTax",   # petroleras, tabacaleras (con impuestos)
  "RevenuesNetOfInterestExpense",                          # bancos de inversion, brokers
  "SalesRevenueNet", "SalesRevenueGoodsNet", "SalesRevenueServicesNet",  # nombres viejos
  "RegulatedAndUnregulatedOperatingRevenue",               # electricas
  "ElectricUtilityRevenue",
  "OperatingLeaseLeaseIncome",                             # inmobiliarias (alquileres)
  "OperatingLeasesIncomeStatementLeaseRevenue",
  "RealEstateRevenueNet",
  "OilAndGasRevenue",
  "HealthCareOrganizationRevenue"
)

# Pedido a la SEC con reintentos. Devuelve estado "ok", "no_existe" o "fallo".
secGET <- function(url) {
  esperas <- c(5, 15)
  detalle <- "sin respuesta"
  for (i in 1:3) {
    Sys.sleep(SEC_PAUSA)
    r <- tryCatch(httr::GET(url, httr::user_agent(SEC_USER_AGENT), httr::timeout(60)),
                  error = function(e) e)
    if (inherits(r, "error")) {
      detalle <- conditionMessage(r)
    } else {
      st <- httr::status_code(r)
      if (st == 200) {
        j <- tryCatch(jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8")),
                      error = function(e) NULL)
        if (!is.null(j)) return(list(estado = "ok", datos = j))
        detalle <- "respuesta ilegible"
      } else if (st == 404) {
        return(list(estado = "no_existe", datos = NULL))
      } else {
        detalle <- paste("HTTP", st)
      }
    }
    if (i < 3) Sys.sleep(esperas[i])
  }
  list(estado = "fallo", datos = NULL, detalle = detalle)
}

# Ticker -> CIK (numero de empresa en la SEC)
getMapaCIK <- function() {
  j <- secGET("https://www.sec.gov/files/company_tickers.json")$datos
  if (is.null(j) || length(j) == 0) return(NULL)
  data.frame(Especie = gsub("\\.", "-", toupper(vapply(j, function(x) as.character(x$ticker), ""))),
             cik     = vapply(j, function(x) as.numeric(x$cik_str), 0),
             stringsAsFactors = FALSE) %>%
    distinct(Especie, .keep_all = TRUE)
}

# Arma la serie trimestral (y calcula el 4to trimestre a partir del anual)
armarTrimestres <- function(h) {
  q <- h %>% filter(dias >= 80, dias <= 100) %>% select(start, end, val)
  a <- h %>% filter(dias >= 350, dias <= 380) %>% select(start, end, val)
  for (k in seq_len(nrow(a))) {
    if (any(abs(as.numeric(q$end - a$end[k])) <= 5)) next      # ese trimestre ya esta
    dentro <- q %>% filter(start >= a$start[k] - 5, end <= a$end[k] + 5)
    if (nrow(dentro) == 3) {
      q <- bind_rows(q, data.frame(start = max(dentro$end) + 1, end = a$end[k],
                                   val = a$val[k] - sum(dentro$val)))
    }
  }
  q %>% arrange(end) %>% distinct(end, .keep_all = TRUE)
}

# Hechos de un concepto (tabla de la SEC) -> serie trimestral
hechosATrimestres <- function(u) {
  if (is.null(u)) return(NULL)
  h <- as.data.frame(u)
  if (!all(c("start", "end", "val", "form", "filed") %in% names(h))) return(NULL)
  h <- h %>%
    filter(form %in% c("10-Q", "10-K", "10-Q/A", "10-K/A", "10-QT", "10-KT"),
           !is.na(start), !is.na(val)) %>%
    mutate(start = as.Date(start), end = as.Date(end), filed = as.Date(filed),
           val = as.numeric(val), dias = as.numeric(end - start)) %>%
    arrange(desc(filed)) %>%                       # si se corrigio un dato, gana el ultimo
    distinct(start, end, .keep_all = TRUE)
  if (nrow(h) == 0) return(NULL)
  armarTrimestres(h)
}

# Todas las series de ventas posibles de una empresa
seriesVentas <- function(gaap) {
  series <- list()
  for (cc in intersect(SEC_CONCEPTOS_VENTAS, names(gaap))) {
    q <- tryCatch(hechosATrimestres(gaap[[cc]]$units$USD), error = function(e) NULL)
    if (!is.null(q) && nrow(q) >= 5) series[[cc]] <- q
  }
  # Bancos: no informan "ventas"; sus ingresos son intereses netos + otros ingresos
  if (all(c("InterestIncomeExpenseNet", "NoninterestIncome") %in% names(gaap))) {
    a <- tryCatch(hechosATrimestres(gaap$InterestIncomeExpenseNet$units$USD), error = function(e) NULL)
    b <- tryCatch(hechosATrimestres(gaap$NoninterestIncome$units$USD), error = function(e) NULL)
    if (!is.null(a) && !is.null(b)) {
      ab <- inner_join(a, b, by = "end", suffix = c(".a", ".b")) %>%
        transmute(start = start.a, end, val = val.a + val.b)
      if (nrow(ab) >= 5) series[["Banco: intereses netos + otros ingresos"]] <- ab
    }
  }
  series
}

# Baja TODOS los datos de la empresa en un solo pedido y elige la mejor serie
ventasEmpresa <- function(cik) {
  sin <- function(motivo, concepto = "") list(q = NULL, concepto = concepto, motivo = motivo)
  r <- secGET(sprintf("https://data.sec.gov/api/xbrl/companyfacts/CIK%010d.json", as.integer(cik)))
  if (r$estado == "fallo")     return(sin(paste("la SEC no respondio:", r$detalle)))
  if (r$estado == "no_existe") return(sin("no tiene balances cargados en la SEC"))
  hechos <- r$datos$facts
  if (is.null(hechos$`us-gaap`)) {
    if (!is.null(hechos$`ifrs-full`)) return(sin("extranjera: presenta en formato internacional (IFRS), sin trimestrales"))
    return(sin("no tiene balances en formato EE.UU."))
  }
  series <- seriesVentas(hechos$`us-gaap`)
  if (length(series) == 0) return(sin("no se encontro la linea de ventas trimestral"))
  fin   <- vapply(series, function(q) as.numeric(max(q$end)), 0)
  ult   <- vapply(series, function(q) as.numeric(tail(q$val, 1)), 0)
  mejor <- names(series)[order(-fin, -ult)[1]]
  q     <- series[[mejor]]
  if (max(q$end) < Sys.Date() - VENTAS_MAX_DIAS) {
    return(sin(paste("ultimo trimestre informado:", max(q$end)), mejor))
  }
  list(q = q, concepto = mejor, motivo = "")
}

# Crecimiento interanual de cada trimestre (contra el mismo trimestre del año anterior)
crecimientoInteranual <- function(q) {
  if (is.null(q) || nrow(q) < 5) return(NULL)
  q$yoy <- NA_real_
  for (i in seq_len(nrow(q))) {
    dif <- abs(as.numeric(q$end - (q$end[i] - 364)))
    j <- which(dif <= 20)
    if (length(j) == 0) next
    j <- j[which.min(dif[j])]
    if (isTRUE(q$val[j] > 0)) q$yoy[i] <- (q$val[i] / q$val[j] - 1) * 100
  }
  q
}

# Clasifica los ultimos crecimientos (del mas viejo al mas nuevo)
clasificarVentas <- function(g) {
  n <- length(g)
  if (n < 2) return("Sin datos")
  g3 <- g[n]; g2 <- g[n - 1]; d <- g3 - g2
  previos <- g[max(1, n - 2):(n - 1)]
  if (g3 > 1 && any(previos <= 0))                         return("2 - Giro a positivo")
  if (n >= 3 && g3 > 0 && d > 1 && (g2 - g[n - 2]) > 1)    return("1 - Acelerando")
  if (d > 2)                                               return("3 - Mejorando")
  if (g3 < 0 && d <= 0)                                    return("6 - Cayendo")
  if (d < -2)                                              return("5 - Frenando")
  "4 - Estable"
}

resumenVentas <- function(v) {
  vacio <- function(motivo) data.frame(TendenciaVentas = "Sin datos", VentasYoY = NA_real_,
                                       AceleracionVentas = NA_real_, HistVentasYoY = "",
                                       FechaBalance = NA_character_, ConceptoVentas = v$concepto,
                                       MotivoSinDatos = motivo, stringsAsFactors = FALSE)
  if (is.null(v$q)) return(vacio(v$motivo))
  q <- crecimientoInteranual(v$q)
  if (is.null(q)) return(vacio("menos de 5 trimestres de historia"))
  ult <- tail(q[!is.na(q$yoy), ], 4)
  if (nrow(ult) < 2) return(vacio("menos de 2 trimestres comparables con el año anterior"))
  g <- ult$yoy
  data.frame(TendenciaVentas   = clasificarVentas(g),
             VentasYoY         = round(g[length(g)], 1),
             AceleracionVentas = round(g[length(g)] - g[length(g) - 1], 1),
             HistVentasYoY     = paste(sprintf("%.0f%%", g), collapse = " | "),   # la flecha se pone al mostrar
             FechaBalance      = as.character(max(ult$end)),
             ConceptoVentas    = v$concepto,
             MotivoSinDatos    = "",
             stringsAsFactors = FALSE)
}

leerCacheVentas <- function() {
  if (!file.exists(VENTAS_CACHE)) return(NULL)
  cache <- tryCatch(read.csv(VENTAS_CACHE, stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                             colClasses = "character", na.strings = c("NA", "")),
                    error = function(e) NULL)
  if (is.null(cache) || nrow(cache) == 0) return(NULL)
  for (col in c("ConceptoVentas", "MotivoSinDatos", "Version", "HistVentasYoY"))
    if (!col %in% names(cache)) cache[[col]] <- NA_character_
  cache %>% mutate(VentasYoY = as.numeric(VentasYoY),
                   AceleracionVentas = as.numeric(AceleracionVentas),
                   HistVentasYoY = coalesce(HistVentasYoY, ""),
                   MotivoSinDatos = coalesce(MotivoSinDatos, ""),
                   ConceptoVentas = coalesce(ConceptoVentas, ""))
}

calcularVentas <- function(especies) {
  cache  <- leerCacheVentas()
  fresco <- !is.null(cache) &&
    all(cache$Version %in% as.character(VENTAS_VERSION)) &&
    as.numeric(Sys.Date() - min(as.Date(cache$FechaDescarga))) < VENTAS_DIAS_CACHE
  faltan <- if (fresco) setdiff(especies, cache$Especie) else especies
  if (length(faltan) == 0) {
    log_consola("Balances: se usan los guardados del", min(cache$FechaDescarga))
    return(cache)
  }

  log_consola("Bajando balances de la SEC para", length(faltan), "tickers")
  mapa <- getMapaCIK()
  if (is.null(mapa)) {
    log_consola("No se pudo leer la lista de empresas de la SEC: se usan los balances guardados")
    return(cache)
  }
  inicio <- Sys.time()
  nuevos <- bind_rows(lapply(seq_along(faltan), function(i) {
    esp <- faltan[i]
    cik <- mapa$cik[match(esp, mapa$Especie)]
    v <- if (is.na(cik)) list(q = NULL, concepto = "", motivo = "no figura en la SEC (extranjera)")
         else tryCatch(ventasEmpresa(cik),
                       error = function(e) list(q = NULL, concepto = "", motivo = paste("error:", conditionMessage(e))))
    res <- tryCatch(resumenVentas(v), error = function(e)
      resumenVentas(list(q = NULL, concepto = "", motivo = paste("error:", conditionMessage(e)))))
    log_progreso(i, length(faltan), inicio,
                 paste0("balance ", esp, ": ", res$TendenciaVentas,
                        if (res$MotivoSinDatos != "") paste0(" (", res$MotivoSinDatos, ")")
                        else paste0(" [", res$ConceptoVentas, "]")))
    cbind(data.frame(Especie = esp, stringsAsFactors = FALSE), res)
  })) %>%
    mutate(FechaDescarga = as.character(Sys.Date()), Version = as.character(VENTAS_VERSION))

  # Resumen en el log: cuantas con datos y por que faltan las demas
  log_consola("Balances listos:", sum(nuevos$TendenciaVentas != "Sin datos"), "con datos de", nrow(nuevos))
  motivos <- sort(table(sub(":.*", "", nuevos$MotivoSinDatos[nuevos$MotivoSinDatos != ""])), decreasing = TRUE)
  for (m in names(motivos)) log_consola("   Sin datos -", m, ":", motivos[[m]])

  con_datos <- function(df) sum(df$TendenciaVentas != "Sin datos", na.rm = TRUE)
  if (fresco) {
    resultado <- bind_rows(cache, nuevos)
  } else if (!is.null(cache) && con_datos(nuevos) < 0.5 * con_datos(cache)) {
    # La SEC fallo a mitad de camino: mejor quedarse con lo guardado
    log_consola("La descarga de la SEC vino incompleta: se usan los balances guardados")
    return(cache)
  } else {
    resultado <- nuevos
  }
  dir.create(dirname(VENTAS_CACHE), showWarnings = FALSE, recursive = TRUE)
  write.csv(resultado, VENTAS_CACHE, row.names = FALSE, fileEncoding = "UTF-8")
  resultado
}

# =====================================================================
#  CORRECCION EN ALZA
#  ---------------------------------------------------------------------
#  Detecta acciones que VENIAN SUBIENDO FUERTE y estan CORRIGIENDO (caso
#  CAT: +42% en el año y -30% desde el pico), para separarlas de las que se
#  estan cayendo de verdad.
#
#  Solo se calcula si se cumplen las dos condiciones (si no: "No aplica"):
#   - Venia bien:   IndiceFuerza365 > 0 y la media de 200 sigue subiendo.
#   - Esta corrigiendo: entre 8% y 40% abajo del maximo de 52 semanas, y el
#     ultimo mes o trimestre en baja (IndiceFuerza30 o IndiceFuerza90 < 0).
#
#  Puntaje de 0 a 100:
#   - Tendencia previa (30): que tan fuerte venia (IndiceFuerza365, tope 60).
#   - Profundidad (25): maximo si corrigio entre 12% y 25%; baja a 0 en 8% y 40%.
#   - Soporte (20): maximo si el precio esta entre -8% y +8% de la media de 200
#     (donde suelen rebotar); baja a 0 en -18% y +25%.
#   - Ventas (15): Acelerando 15, Giro a positivo 12, Mejorando 10, Estable 7,
#     Sin datos 5, Frenando 3, Cayendo 0.
#   - Rebote (10): IndiceFuerza7 > 0 -> 10; entre -2 y 0 -> 5; menos -> 0.
#  Clase: 80+ "1 - Oportunidad", 65-79 "2 - Vigilar", menos "3 - Debil".
# =====================================================================
puntajeTrapecio <- function(x, a0, a1, b1, b0) {
  # 0 hasta a0, sube hasta 1 en a1, 1 entre a1 y b1, baja a 0 en b0
  y <- ifelse(x <= a0 | x >= b0, 0,
       ifelse(x >= a1 & x <= b1, 1,
       ifelse(x < a1, (x - a0) / (a1 - a0), (b0 - x) / (b0 - b1))))
  ifelse(is.na(y), 0, y)
}

agregarIndiceCorreccion <- function(data) {
  caida <- -data$PctMax52
  venia <- !is.na(data$IndiceFuerza365) & data$IndiceFuerza365 > 0 &
           !is.na(data$PendMedia200) & data$PendMedia200 > 0
  corrige <- !is.na(caida) & caida >= 8 & caida <= 40 &
             ((!is.na(data$IndiceFuerza30) & data$IndiceFuerza30 < 0) |
              (!is.na(data$IndiceFuerza90) & data$IndiceFuerza90 < 0))
  aplica <- venia & corrige & data$Rubro != "ETF"

  p_tendencia   <- pmin(pmax(data$IndiceFuerza365, 0), 60) / 60 * 30
  p_profundidad <- puntajeTrapecio(caida, 8, 12, 25, 40) * 25
  p_soporte     <- puntajeTrapecio(data$PctMedia200, -18, -8, 8, 25) * 20
  p_ventas      <- unname(c("1 - Acelerando" = 15, "2 - Giro a positivo" = 12, "3 - Mejorando" = 10,
                            "4 - Estable" = 7, "5 - Frenando" = 3, "6 - Cayendo" = 0,
                            "Sin datos" = 5)[data$TendenciaVentas])
  p_ventas[is.na(p_ventas)] <- 5
  p_rebote      <- ifelse(!is.na(data$IndiceFuerza7) & data$IndiceFuerza7 > 0, 10,
                   ifelse(!is.na(data$IndiceFuerza7) & data$IndiceFuerza7 > -2, 5, 0))

  total <- round(p_tendencia + p_profundidad + p_soporte + p_ventas + p_rebote)
  data$IndiceCorreccion <- ifelse(aplica, total, NA_real_)
  data$CorreccionAlza   <- ifelse(!aplica, "No aplica",
                           ifelse(total >= 80, "1 - Oportunidad",
                           ifelse(total >= 65, "2 - Vigilar", "3 - Debil")))
  data
}



# =====================================================================
#  EJECUCION (con cache del dia)
#  ---------------------------------------------------------------------
#  Bajar las ~840 acciones tarda 25-30 minutos. Si ya se bajaron para el
#  ultimo cierre de Wall Street y R/etl.R no cambio, se reusa lo guardado en
#  _cache/datos_dia.rds y la pagina se arma en 1-2 minutos. Sirve para los
#  commits del mismo dia (por ejemplo, cambiar index.Rmd).
#
#  Se vuelve a bajar todo cuando:
#   - hay un cierre nuevo (la actualizacion diaria de las 19:30), o
#   - cambio R/etl.R (tickers nuevos, otro calculo, etc.), o
#   - FORZAR_DESCARGA es TRUE.
# =====================================================================
FORZAR_DESCARGA <- FALSE
CACHE_DIA       <- "_cache/datos_dia.rds"

# Fecha del ultimo cierre de Wall Street (16:30 NY con margen; fin de semana -> viernes)
fechaMercado <- function() {
  ny <- with_tz(Sys.time(), "America/New_York")
  d  <- as.Date(format(ny, "%Y-%m-%d"))
  if (hour(ny) * 60 + minute(ny) < 16 * 60 + 30) d <- d - 1
  while (format(d, "%u") %in% c("6", "7")) d <- d - 1
  d
}

procesarTodo <- function() {
  sp500 <- getOrder(getRawData('^GSPC'))
  data  <- crear_dataframe(datos, sp500)

  data <- data %>%
    mutate(Rubro = unname(rubros[Especie]),
           Rubro = ifelse(is.na(Rubro), "Otro", Rubro)) %>%
    relocate(Rubro, .after = Nombre)

  # Correlacion de todas contra todas (pestana "Correlaciones")
  log_consola("Calculando correlaciones de todas contra todas...")
  corr_resumen <- calcularCorrResumen(precios_hist, datos)
  corr_resumen <- corr_resumen %>%
    left_join(data %>% select(Especie, Rubro), by = "Especie") %>%
    relocate(Rubro, .after = Nombre)

  ventas <- tryCatch(calcularVentas(data$Especie[data$Rubro != "ETF"]),
                     error = function(e) { log_consola("Error en balances:", conditionMessage(e)); NULL })
  if (is.null(ventas) || nrow(ventas) == 0) {
    ventas <- data.frame(Especie = character(), TendenciaVentas = character(), VentasYoY = numeric(),
                         AceleracionVentas = numeric(), HistVentasYoY = character(),
                         FechaBalance = character(), stringsAsFactors = FALSE)
  }
  data <- data %>%
    left_join(ventas %>%
                mutate(VentasYoY = as.numeric(VentasYoY),
                       AceleracionVentas = as.numeric(AceleracionVentas),
                       FechaBalance = as.character(FechaBalance)) %>%
                select(Especie, TendenciaVentas, VentasYoY, AceleracionVentas,
                       HistVentasYoY, FechaBalance) %>%
                distinct(Especie, .keep_all = TRUE),
              by = "Especie") %>%
    mutate(TendenciaVentas = coalesce(TendenciaVentas, "Sin datos"),
           HistVentasYoY   = gsub(" | ", " \u2192 ", coalesce(HistVentasYoY, ""), fixed = TRUE))

  # Correccion en alza (usa precios + ventas, por eso va al final)
  data <- agregarIndiceCorreccion(data)

  list(data = data, corr_resumen = corr_resumen, errores_tickers = errores_tickers)
}

huella_etl <- unname(tools::md5sum("R/etl.R"))
fecha_merc <- fechaMercado()
cache_dia  <- NULL
if (!FORZAR_DESCARGA && file.exists(CACHE_DIA)) {
  cache_dia <- tryCatch(readRDS(CACHE_DIA), error = function(e) NULL)
}

if (!is.null(cache_dia) && identical(cache_dia$fecha_mercado, fecha_merc) &&
    identical(cache_dia$huella, huella_etl)) {
  log_consola("Usando los datos guardados del cierre", format(fecha_merc), "(bajados el",
              cache_dia$guardado, "): no se baja nada")
  res <- cache_dia
} else {
  res <- procesarTodo()
  # Solo se guarda si la descarga salio bien (al menos la mitad de los tickers)
  if (nrow(res$data) >= 0.5 * length(datos)) {
    res$fecha_mercado <- fecha_merc
    res$huella        <- huella_etl
    res$guardado      <- format(with_tz(Sys.time(), "America/Argentina/Buenos_Aires"), "%Y-%m-%d %H:%M")
    dir.create(dirname(CACHE_DIA), showWarnings = FALSE, recursive = TRUE)
    saveRDS(res, CACHE_DIA)
  }
}

data            <- res$data
corr_resumen    <- res$corr_resumen
errores_tickers <- res$errores_tickers
log_consola("Datos listos:", nrow(data), "tickers. Armando las tablas del dashboard...")


# Ordenar por fuerza relativa (mayor -> menor). Descomenta la que uses:
# data <- data[order(-data$IndiceFuerza90), ]
# data <- data[order(-data$IndiceFuerza365), ]
