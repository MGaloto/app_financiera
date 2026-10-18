library(quantmod)
library(readr)  
library(dplyr) 
library(modeest)
library(readxl)
library(lubridate)
library(tidyverse)




fecha_hoy <- Sys.Date()
today <- as.Date(format(with_tz(fecha_hoy, tz = "America/Argentina/Buenos_Aires"), "%Y-%m-%d"))

getRawData = function(accion){
  
  raw_data = as.data.frame(
    getSymbols(
      accion , 
      src = 'yahoo', 
      auto.assign = F, 
      from = "2002-01-01", 
      to = Sys.Date(), 
      periodicity = "daily"
    )
  )
  
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



probTwoDays = function(data) {
  casos = c()
  frecuencia = c()
  count = 0
  vectorPrice = data$precio
  vectorPrice = c(diff(vectorPrice)/vectorPrice[-length(vectorPrice)] * 100)
  
  for (v in vectorPrice) {
    # v ya es vectorPrice[count+1] (dia actual); el dia siguiente es count+2
    valorActual     = v
    valorPosterior  = vectorPrice[count + 2]
    valorPosterior2 = vectorPrice[count + 3]
    
    # isTRUE() vuelve FALSE cualquier NA del borde -> ya no rompe el if
    if (isTRUE(valorActual < 0) && isTRUE(valorPosterior < 0)) {
      casos[length(casos) + 1] = 1
      if (isTRUE(valorPosterior2 > 0)) {
        frecuencia[length(frecuencia) + 1] = 1
      }
    }
    
    count = count + 1
  }
  
  return(round((sum(frecuencia)/sum(casos)),2))
}



probThreeDays = function(data) {
  casos = c()
  frecuencia = c()
  count = 0
  vectorPrice = data$precio
  vectorPrice = c(diff(vectorPrice)/vectorPrice[-length(vectorPrice)] * 100)
  
  for (v in vectorPrice) {
    # v ya es vectorPrice[count+1] (dia actual); los siguientes van corridos
    valorActual     = v
    valorPosterior  = vectorPrice[count + 2]
    valorPosterior2 = vectorPrice[count + 3]
    valorPosterior3 = vectorPrice[count + 4]
    
    if (isTRUE(valorActual < 0) && isTRUE(valorPosterior < 0) && isTRUE(valorPosterior2 < 0)) {
      casos[length(casos) + 1] = 1
      if (isTRUE(valorPosterior3 > 0)) {
        frecuencia[length(frecuencia) + 1] = 1
      }
    }
    
    count = count + 1
  }
  
  return(round((sum(frecuencia)/sum(casos)),2))
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



sp500 = getOrder(getRawData('^GSPC'))




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
    SMA50 = med$SMA50,
    SMA200 = med$SMA200,
    CoefVariationPromedioAnual = getAnualVariationCoeff(data),
    probTwoDays = probTwoDays(data),
    probThreeDays = probThreeDays(data),
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

  # --- Cartera preferida: los que faltaban ---
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

  for (accion in names(acciones)) {
    print(accion)
    accion_completa = acciones[[accion]]

    tryCatch({
      raw_data = getRawData(accion)
      if (nrow(raw_data) < min_ruedas) stop(paste("menos de", min_ruedas, "ruedas de historia"))
      data = getData(raw_data, accion, accion_completa, sp500)

      dataframe_final <- rbind(dataframe_final, data)

    }, error = function(e) {
      cat("Error en la acción:", accion, "\n")
      cat("Mensaje de error:", conditionMessage(e), "\n")
      errores <<- rbind(errores, data.frame(Especie = accion, Nombre = accion_completa,
                                            Error = conditionMessage(e)))
    })
  }

  rownames(dataframe_final) <- NULL
  errores_tickers <<- errores
  return(dataframe_final)
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
#  GENERACION DEL DATAFRAME FINAL
# =====================================================================
data <- crear_dataframe(datos, sp500)

# Ordenar por fuerza relativa (mayor -> menor). Descomenta la que uses:
# data <- data[order(-data$IndiceFuerza90), ]
# data <- data[order(-data$IndiceFuerza365), ]
