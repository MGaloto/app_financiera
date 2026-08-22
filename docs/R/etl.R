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
  
  return(na.omit(raw_data))
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


getVariationInterAnual = function(data) {
  ultimo_precio <- data$precio[nrow(data)]
  anteultimo_precio <- data$precio[nrow(data) - 365]
  variacion <- round(((ultimo_precio - anteultimo_precio) / anteultimo_precio) * 100,2)
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


getVariationUltimoMaximo = function(data) {
  precio_actual = data$precio[nrow(data)]
  ultimo_maximio = max(data$precio)
  variacion = round(((ultimo_maximio - precio_actual) / precio_actual) * 100,2)
  return(variacion)
}


getVariationUltimoMinimo = function(data) {
  precio_actual = data$precio[nrow(data)]
  ultimo_minimo = min(data$precio)
  variacion = round(((precio_actual - ultimo_minimo) / ultimo_minimo) * 100,2)
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



getAnualVariationCoeff = function(data) {
  data365 <- data[(nrow(data)-365):nrow(data)-150, ]
  cv365 <- round(sd(data365$precio) / mean(data365$precio) * 100, 2)
  data150 <- data[(nrow(data)-150):nrow(data)-90, ]
  cv150 <- round(sd(data150$precio) / mean(data150$precio) * 100, 2)
  data90 <- data[(nrow(data)-90):nrow(data)-60, ]
  cv90 <- round(sd(data90$precio) / mean(data90$precio) * 100, 2)
  data60 <- data[(nrow(data)-60):nrow(data)-30, ]
  cv60 <- round(sd(data60$precio) / mean(data60$precio) * 100, 2)
  data30 <- data[(nrow(data)-30):nrow(data), ]
  cv30 <- round(sd(data30$precio) / mean(data30$precio) * 100, 2)
  return(
    round(mean(cv365 + cv150 + cv90 + cv60 + cv30),2)
  )
}

getHistoricTrend = function(data) {
  model <- lm(precio ~ fecha, data = data)
  pendiente <- round(coef(model)[2],2)
  return(pendiente)
}

getTrend365Days = function(data) {
  data <- tail(data, 365)
  model <- lm(precio ~ fecha, data = data)
  pendiente <- round(coef(model)[2],2)
  return(pendiente)
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
#  getTrend90Days / getVolumenTrend90Days: tail(data, dias) = ultimas N ruedas).
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


getTrend150Days = function(data) {
  data <- tail(data, 150)
  model <- lm(precio ~ fecha, data = data)
  pendiente <- round(coef(model)[2],2)
  return(pendiente)
}


getTrend90Days = function(data) {
  data <- tail(data, 90)
  model <- lm(precio ~ fecha, data = data)
  pendiente <- round(coef(model)[2],2)
  return(pendiente)
}


getVolumenPromedioMensual = function(data) {
  data <- tail(data, 90)
  promedio <- mean(data$volumen)
  promedio <- round(promedio,2)
  return(promedio)
}


getTrend60Days = function(data) {
  data <- tail(data, 60)
  model <- lm(precio ~ fecha, data = data)
  pendiente <- round(coef(model)[2],2)
  return(pendiente)
}


getAnalisisTrend <- function(vector) {
  
  negativos <- discard(vector, ~.x >= 0)
  positivos <- discard(vector, ~.x < 0)
  
  negativos <- na.omit(negativos)
  positivos <- na.omit(positivos)
  
  result <- case_when(
    length(positivos) == 4 ~ "1 - Alza",
    length(positivos) == 3 ~ "2 - Alza Media",
    length(positivos) == 2 ~ "3 - Neutro",
    length(positivos) == 1 ~ "4 - Baja Media",
    TRUE ~ "5 - Baja"
  )
  
  return(result)
  
}



getCorr = function(data_uno, data_dos){
  tryCatch({
    data_uno_1000_rows <- tail(data_uno, 1000)
    data_dos_1000_rows <- tail(data_dos, 1000)
    correlacion <- cor(data_uno_1000_rows$precio, data_dos_1000_rows$precio)
    cor=round(correlacion,2)
  }, error = function(e1) {
    tryCatch({
      data_uno_500_rows <- tail(data_uno, 500)
      data_dos_500_rows <- tail(data_dos, 500)
      correlacion <- cor(data_uno_500_rows$precio, data_dos_500_rows$precio)
      cor = round(correlacion,2)
    }, error = function(e2) {
      data_uno_200_rows <- tail(data_uno, 200)
      data_dos_200_rows <- tail(data_dos, 200)
      correlacion <- cor(data_uno_200_rows$precio, data_dos_200_rows$precio)
      cor = round(correlacion,2)
    })
  })
  return(cor)
}



sp500 = getOrder(getRawData('^GSPC'))




getData = function(data, vector_trends, especie, accion_completa, sp500) {
  
  data <- getOrder(data)
  
  fecha_ultimo_maximo <- getFechaUltimoMaximo(data)
  
  # Descomposicion del indice de fuerza para cada ventana (una sola regresion c/u)
  det7   <- getIndiceFuerzaDetalle(data, 7, min_obs = 5)
  det30  <- getIndiceFuerzaDetalle(data, 30)
  det90  <- getIndiceFuerzaDetalle(data, 90)
  det365 <- getIndiceFuerzaDetalle(data, 365)
  
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
    TrendActual = getAnalisisTrend(vector_trends),
    TrendUltYear = getTrend365Days(data),
    TrendUlt90Days = getTrend90Days(data),
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
  CNHI = "CNH Industrial N.V.",
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
  GPS = "The Gap, Inc.",
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
  IMGN = "ImmunoGen, Inc.",
  INFY = "Infosys Limited",
  INTC = "Intel Corporation",
  IQ = "iQIYI, Inc.",
  ITUB = "Itaú Unibanco Holding S.A.",
  JCI = "Johnson Controls International plc",
  JD = "JD.com, Inc.",
  JNPR = "Juniper Networks, Inc.",
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
  MRO = "Marathon Oil Corporation",
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
  NYCB = "New York Community Bancorp, Inc.",
  O = "Realty Income Corporation",
  ON = "ON Semiconductor Corporation",
  OPEN = "Opendoor Technologies Inc.",
  ORCL = "Oracle Corporation",
  OSCR = "Oscar Health, Inc.",
  OXY = "Occidental Petroleum Corporation",
  PARA = "Paramount Global",
  PATH = "UiPath Inc.",
  PBR = "Petróleo Brasileiro S.A. - Petrobras",
  PCG = "PG&E Corporation",
  PDD = "PDD Holdings Inc.",
  PEAK = "Healthpeak Properties, Inc.",
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
  RCM = "R1 RCM Inc.",
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
  SQ = "Block, Inc.",
  SWN = "Southwestern Energy Company",
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
  WBA = "Walgreens Boots Alliance, Inc.",
  WBD = "Warner Bros. Discovery, Inc.",
  WFC = "Wells Fargo & Company",
  WIT = "Wipro Limited",
  WMB = "The Williams Companies, Inc.",
  XOM = "Exxon Mobil Corporation",
  XPEV = "XPeng Inc.",
  YPF = "YPF",
  ZI = "ZoomInfo Technologies Inc.",
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
  DESP = "Despegar.com, Corp.",
  BIOX = "Bioceres Crop Solutions Corp.",
  CAAP = "Corporación América Airports S.A.",
  TX = "Ternium"
)


crear_dataframe <- function(acciones, sp500) {
  dataframe_final <- data.frame()
  
  for (accion in names(acciones)) {
    print(accion)
    accion_completa = acciones[[accion]]
    
    tryCatch({
      raw_data = getRawData(accion)
      vector_trends = c(
        getTrend365Days(raw_data), 
        getTrend150Days(raw_data), 
        getTrend90Days(raw_data), 
        getTrend60Days(raw_data)
      )
      data = getData(raw_data, vector_trends, accion, accion_completa, sp500)
      
      dataframe_final <- rbind(dataframe_final, data)
      
    }, error = function(e) {
      cat("Error en la acción:", accion, "\n")
      cat("Mensaje de error:", conditionMessage(e), "\n")
    })
  }
  
  rownames(dataframe_final) <- NULL
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
