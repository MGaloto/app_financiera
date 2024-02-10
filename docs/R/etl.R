


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


data = getRawData("MELi")



probTwoDays = function(data) {
  casos = c()
  frecuencia = c()
  count = 0
  vectorPrice = data$precio
  vectorPrice = c(diff(vectorPrice)/vectorPrice[-length(vectorPrice)] * 100)
  
  tryCatch({
    
    for (v in vectorPrice) {
      valorActual = v
      valorPosterior = vectorPrice[count+1]
      valorPosterior2 = vectorPrice[count+2]
      
      if (valorActual < 0 & valorPosterior < 0){
        lenCasos = length(casos)
        casos[lenCasos+1] = 1
        
        if(valorPosterior2 > 0) {
          lenFrecuencia = length(frecuencia)
          frecuencia[lenFrecuencia+1] = 1
        }
      }
    
      count = count + 1
    }
    
  }, error = function(e) {
      cat("Mensaje de error:", conditionMessage(e), "\n")
  })
  
  return(round((sum(frecuencia)/sum(casos)),2))
}



probThreeDays = function(data) {
  casos = c()
  frecuencia = c()
  count = 0
  vectorPrice = data$precio
  vectorPrice = c(diff(vectorPrice)/vectorPrice[-length(vectorPrice)] * 100)
  
  tryCatch({
    
    for (v in vectorPrice) {
      valorActual = v
      valorPosterior = vectorPrice[count+1]
      valorPosterior2 = vectorPrice[count+2]
      valorPosterior3 = vectorPrice[count+3]
      
      if (valorActual < 0 & valorPosterior < 0 & valorPosterior2 < 0){
        lenCasos = length(casos)
        casos[lenCasos+1] = 1
        
        if(valorPosterior3 > 0) {
          lenFrecuencia = length(frecuencia)
          frecuencia[lenFrecuencia+1] = 1
        }
      }
      
      count = count + 1
    }
    
  }, error = function(e) {
    cat("Mensaje de error:", conditionMessage(e), "\n")
  })
  
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



getAnalisisTrend = function(vector) {
  
  negativos = c()
  positivos = c()
  
  count = 0
  for (v in vector){
    count = count + 1
    if (v < 0){
      negativos[count] = v
    } else {
      positivos[count] = v
    }
  }
  
  negativos = na.omit(negativos)
  positivos = na.omit(positivos)
  
  if (length(positivos) == 4){
    result <- "1 - Alza"
  } else if (length(positivos) == 3) {
    result <- "2 - Alza Media"
  } else if (length(positivos) == 2) {
    result <- "3 - Neutro"
  } else if (length(positivos) == 1) {
    result <- "4 - Baja Media"
  } else {
    result <- "5 - Baja"
  }
  
  return(result)
  
}




getData = function(data, vector_trends, especie, accion_completa) {
  
  data <- getOrder(data)
  
  fecha_ultimo_maximo <- getFechaUltimoMaximo(data)
  
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
    VolumenTrend90Days = getVolumenTrend90Days(data)
    
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
  TX = "Ternium"
)

crear_dataframe <- function(acciones) {
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
      data = getData(raw_data, vector_trends, accion, accion_completa)
      
      dataframe_final <- rbind(dataframe_final, data)
      
    }, error = function(e) {
      cat("Error en la acción:", accion, "\n")
      cat("Mensaje de error:", conditionMessage(e), "\n")
    })
  }
  
  rownames(dataframe_final) <- NULL
  return(dataframe_final)
}


acciones <- c(MELI="MELI")

data <- crear_dataframe(datos)


