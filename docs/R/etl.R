


library(quantmod)
library(readr)  
library(dplyr) 
library(modeest)
library(readxl)
library(lubridate)
library(tidyverse)




fecha_hoy <- Sys.Date()
today <- as.Date(format(with_tz(fecha_hoy, tz = "America/Argentina/Buenos_Aires"), "%Y-%m-%d"))

# MELI=na.omit(as.data.frame(
#   getSymbols(
#     "HD.BA" , src = 'yahoo', auto.assign = F, from = "2020-01-01", to = Sys.Date(), periodicity = "daily"
#     )
#   ))
# 
# MELI$Fecha <- as.Date(row.names(MELI))
#   
# colnames(MELI) = c("Open", "High", "Low", "Close", "Volumen", "Ajustado", "Fecha")
# 
# MELI = MELI %>% select("Ajustado", "Fecha", "Volumen")
# 
# rownames(MELI) = 1:nrow(MELI)
# 
# row_ultimo_maximo = which.max(MELI[['Ajustado']])
# 
# distancia_ultimo_maximo = nrow(MELI) - row_ultimo_maximo
# 
# 
# colnames(MELI) = c("precio","fecha", "volumen")



getRawData = function(accion){
  
  raw_data = as.data.frame(
    getSymbols(
      accion , 
      src = 'yahoo', 
      auto.assign = F, 
      from = "2021-01-01", 
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


getUltVolumen = function(data) {
  volumen_actual = data$volumen[nrow(data)]
  return(volumen_actual)
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
    mean(cv365 + cv150 + cv90 + cv60 + cv30)
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




getData = function(data, vector_trends, especie) {
  
  data <- getOrder(data)
  
  fecha_ultimo_maximo <- getFechaUltimoMaximo(data)
  
  df <- data.frame(
    Especie = especie,
    Variacion = getVariation(data),
    InterAnual = getVariationInterAnual(data),
    PorcentajeRespectoAlMax = getVariationUltimoMaximo(data),
    DiasUltMax = getDiasUtimoMaximo(
      fecha_ultimo_maximo, today
    ),
    FechaUltMax = fecha_ultimo_maximo,
    TrendActual = getAnalisisTrend(vector_trends),
    TrendUltYear = getTrend365Days(data),
    CoefVariationPromedioAnual = getAnualVariationCoeff(data)
    
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
  ZI = "ZoomInfo Technologies Inc."
)

# 
# 
# cedears  <- c(
#   "AAL" = "AMERICAN AIRLIN...",
#   "AAPL" = "Apple",
#   "AAPLC" = "Apple",
#   "AAPLD" = "Apple",
#   "ABBV" = "AbbVie Inc.",
#   "ABBVD" = "Abbvie Inc.",
#   "ABEV" = "Ambev",
#   "ABNB" = "Airbnb, Inc.",
#   "ABT" = "Abbott Laborato...",
#   "ABTD" = "Abbott Laborato...",
#   "ADBE" = "Adobe Systems I...",
#   "ADGO" = "Adecoagro",
#   "ADI" = "Analog Devices",
#   "AEM" = "Agnico Eagle Mi...",
#   "AIG" = "American Intern...",
#   "AKO.B" = "EMBOTELLADORA A...",
#   "AMAT" = "Applied Materials",
#   "AMD" = "Advanced Micro ...",
#   "AMDD" = "Advanced Micro ...",
#   "AMGN" = "Amgen",
#   "AMX" = "America Movil",
#   "AMXD" = "America Movil",
#   "AMZN" = "Amazon",
#   "AMZNC" = "Amazon",
#   "AMZND" = "Amazon",
#   "AOCAD" = "Aluminum Corpor...",
#   "ARCO" = "Arcos Dorados H...",
#   "AUY" = "Yamana Gold Inc.",
#   "AVGO" = "Broadcom Inc.",
#   "AVGOD" = "Broadcom Inc.",
#   "AVY" = "Avery Dennison",
#   "AXP" = "American Express",
#   "AXPD" = "American Express",
#   "AZN" = "Astrazeneca",
#   "AZND" = "Astrazeneca",
#   "BA" = "Boeing",
#   "BA.C" = "Bank of America",
#   "BA.CD" = "Bank of America",
#   "BABA" = "Alibaba Group",
#   "BABAC" = "Alibaba Group",
#   "BABAD" = "Alibaba Group",
#   "BAD" = "Boeing",
#   "BB" = "Blackberry",
#   "BBD" = "Banco Bradesco ...",
#   "BBDC" = "Banco Bradesco ...",
#   "BBDD" = "Banco Bradesco ...",
#   "BBV" = "Banco Bilbao Vi...",
#   "BCS" = "Barclays",
#   "BHP" = "BHP Billiton Ltd.",
#   "BIDU" = "Baidu",
#   "BIDUD" = "Baidu",
#   "BIIB" = "Biogen Inc.",
#   "BIOX" = "Bioceres Crop S...",
#   "BIOXD" = "Bioceres Crop S...",
#   "BITF" = "Bitfarms Ltd.",
#   "BK" = "Bank of New Yor...",
#   "BMY" = "Bristol-Myers S...",
#   "BNG" = "Bunge Limited",
#   "BP" = "BP",
#   "BPD" = "BP",
#   "BRFS" = "BRF S.A.",
#   "BRKB" = "Berkshire Hathaway",
#   "BRKBD" = "Berkshire Hathaway",
#   "BSBR" = "Banco Santander...",
#   "C" = "Citigroup",
#   "C.D" = "Citigroup",
#   "CAAP" = "Corporación Amé...",
#   "CAH" = "Cardinal Health",
#   "CAHD" = "Cardinal Health",
#   "CAR" = "Avis Budget Gro...",
#   "CAT" = "Caterpillar",
#   "CATD" = "Caterpillar",
#   "CDE" = "Coeur Mining",
#   "CL" = "Colgate-Palmolive",
#   "COIN" = "COINBASE GLOBAL...",
#   "COST" = "Costco Wholesale",
#   "CRM" = "Salesforce.com ...",
#   "CS" = "Credit Suisse G...",
#   "CSCO" = "Cisco Systems",
#   "CSCOD" = "Cisco Systems",
#   "CVX" = "Chevron",
#   "CVXD" = "Chevron",
#   "CX" = "Cemex",
#   "DD" = "Dupont Corporation",
#   "DE" = "Deere",
#   "DEO" = "CEDEAR DIAGEO PL",
#   "DESP" = "Despegar",
#   "DESPD" = "Despegar",
#   "DISN" = "The Walt Disney...",
#   "DISND" = "The Walt Disney...",
#   "DOCU" = "DocuSign Inc.",
#   "DOCUD" = "Docusign Inc",
#   "DOW" = "Dow Inc.",
#   "EA" = "ELECTRONIC ARTS...",
#   "EBAY" = "eBay",
#   "EBAYD" = "eBay",
#   "EBR" = "Centrais Elétri...",
#   "EFX" = "Equifax Inc.",
#   "EFXD" = "Equifax Inc.",
#   "ERIC" = "LM Ericsson Tel...",
#   "ERJ" = "Embraer S.A.",
#   "ERJD" = "Embraer S.A.",
#   "ETSY" = "Etsy Inc.",
#   "ETSYD" = "Etsy Inc",
#   "F" = "Ford Motor Company",
#   "FCX" = "Freeport-Mcmora...",
#   "FDX" = "FedEx",
#   "FDXD" = "FedEx",
#   "FMX" = "Femsa",
#   "FMXD" = "Femsa",
#   "FSLR" = "First Solar",
#   "GE" = "General Electric",
#   "GEC" = "General Electric",
#   "GFI" = "Gold Fields",
#   "GGB" = "Gerdau",
#   "GILD" = "Gilead Sciences",
#   "GILDC" = "Gilead Sciences",
#   "GILDD" = "Gilead Sciences",
#   "GLOB" = "Globant",
#   "GLOBD" = "Globant",
#   "GLW" = "Corning",
#   "GM" = "General Motors Co.",
#   "GOGLD" = "Google",
#   "GOLD" = "Barrick Gold",
#   "GOLDC" = "Barrick Gold",
#   "GOLDD" = "Barrick Gold",
#   "GOOGL" = "Google",
#   "GPRK" = "Geopark Ltd.",
#   "GRMN" = "Garmin LTD.",
#   "GS" = "Goldman Sachs",
#   "GSD" = "Goldman Sachs",
#   "GSK" = "GlaxoSmithKline",
#   "GSKC" = "GlaxoSmithKline",
#   "GSKD" = "GlaxoSmithKline",
#   "HAL" = "Halliburton Co.",
#   "HD" = "Home Depot",
#   "HL" = "Hecla Mining",
#   "HMC" = "Honda Motor",
#   "HMY" = "Harmony Gold",
#   "HMYD" = "Harmony Gold",
#   "HOG" = "Harley Davidson",
#   "HON" = "Honeywell Inter...",
#   "HPQ" = "Hewlett-Packard",
#   "HSBC" = "Hsbc Holdings",
#   "HSY" = "Hershey",
#   "HUT" = "Hut 8 Mining Corp.",
#   "HWM" = "Howmet Aerospace",
#   "HWMD" = "Howmet Aerospace",
#   "IBM" = "Ibm",
#   "IBMD" = "IBM",
#   "INTC" = "Intel",
#   "INTCD" = "Intel",
#   "IP" = "International P...",
#   "ITUB" = "Banco Itaú Unib...",
#   "ITUBD" = "Banco Itaú Unib...",
#   "JD" = "Jd.Com",
#   "JMIA" = "Jumia Technolog...",
#   "JNJ" = "Johnson & Johnson",
#   "JNJC" = "Johnson & Johnson",
#   "JNJD" = "Johnson & Johnson",
#   "JPM" = "Jpmorgan Chase ...",
#   "JPMD" = "JPMorgan Chase ...",
#   "KMB" = "Kimberly-Clark",
#   "KMBC" = "Kimberly-Clark",
#   "KO" = "The Coca-Cola C...",
#   "KOC" = "The Coca-Cola C...",
#   "KOD" = "The Coca-Cola C...",
#   "LLY" = "Eli Lilly",
#   "LMT" = "Lockheed Martin",
#   "LMTD" = "Lockheed Martin",
#   "LRCX" = "LAM RESEARCH CORP.",
#   "LVS" = "Las Vegas Sands",
#   "LYG" = "Lloyds Banking ...",
#   "MA" = "Mastercard Inc.",
#   "MAD" = "Mastercard",
#   "MCD" = "McDonald's",
#   "MCDD" = "Mcdonald's",
#   "MDT" = "Medtronic",
#   "MELI" = "MercadoLibre",
#   "MELIC" = "MercadoLibre",
#   "MELID" = "MercadoLibre",
#   "META" = "Meta Platforms Inc",
#   "METAC" = "Meta Platforms Inc",
#   "METAD" = "Meta Platforms Inc",
#   "MFG" = "MIZUHO FINANCIA...",
#   "MMC" = "Marsh & McLennan",
#   "MMM" = "3M",
#   "MMMD" = "3M",
#   "MO" = "Altria Group",
#   "MOD" = "Altria Group",
#   "MOS" = "The Mosaic Co",
#   "MRK" = "Merck",
#   "MRKD" = "Merck",
#   "MSFT" = "Microsoft",
#   "MSFTC" = "Microsoft",
#   "MSFTD" = "Microsoft",
#   "MSI" = "Motorola",
#   "MSTR" = "MicroStrategy I...",
#   "MU" = "Micron Technolo...",
#   "NEM" = "Newmont Mining ...",
#   "NEMD" = "Newmont Mining ...",
#   "NFLX" = "Netflix",
#   "NFLXD" = "Netflix",
#   "NGG" = "National Grid",
#   "NIO" = "NIO INC.",
#   "NKE" = "Nike",
#   "NKED" = "Nike",
#   "NOKA" = "Nokia",
#   "NTCO" = "Cedear Natura &...",
#   "NTES" = "Netease, Inc.",
#   "NUE" = "Nucor",
#   "NVDA" = "Nvidia",
#   "NVDAD" = "Nvidia",
#   "NVS" = "Novartis Ag",
#   "NVSD" = "Novartis AG",
#   "OGZD" = "Gazprom",
#   "OGZDD" = "Gazprom",
#   "ORAN" = "Orange",
#   "ORCL" = "Oracle",
#   "OXY" = "Occidental Petr...",
#   "PAAS" = "Pan American Si...",
#   "PANW" = "Palo Alto Netwo...",
#   "PBI" = "Pitney Bowes",
#   "PBR" = "Petroleo Brasil...",
#   "PBRD" = "Petroleo Brasil...",
#   "PCAR" = "PACCAR",
#   "PEP" = "PepsiCo",
#   "PFE" = "Pfizer",
#   "PFED" = "Pfizer",
#   "PG" = "Procter & Gamble",
#   "PGD" = "Procter & Gamble",
#   "PHG" = "Phillips Electr...",
#   "PKS" = "Posco Holdings ...",
#   "PSX" = "Phillips 66",
#   "PSXD" = "Phillips 66",
#   "PYPL" = "Paypal Holdings...",
#   "PYPLD" = "Paypal Holdings...",
#   "QCOM" = "Qualcomm",
#   "QCOMC" = "Qualcomm",
#   "QCOMD" = "Qualcomm",
#   "RBLX" = "Roblox Corporation",
#   "RIO" = "Rio Tinto",
#   "RIOD" = "Rio Tinto",
#   "RTX" = "United Technolo...",
#   "SAN" = "Banco Santander...",
#   "SAP" = "SAP",
#   "SATL" = "Satellogic Inc.",
#   "SBS" = "COMPANHIA DE SA...",
#   "SBUX" = "Starbucks",
#   "SCCO" = "Southern Copper",
#   "SE" = "Sea Ltd.",
#   "SHEL" = "Shell Plc",
#   "SHOP" = "Shopify Inc.",
#   "SHOPD" = "Shopify Inc.",
#   "SI" = "Silvergate Capi...",
#   "SID" = "Companhia Sider...",
#   "SLB" = "Schlumberger",
#   "SLBD" = "Schlumberger",
#   "SNA" = "Snap-On",
#   "SNAP" = "Snap Inc.",
#   "SNOW" = "Snowflake Inc.",
#   "SNOWD" = "Snowflake Inc",
#   "SONY" = "Sony Corporation",
#   "SPGI" = "S&P GLOBAL INC.",
#   "SPOT" = "Spotify Technol...",
#   "SPOTD" = "Spotify Technol...",
#   "SQ" = "Square Inc.",
#   "SQD" = "Square Inc",
#   "SUZ" = "Suzano",
#   "SYY" = "Sysco",
#   "T" = "At&T",
#   "TD" = "AT&T",
#   "TEFO" = "Telefonica Sa",
#   "TEN" = "Tenaris",
#   "TEND" = "Tenaris",
#   "TGT" = "Target Corporation",
#   "TIMB" = "Tim Participaçõ...",
#   "TM" = "Toyota Motors",
#   "TMD" = "Toyota Motors",
#   "TMO" = "Thermo Fisher S...",
#   "TRIP" = "Tripadvisor Inc.",
#   "TSLA" = "Tesla",
#   "TSLAC" = "Tesla",
#   "TSLAD" = "Tesla",
#   "TSM" = "Taiwan Semicond...",
#   "TTE" = "Total",
#   "TV" = "Grupo Televisa",
#   "TWLO" = "TWILIO INC.",
#   "TWTR" = "Twitter",
#   "TWTRD" = "Twitter",
#   "TXN" = "Texas Instruments",
#   "TXR" = "Ternium",
#   "TXRD" = "Ternium",
#   "UAL" = "United Airlines...",
#   "UBER" = "Uber Technologi...",
#   "UGP" = "Ultrapar Partic...",
#   "UL" = "Unilever",
#   "UNH" = "UnitedHealth Gr...",
#   "UNHD" = "UnitedHealth Gr...",
#   "UNP" = "Union Pacific C...",
#   "UNPD" = "Union Pacific C...",
#   "UPST" = "Upstart Holding...",
#   "USB" = "CEDEAR U.S.Banco",
#   "V" = "Visa",
#   "VALE" = "Vale",
#   "VALED" = "Vale",
#   "VD" = "Visa",
#   "VIST" = "Vista Energy S....",
#   "VISTD" = "Vista Energy S....",
#   "VIV" = "Telefonica Brasil",
#   "VOD" = "Vodafone Group",
#   "VRSN" = "Verisign",
#   "VZ" = "Verizon Communi...",
#   "VZD" = "Verizon Communi...",
#   "WBA" = "Walgreens Boots...",
#   "WFC" = "Wells Fargo",
#   "WFCC" = "Wells Fargo",
#   "WFCD" = "Wells Fargo",
#   "WMT" = "Walmart",
#   "WMTD" = "Walmart",
#   "X" = "United States S...",
#   "XD" = "United States S...",
#   "XOM" = "Exxon Mobil",
#   "XOMD" = "Exxon Mobil",
#   "XP" = "XP INC.",
#   "XROX" = "Xerox",
#   "YELP" = "Yelp Inc",
#   "ZM" = "Zoom Video Comm...",
#   "ZMD" = "Zoom Video Comm...")


crear_dataframe <- function(acciones) {
  dataframe_final <- data.frame()
  
  for (accion in acciones) {
    print(accion)
    
    tryCatch({
      raw_data = getRawData(accion)
      vector_trends = c(
        getTrend365Days(raw_data), 
        getTrend150Days(raw_data), 
        getTrend90Days(raw_data), 
        getTrend60Days(raw_data)
      )
      data = getData(raw_data, vector_trends, accion)
      
      dataframe_final <- rbind(dataframe_final, data)
      
    }, error = function(e) {
      cat("Error en la acción:", accion, "\n")
      cat("Mensaje de error:", conditionMessage(e), "\n")
    })
  }
  
  rownames(dataframe_final) <- NULL
  return(dataframe_final)
}


acciones <- c("MELI", "AAPL", "TSLA", "NVDA", "AMZN", "MSFT", "KO", "BABA", "DIS", "COIN", "VIST",  "WMT", "PBR", "MCD", "AAL", "JPM", "GOLD", "YPF", "ABNB")



resultado <- crear_dataframe(acciones)


