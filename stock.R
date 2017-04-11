library(quantmod)
library(plyr)
library(data.table)

Sys.setenv(TZ='GMT')

# mydata = Quandl("NSE/OIL", start_date="yyyy-mm-dd", end_date="yyyy-mm-dd")
# https://www.quandl.com/api/v3/datatables/WIKI/PRICES.json?ticker=<your_codes>&date=<your_date>&qopts.columns=<your_columns>&api_key=J9GL-ku7cowEyDrv8yBW


#Sys.timezone(location = TRUE)

#eventually use this to push and pull from db's

#quandl.api(path, http = c("GET", "PUT", "POST", "DELETE"), postdata = NULL, ...)

# Look up the first 3 results for 'Bitcoin' within the Quandl database:

# results <- Quandl.search(query = "Bitcoin", page = 1, silent = FALSE)

# gets financial data from 2 stocks (non time series related

# datatable <- Quandl.datatable("ZACKS/FC", ticker=c("AAPL","MSFT"))

# Retrieve all rows for ZACKS/FC for 'AAPL' and 'MSFT'.  Only return the ticker and per_end_date columns

# datatable_columns <- Quandl.datatable("ZACKS/FC", ticker=c("AAPL","MSFT"), qopts.columns=c("ticker", "per_end_date"))

# set external path to database on server to pull data and push data from users and central database network

# path<-"pathToData.html"

# get code from NYSE .csv

#myfunction <- disc(indicator, length){

#return(object)

#}

# qbase <- read.csv(file="~/JC/wind/q/databases.csv", header=TRUE, sep",", colClasses=c())

# data<-Quandl.database.bulk_download_to_file(database_code, filename, ...)

# Plot the chart with the help of candleChart()

# candleChart(stock)
# write.candleChart(stock)



# Consider a data frame D which has columns Y, X , Z, and W. Then the function call
# fit <- lm(Y ~ ., data = D) is equivalent to fit <- lm(Y ~ X + Z + W, data = D)


# Similarly, fit <- lm(Y ~ .-W, data = D) is equivalent to fit <- lm(Y ~ X + Z)
# and fit <- lm(Y ~ .*W, data = D) is equivalent to fit <- lm(Y ~ X + Z + W + X:W + Z:W)

# in build model, x is an R formula
# the function specify model can build the formula
# e.g. specifyModel(Next(OpCl(QQQQ)) ~ Lag(OpHi(QQQQ),0:3) + Hi(DIA))

# buildModel(x, method, training.per, ...)

bigBus <-function(folder)
  {
  
  return ( list.files(path = folder, pattern = "*.csv", all.files = T) )
  
} # grab the data main


pivots<-function(i,n){

  # peaks and valleys
  
  sp <- list()
  sv <- list()
  #dp <- list()
 # dv <- list()
  #dpv <- list()
  #deu <- list()
  pv <- list()
  ep <- list()
  ev <- list()
  # calculate first dervatives
  
  sp <- findPeaks(i$nd)
  cat("peaks")
  cat("\n")
  sv <- findValleys(i$nd)
  cat("valleys")
  cat("\n")
  
  
  i[["sv"]] = sv
  i[["sp"]] = sp
  
  # discretize 
  # n$d[[dp]] <- lapply(n$d, findPeaks())
  #  n$d[[dv]] <- lapply(n$d, findValleys())

  # Euler equations
  ep <-phi(sp, n)
  i[["ep"]]
  
  ev <-phi(sv, n)
  i[["ev"]] = ev
 

  return(i)
} # pivots




nuance <- function(symbol, data, nMutes, interval, radius) 
  {
  
  d <-
    floor(runif(1, 1, 5)) # max discretization is 1/10th of the total size of the aggregate data
  n <-
    floor(runif(1, 1, 7)) # max depth in hedging puts and calls
  
  r <- list()
  
  inds <- list()
 
  i <- list()
  
  i <- indicators( data )
  
  imax <- matrix()
  
  imax <- as.matrix(i$nd)
  
  cores <- cor(imax)
 
  txtStart(paste(symbol, "_cores", ".csv", sep=""), commands=T, results=T, append=T, cmdfile, visible.only=T)
  summary(cores)
  txtStop()
  
      cat("indicators")
      cat("\n")
  p <- list()
  
  p <- lapply( i$nd, pivots(), i = i$nd, n = 7 )
  cat("pivots")
  i[["pivots"]] = p
  
  cat("pivots")
  cat("\n")
  #when necessary, do discretizations so as to make
  
  #in.df = ldply(indicators, function(t) t$toDataFrame())
  #write.csv(in.df, file = paste(symbol, "ind.csv", sep=''))

    
    # n$i[dp][discSup] <- lapply(n$i[dp], phi(), n = depth)
    # n$i[dv][discRes] <- lapply(n$i[dv], phi(), n = depth)
  
  ptmIndicators <- proc.time()
  
  runTime = (ptmStart - ptmIndicators)
  
  
  txtStart(paste("log", symbol, "_data.csv"), commands=T, results=T, append=T, cmdfile,
           visible.only=T)
  
  lapply(i[["nd"]], summary())
  lapply(i[["pivots"]], summary())
  
  cat(paste(runTime, "runtime", "\n") )
  
  txtStop()
  
  
  return(i)
#in.df = ldply(indicators$ins$pv, function(t) t$toDataFrame())
#write.csv(in.df, file = paste(symbol, "pvs.csv", sep=''))

  
  # w = "weights"
  
  # TODO: get neural weights for indicators using deep learning
  
  #stock[[w]] <- getWeights(stock$n)
  
  # regressions nd other models of correlation for feature selection and rule based decisions
 

  
} # calc


# support and resistance


# return random mutation rate



# returns a quasi-random mutation given a maximum threshold
# mutations (positive or negative) are later summed with a fundamental value
# to produce periodicity for certain technical indicators



indicators <- function(e) {


  b <- as.matrix(e)

  i <- list()

  nm <- list()

  delta <- list()
  
  delta <- diff(x = b[, 12], lag = 1)
  
  i[["delta"]] <- delta
  nm <- "delta"
  
  cat("delta")

  cat("\n")
  
  # aroon oscillator
  
  
  a <- aroon(b[, c("Adj..High", "Adj..Low")], n = 20)
  
  o = length(a)
  i = o/3
  b = 2*o/3
  c = 1+i
  d = 1+b
  
   i[["aroonUp"]] <- list(sample(a, 1:i))
   i[["arroonDn"]] <- list(sample(a, c:b))
   i[["aroonOsc"]] <- list(sample(a, d:o))
  
  cat("aroonlength")

  cat("\n")
  
  # average true range

 atr <- ATR(b[, c("Adj..High", "Adj..Low", "Adj..Close")], n=14)
  
 cat("atrlength")

  o = length(atr)
 i = o/4
 b = i+1
 c = 3*o/4
 d = o/2
 e = c+1
 f = d+1
  
  
 i[["tr"]] <- list(sample(atr, 1:i))
 i[["atr"]] <- list(sample(atr, b:d))
 i[["atrTrueH"]] <- list(sample(atr, f:c))
 i[["atrTrueL"]] <- list(sample(atr, e:o))
  
 nm <- c(nm,  "tr", "atr", "atrTrueH", "atrTrueL")
  
  
  cat("\n")
  
  # bollinger bands
  
  bb.HLC <- BBands(b[, c("Adj..High", "Adj..Low", "Adj..Close")])
  bb <- BBands(b[, "Adj..Close"])
  
  o = length(bb)
  i = o/4
  b = i+1
  c = 3*o/4
  d = o/2
  e = c+1
  f = d+1
  cat("\n")
  
  
  i[["bb.dn"]] <- list(sample(bb, 1:i))
  i[["bb.mavg"]] <- list(sample(bb, b:d))
  i[["bb.up"]] <- list(sample(bb, f:c))
  i[["bb.pctB"]] <- list(sample(bb, e:o))
  
  nm <- c(nm,  "bb")
  

  #commodity channel index
  
  cci <- CCI(b[, c("Adj..High", "Adj..Low", "Adj..Close")])
  
  
  cat("ccilength")
  cat("\n")
  
  i[["cci"]] = cci
  nm <- c(nm,  "cci")

  
  #chaikin accumulation/distribution
  
  ad <-
    chaikinAD(b[, c("Adj..High", "Adj..Low", "Adj..Close")], b[, "Adj..Volume"])
  
  i[["ad"]] <- ad
  
  cat("adlength")
  cat("\n")
  
  # chaikin volatility
  
  volatility <- chaikinVolatility(b[, c("Adj..High", "Adj..Low")])
  
  i[["cVol"]] = volatility
  nm <- c(nm,  "cVol")
  
  cat("chaikin")

  cat("\n")
  
  # close location value
  
  clv <- CLV(b[, c("Adj..High", "Adj..Low", "Adj..Close")])
  
  i[["clv"]] = clv
  nm <- c(nm,  "clv")
  
  cat("clv")

  cat("\n")
  
  # chaikin money fAdj..Low
  
  cmf <-
    CMF(b[, c("Adj..High", "Adj..Low", "Adj..Close")], b[, "Adj..Volume"])
  
  i[["cmf"]] = cmf
  nm <- c(nm,  "cmf")
  
  cat("cmf")

  cat("\n")
  
  
  
  # chande momentum oscillator
  
  
  cmo <- CMO(b[, "Adj..Close"])
  
  i[["cmo"]] = cmo
  nm <- c(nm,  "cmo")
  
  cat("cmo")

  cat("\n")
  
  
  
  # DonchianChannel
  
  
  dc <- DonchianChannel(b[, c("Adj..High", "Adj..Low")])
  

  o = length(dc)
  i = o/3
  b = 2*o/3
  c = 1+i
  d = 1+b
  
  i[["donchianHi"]] <- list(sample(dc, 1:i))
  i[["donchianLo"]] <- list(sample(dc, c:b))
  i[["donchianMd"]] <- list(sample(dc, d:o))
  
  nm <- c(nm,  "donchian")
  
  cat("dc")

  cat("\n")
  
  
  
  # De-Trended Price Oscillator
  
  
  priceDPO <- DPO(b[, "Adj..Close"])
  
  i[["priceDPO"]] = priceDPO
  nm <- c(nm,  "priceDPO")
  
  volumeDPO <- DPO(b[, "Adj..Volume"])
  
  i[["volumeDPO"]] = volumeDPO
  nm <- c(nm,  "volumeDPO")
  
  
  
  cat("priceDPO")
 
  cat("\n")
  cat("volumeDPO")

  cat("\n")
  
  
  
  # directional volume index
  
  
  dvi <- DVI(b[, "Adj..Close"])
  
  i[["dvi"]] = dvi
  nm <- c(nm,  "dvi")
  
  cat("dvi")

  cat("\n")
  
  
  
  # guppy
  
  
  gmma <- GMMA(b[, "Adj..Close"], 
               short = c(3, 5, 8, 10, 12, 15), 
               long = c(30, 35, 40, 45, 50, 60) )
  
  a = 4174
  b = a*2
  c = a*3
  d = a*4
  e = a*5
  f = a*6
  g = a*7
  h = a*8
  i = a*9
  j = a*10
  k = a*11
  l = a*12

  o = b+1
  p = c+1
  q = d+1
  r = e+1
  s = f+1
  t = g+1
  u = h+1
  v = i+1
  w = j+1
  x = k+1
  y = l+1
 
  
  
  
  g3 <- list(sample(gmma, 1:a))
  g5 <- list(sample(gmma, a+1:b))
  g8 <- list(sample(gmma, o:c))
  g10 <- list(sample(gmma, p:d))
  g12 <- list(sample(gmma, q:e))
  g15 <- list(sample(gmma, r:f))
  g30 <- list(sample(gmma, s:g))
  g35 <- list(sample(gmma, t:h))
  g40 <- list(sample(gmma, u:i))
  g45 <- list(sample(gmma, v:j))
  g50 <- list(sample(gmma, w:k))
  g60 <- list(sample(gmma, x:l))
  
  
  i[["g5"]] = g5
  i[["g8"]] = g8
  i[["g10"]] = g10
  i[["g12"]] = g12
  i[["g15"]] = g15
  i[["g30"]] = g30
  i[["g35"]] = g35
  i[["g40"]] = g40
  i[["g45"]] = g45
  i[["g50"]] = g50
  i[["g60"]] = g60
  
  
  nm <- c(nm,  "g5", "g8", "g10", "g12", "g15", "g30", 
          "g35", "g40", "g45", "g50", "g60")
  
  cat("guppy")
  cat("\n")
  
  
  cat("kst")
  cat("\n")
  
  
  
  # know sure thing
  
  
  kst <- KST(b[, "Adj..Close"])
  
  i[["kst"]] = kst
  nm <- c(nm,  "kst")
  

  
  cat("kst.4ma")
  cat("\n")
  
  # know sure thing 4 moving average
  
  kst4MA <-
    KST(b[, "Adj..Close"], maType = list(list(SMA), list(EMA), list(DEMA), list(WMA)))
  
  i[["kst.4MA"]] = kst4MA
  nm <- c(nm,  "kst.4MA")
  
  
  m <-
    MACD(b[, "Adj..Close"], 9, 12, 26, maType = list(list(SMA), list(EMA, wilder = TRUE), list(SMA)))
  
  o = length(m)

  b = o/2
  i = b+1
  
  i[["macd"]] <-list(sample(m, 1:b))
  i[["macdsignal"]] <-list(sample(m, i:o))
  nm <- c(nm,  "macd", "macdsignal")
  
  cat("macd")

  cat("\n")
  
  
  
  # money flow indices
  
  mfi <-
    MFI(b[, c("Adj..High", "Adj..Low", "Adj..Close")], b[, "Adj..Volume"])
  
  i[["mfi"]] = mfi
  nm <- c(nm,  "mfi")
    n 
  cat("mfi")

  cat("\n")
  
  # on balance volume
  
  obv <- OBV(e[, "Adj..Close"], e[, "Adj..Volume"])
  
  i[["obv"]] = obv
  nm <- c(nm,  "obv")
  

  cat("\n")
  cat("obv")

  cat("\n")
  
  # PBands

  pbands <- PBands(b[, "Adj..Close"])

  
  o = length(pbands)
  i = o/3
  b = 2*o/3
  c = 1+i
  d = 1+b
  
  i[["pbDn"]] <- list(sample(pbands, 1:i))
  i[["pbCenter"]] <- list(sample(pbands, c:b))
  i[["pbUp"]] <- list(sample(pbands, d:o))
  
  nm <- c(nm,  "pbDn", "pbCenter", "pbUp")
  

  cat("\n")
  cat("pbands")

  cat("\n")  
  
  # rate of change
  
  roc <- ROC(b[, "Adj..Close"])
  
  i[["roc"]] = roc
  nm <- c(nm,  "roc")
  
  cat("roc")

  cat("\n")
  
  # momentum
  
  mom <- momentum(b[, "Adj..Close"])
  
  i[["mom"]] = mom
  nm <- c(nm,  "mom")

  cat("mom")

  cat("\n")
  
  # relative strength index
  
  price <- b[, "Adj..Close"]
  

  cat("\n")
  
  
  rsi <- RSI(price)
  
  
  i[["rsi"]] = rsi
  nm <- c(nm,  "rsi")
  
  
  cat("rsi")

  cat("\n")
  
  
  rsiMA1 <-
    RSI(price,
        n = 14,
        maType = "WMA",
        wts = b[, "Adj..Volume"])
  
  i[["rsiMA1"]] = rsiMA1
  nm <- c(nm,  "rsiMA1")
  
  
  cat("rsiMA1")

  cat("\n")

  
  rsiMA2 <-
    RSI(price,
        n = 14,
        maType = list(
          maUp = list(EMA, ratio = 1 / 5),
          maDown = list(WMA, wts = 1:10)
        ))
  
  
  i[["rsiMA2"]] = rsiMA2
  nm <- c(nm,  "rsiMA2")

  cat("rsiMA2")

  cat("\n")
  
  
  # Parabolic Stop-and-Reverse
  
  sar <- SAR(b[, c("Adj..High", "Adj..Low")])
  
  i[["sar"]] = sar
  nm <- c(nm,  "sar")
  
  
  cat("sar")

  cat("\n")
  
  
  # Moving Averages
  
  ema.20 <- EMA(b[, "Adj..Close"], 14)
  
  i[["ema.20"]] = ema.20
  nm <- c(nm,  "ema.20")
  
  cat("ema.20")

  cat("\n")

  
  sma.20 <- SMA(b[, "Adj..Close"], 20)
  
  i[["sma.20"]] = sma.20
  nm <- c(nm,  "sma.20")
  
  cat("sma.20")

  cat("\n")
  
  
  dema.20 <- DEMA(b[, "Adj..Close"], 20)
  
  i[["dema.20"]] = dema.20
  nm <- c(nm,  "dema.20")

  cat("dema.20")

  cat("\n")

  
  evwma.20 <- EVWMA(b[, "Adj..Close"], b[, "Adj..Volume"], 20)
  
  i[["evwma.20"]] = evwma.20
  nm <- c(nm,  "evwma.20")

  cat("evwma.20")

  cat("\n")
 
  
  zlema.20 <- ZLEMA(b[, "Adj..Close"], 20)
  
  i[["zlema.20 "]] <- zlema.20
  nm <- c(nm,  "zlema.20 ")

  
  cat("zlema.20")

  cat("\n")
  
  hma = HMA(b[, "Adj..Close"])
  i[["hma"]] = hma
  nm <- c(nm,  "hma")
  
  
  cat("hma")

  cat("\n")
  
  
  
  ## Example of Tim Tillson's T3 indicator
  
  T3 <-
    function (x, n = 10, v = 1)
      DEMA(DEMA(DEMA(x, n, v), n, v), n, v)
  
  t3 <- T3(b[, "Adj..Close"])
  
  
  i[["t3"]] = t3
  nm <- c(nm,  "t3")
  
  
  cat("t3")

  cat("\n")
  
  # stochastic oscillators
  
  sto <- stoch(b[, c("Adj..High", "Adj..Low", "Adj..Close")])
  
  o = length(sto)
  i = o/5
  b = i+1
  c = 2*o/5
  d = 3*o/5
  e = 4*o/5
  r = c+1
  s = d+1
  t = e+1
  
  cat("\n")
  
  i[["stochFK"]] <- list(sample(sto, 1:i))
  i[["stochFD"]] <- list(sample(sto, b:c))
  i[["stochSD"]] <- list(sample(sto, r:d))
  i[["stochSMI"]] <- list(sample(sto, s:e))
  i[["stochS"]] <- list(sample(sto, t:o))
  
  
  nm <- c(nm, "stochFK", "stochFD","stochSD","stochSMI","stochS")
  
  
  cat("stochOSC")

  cat("\n")
  
  
  # williams percent r
  
  stochWPR <- WPR(b[, c("Adj..High", "Adj..Low", "Adj..Close")])
  
  
  i[["stochWPR"]] = stochWPR
  nm <- c(nm,  "stochWPR")
  
  cat("stochWPR")

  cat("\n")
  
  stoch2MA <-
    stoch(b[, c("Adj..High", "Adj..Low", "Adj..Close")], maType = list(list(SMA), list(EMA, wilder = TRUE), list(SMA)))
  
  
  i[["stoch2MA"]] = stoch2MA
  nm <- c(nm,  "stoch2MA")
  
  
  cat("stoch2MA")

  cat("\n")
  
  SMI3MA <-
    SMI(b[, c("Adj..High", "Adj..Low", "Adj..Close")], maType = list(list(SMA), list(EMA, wilder = TRUE), list(SMA)))
  
  
  i[["SMI3MA"]] = SMI3MA
  nm <- c(nm,  "SMI3MA")
  
  
  cat("SMI3MA")

  cat("\n")
  
  stochRSI <- stoch(RSI(b[, "Adj..Close"]))
  
  
  i[["stochRSI"]] = stochRSI
  nm <- c(nm,  "stochRSI")
  
  cat("stochRSI")

  cat("\n")
  
  # trend detection index
  
  tdi <- TDI(b[, "Adj..Close"], n = 14)
  
  t = length(tdi)
  r = t/2
  u = r+1
  
  i[["tdi"]] <- list(sample(tdi, 1:r))
  i[["di"]] <- list(sample(tdi, u:t))
  
  nm <- c(nm,  "tdi", "di")
  
  
  cat("tdi")

  cat("\n")
  
  trix <- TRIX(b[, "Adj..Close"])
  
  i[["trix"]] = trix
  nm <- c(nm,  "trix")
  
  
  cat("trix")
  cat("\n")
  
  trix4 <-
    TRIX(b[, "Adj..Close"], maType = list(list(SMA), list(EMA, wilder = TRUE), list(SMA), list(DEMA)))
  
  i[["trix4"]] = trix4
  nm <- c(nm,  "trix4")
  
  
  cat("trix4")
  cat("\n")
  
  # ultimate oscillator
  
  ultOSC <-
    ultimateOscillator(b[, c("Adj..High", "Adj..Low", "Adj..Close")])
  
  i[["ultOSC"]] = ultOSC
  nm <- c(nm,  "ultOSC")
  
  cat("ultOSC")

  cat("\n")
  
  # vertical horizon filter
  
  vhf.close <- VHF(b[, "Adj..Close"])
  
  i[["vhf.close"]] = vhf.close
  nm <- c(nm,  "vhf.close")
  
  # custom
  
  cat("vhf.close")

  cat("\n")
  
  vhf.hilow <-
    VHF(b[, c("Adj..High", "Adj..Low", "Adj..Close")])
  
  
  i[["vhf.hilow"]] = vhf.hilow
  nm <- c(nm,  "vhf.hilow")
  
  # custom
  

  cat("vhf.hilow")

  
  # Volatility
  
  ohlc <- b[, c("Adj..Open", "Adj..High", "Adj..Low", "Adj..Close")]
  
  i[["ohlc"]] = ohlc
  nm <- c(nm,  "ohlc")

  cat("\n")
  cat("\n")
  
  vClose <- volatility(ohlc, calc = "close")
  
  
  i[["vC"]] = vClose
  nm <- c(nm,  "vC")
  
  vClose <- volatility(ohlc, calc = "rogers.satchell")
  cat("\n")
  cat("satchell")

  cat("\n")
  
  i[["vS"]] = vClose
  nm <- c(nm,  "vS")
  
  vClose <- volatility(ohlc, calc = "garman.klass")
  
  cat("\n")
  cat("garman")

  cat("\n")
  
  i[["vG"]] = vClose
  nm <- c(nm,  "vG")
  
  vClose <- volatility(ohlc, calc = "yang.zhang")
  
  cat("\n")
  cat("yang")

  cat("\n")
  
  i[["vY"]] = vClose
  nm <- c(nm,  "vY")
  
  # custom
  
  vGK <- volatility(ohlc, calc = "gk.yz")
  
  
  i[["vGK"]] = vGK
  nm <- c(nm,  "vGK")
  
  cat("\n")
  cat("vGK")

  cat("\n")
  
  # custom garman vol

  vP <- volatility(ohlc, calc = "parkinson")
  
  
  i[["vP"]] = vP
  nm <- c(nm,  "vP")
  
  # custom parkinson vol
 
  
  cat("\n")
  cat("vP")

  cat("\n")
 
  # williams a/d
  
  ad <- williamsAD(b[, c("Adj..High", "Adj..Low", "Adj..Close")])
  
  
  i[["ad"]] = ad
  nm <- c(nm,  "ad")
  
  
  cat("\n")
  cat("ad")

  cat("\n")
  
  
  # williams percent R
  
  
  stochWPR <- WPR(b[, c("Adj..High", "Adj..Low", "Adj..Close")])
  
  i[["stochWPR"]] = stochWPR
  nm <- c(nm, "stochWPR")
  

  cat("\n")
  cat("stochWPR")
  cat("\n")
  
  
  # zig zag
  
  zz <- ZigZag(b[, c("Adj..High", "Adj..Low")], change = 14)
  
  nm <- c(nm, "zz")
  
  i[["zz"]] = zz

  nd <- list()
  
  explanatory <- as.vector(nm)
  
  x <- as.data.frame(as.matrix(i))
  
  cat("\n")
  cat("data")
  cat("\n")
 # write.csv(x = data, paste(symbol, "_indicators.csv", sep=""), append=T, sep=",", na="NA")
  
  cat("write")
  cat("\n")
  
  training = "delta"

  nd[["data"]] <- list(nd = i, class = explanatory, x = table)
 
  cat("list")
  cat("\n")
  
  return(nd)
  
} # inds


# toDate <- function(x) as.Date(x, origin = "1-12-30")









#######################   BEGIN CALCULATIONS   ###########################


# note: dataframes are passed by reference, to pass other R variables by reference
# an environment needs to be declared first

#start <- proc.time()
#prefix = "C:/Users/JC/Documents/R/optimystic/files/"
#suffix = ".csv"



#paths <- list()
#data <- list()
#book <- list()
#balance <- list()

#for (f in large) {
  
#  cat(f)
#  cat("\n")
  
#  path <- paste(prefix, f, suffix, sep = '')
#  data <- read(path)
#  know <- learn(f, data)
  
#  midway <- proc.time()
#  elaps = start - midway
  
#  cat(elaps)
#  cat("el")
#  cat("\n")
  
  # balance <- checkBalance(know)
  
  # order <- trade(balance)
  
  # if (isTrue(order$trade))
  #  write(order$trades)
  
#} # for f in large

# for(random in random_A_symbols) {

#    random[[index]] <- paste(prefix, random, suffix)
#    index = index+1

#}


# do data runs 10 times to help sort stochastic anomales


#  for(mod in book$mod) {

#    results<-tradeModel(mod,
# signal.threshold = mod$t,
#             leverage = 1,
#             return.model = TRUE,
#             plot.model = TRUE,
#             trade.dates = NULL,
#             exclude.training = FALSE,
#             ret.type = "quarters")

#    end <- proc.time()
#    elapsed = start - end

#  txtStart(c(i, mod, "formulae.txt", sep = "_"))

#  summary(results)
#  paste("runtime: ", elapsed)

#  txtStop()

#                       } # for all models in file$mod
#      } # for all i

#  finish <- proc.time()
#  elapsed = start - finish

#  txtStart(c(i, mod, "formulae.txt", sep = "_"))

#  paste(elapsed, ": total runtime")

#  txtStop()
