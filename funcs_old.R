
phi <- function(v, n){
  
  phi = (v * (n + 1)) / (v * n)
  
  return(phi)
  
}


models <- function() {
  models <-
    list(
      "lm",
      "glm",
      "loess",
      "step",
      "ppr",
      "rpart[rpart]",
      "tree[tree]",
      "randomForest[randomForest]",
      "mars[mda]",
      "poly-mars[polspline]",
      "lars[lars]",
      "rq[quantreg]",
      "lqs[MASS]",
      "rlm[MASS]",
      "svm[e1071]",
      "nnet[nnet]"
    )
  
  return(models)
  
}

compareModels <- function(stock, n) {
  
  modes <- models()
  indicators <- indicators()
  operators <- operators()
  cat("modes, inds, operators")
  cat("\n")
  
  o <- length(operators)
  i = (o + 1)
  
  #set formula to compare every dimension
  form1 <- c("Y ~ ., data = stock[[\"indicatorData\"]$I")
  form2 <- c("Y ~ ., data = stock[[\"indicatorData\"]$D")
  cat(form1)
  cat("\n")
  r = 0
  
  while (r != n) {
    
    
    # get random samples from both the operator list and the indicator list
    oSample <- sample(operators, o, replace = T)
    iSample <- sample(indicators, i, replace = T)
    
    # interleave the operators and indicators to make a formula
    form2 <-
      paste(order (c (
        seq_along(iSample), seq_along(oSample)
      )), collapse = '')
    form3 <- c("Y ~ ", form2, "data = stock[[\"indicatorData\"]$I")
    form4 <- c("Y ~ ", form2, "data = stock[[\"indicatorData\"]$D")
    cat("\n")
    
    for (mo in modes) {
      
      # convert to someething the program can understand
      
      f1 <- as.formula(form1)
      
      f2 <- as.formula(form2)
      
      f3 <- as.formula(form3)
      
      f4 <- as.formula(form4)
      
      # run each formula through the models for feature selection and correlation
      m1 <-
        specifyModel(f1, na.rm = T) #  <- don't include any incomplete rows
      m2 <- specifyModel(f2, na.rm = T)
      
      m3 <-
        specifyModel(f3, na.rm = T) #  <- don't include any incomplete rows
      m4 <- specifyModel(f4, na.rm = T)
      
      # character vector representing dates in ISO 8601 format “CCYY-MM-DD” or
      # “CCYY-MM-DD HH:MM:SS” of length 2
      
      # all indicators
      
      m1.b <- buildModel(m1, mo, training.per = c(stock$head, stock$tail))
      m2.b <- buildModel(m2, mo, training.per = c(stock$head, stock$tail))
      
      # stochastic formula
      
      m3.b <- buildModel(m3, mo, training.per = c(stock$head, stock$tail))
      m4.b <- buildModel(m4, mo, training.per = c(stock$head, stock$tail))
      
      #add models to stock data
      stock$m[r] <-as.list(c(mode1 = m1.b, mode2 = m2.b, mode3 = m3.b, mode4 = m4.b ))
      
      #write model summary data to file
      txtStart(c(stock$symbol, txtStop(), "formulae.txt", sep = "_"))
      
      lapply(stock$mods, summary() )
      
      txtStop()
      
      r = r + 1
    } # mo in modes
    
    
  } # while
  
  return(stock)
  
} # compare




operators <- function() {
  #  X + Z + W + X:Z + X:W + Z:W + X:Z:W
  operators <- c('*', '.', '|', '+', '-', ':', '^', 'I', '1')
  
  return(operators)
  
}

one <- function(list) {
  one <- sample(list, 1)
  
  return(one)
  
}

calc <- function(stocks, n) {
  
  while(n > 0){
  
    discs <- runif(1, 10) # max discretization is 1/10th of the total size of the aggregate data
    depth <- runif(1, 10) # max depth in hedging puts and calls
    
    
  for (stock in stocks) {
    
    n = (n - 1)
    
    ptmStart <- proc.time()
    
    # randomized set of mutations
    stock$r20 <- getRadiance(20, 3, 6)
    stock$r10 <- getRadiance(10, 2, 4)
    stock$r5 <- getRadiance(5, 1, 2)
    
    # technical indicators and a bunch of other useful data
    ins(stock, stock$r20, stock$r10, stock$r5, discs)
  
    cat("indicators")
    cat("\n")
    
    # where first derivative is 0, (or: inflection points of second derivative) 
    
    ip <- lapply( stock$i, findPeaks(stock$i) )
    iv <- lapply( stock$i, findValleys(stock$i) )
      
    dp <- lapply( stock$d, findPeaks(stock$d) )
    dv <- lapply( stock$d, findValleys(stock$d) )
     
    
    # support and resistance calculations for hedging puts and calls
    
    cat("peaks and valleys")
    cat("\n")
    
    sumRes <- lapply(ip, phi(ip, depth) )
    sumSup <- lapply(iv, phi(iv, depth) )
    someRes <- lapply(dp, phi(dp, depth) )
    someSup <- lapply(dv, phi(dv, depth) )
    
    pivots <- as.list(c(groupRes = sumRes, groupSups = sumSup, discRes = someRes, discSups = someSup) ) 
 
    stock$pv = pivots
     
    cat("pivots")
    cat("\n")
    

    
    # regressiona nd other models of correlation for feature selection and rule based decisions
    
    stock$mod <- compareModels(stock, n)
    
    cat("compare models")
    cat("\n")
    
    ptmIndicators <- proc.time()
    
    runTime = (ptmStart - ptmIndicators)
    
    stock$runtime[n]  = runTime
    #model data
 
    
  } # for each file
  
    
  } # for each run
  
  
}




# support and resistance

pivots <- function(n, v) {
  
  set <- c(1:n, 1)
  
  for (p in v) {
    
    for (value in set) {
      
      pivots <- phi(value, p)
    } # for
    
  } # for
  
  return(pivots)
  
} # pivots



#discretizing function

disc <- function(indicator, n, discs) {
  
  data.frame(min = sapply(indicator, iMin),
             max = sapply(indicator, iMax))
  
  range <- (max - min)
  
  levels <- floor(n * discs)
  
  # discs is a percentage of total length
  
  indicator(gaussian.test) 
  
  # TODO: other forms of discretization are possible. 
  # Identify the most efficient and adapt the model building accordingly
  d <-
    discretize(gaussian.test, method = 'hartemink', breaks = levels)
  
  return(d)
  
} # disc

# return random mutation rate

getRadiance <- function (period, minRadiance, maxRadiance) {
    radiance <-
      runif(
        10,
        min = (period - maxRadiance),
        max = (period - minRadiance)
      )
    
    return(radiance)
    
  } # getRadiance

# returns a quasi-random mutation given a maximum threshold
# mutations (positive or negative) are later summed with a fundamental value
# to produce periodicity for certain technical indicators

mute <- function (v) {
  
  mutation <- sample(v, 1)
  
  return(mutation)
  
}

ins <- function(book, p1, p2, p3, divisor) {
    
    symbol <- book$data
      
    discs <- floor(divisor / 100)
    
    book$i["deltaPrice"] <- diff(symbol["Adj. Close"], lag = 1)


    book$d["deltaPrice"] <- disc(book$i["deltaPrice"] , length(book$i["deltaPrice"] ), discs)
    book$n <- "deltaPrice"
    
    # aroon oscillator
    
     book$m["aroon"] <- mute(p2)
    
    aroon <-
      aroon(symbol[, c("High", "Low")], n = 14 + book$m["aroon"])
    
    book$i["aroon"] <- aroon
    book$d["aroon"] <- disc(aroon, nrow(aroon), discs)
    
    book$n <- c(book$n,  "aroon")
    
    cat("here2")
    cat("\n")
    
    # average true range
    
    book$m["atrMu"] <- mute(p2)
    
    atr <-
      ATR(symbol[, c("High", "Low", "Close")], n = 14 + book$m["atrMu"])
    
    book$i["atr"] <- atr
    book$d["atr"] <- disc(atr, nrow(atr), discs)
    book$n <- c(book$n,  "atr")
    
    cat("here3")
    cat("\n")
    
    # bollinger bands
    
    bbands.HLC <- BBands(symbol[, c("High", "Low", "Close")])
    bbands.close <- BBands(symbol[, "Close"])
    
    book$i["bb"] <- bbands.close
    book$d["bb"] <- disc(bbands.close, nrow(bbands.close), discs)
    book$n <- c(book$n,  "bb")

    
    cat("here4")
    cat("\n")
    
    #commodity channel index
    
    cci <- CCI(symbol[, c("High", "Low", "Close")])
    
    book$i["cci"] <- cci
    book$d["cci"] <- disc(cci, nrow(cci), discs)
    book$n <- c(book$n,  "cci")
  
    
    
    cat("here5")
    cat("\n")
    
    #chaikin accumulation/distribution
    
    ad <-
      chaikinAD(symbol[, c("High", "Low", "Close")], symbol[, "Volume"])
    
    book$i["ad"] <- ad
    book$d["ad"] <- disc(ad, nrow(ad), discs)

    
    
    cat("here6")
    cat("\n")
    
    # chaikin volatility
    
    volatility <- chaikinVolatility(symbol[, c("High", "Low")])
    
    book$i["cVol"] <- volatility
    book$d["cVol"] <- disc(volatility, nrow(volatility), discs)
    book$n <- c(book$n,  "cVol")
 
    
    
    cat("here7")
    cat("\n")
    
    # close location value
    
    clv <- CLV(symbol[, c("High", "Low", "Close")])
    
    book$i["clv"] <- clv
    book$d["clv"] <- disc(clv, nrow(clv), discs)
    book$n <- c(book$n,  "clv")
 
    
    
    cat("here8")
    cat("\n")
    
    # chaikin money flow
    
    cmf <-
      CMF(symbol[, c("High", "Low", "Close")], symbol[, "Volume"])
    
    book$i["cmf"] <- cmf
    book$d["cmf"] <- disc(cmf, nrow(cmf), discs)
    book$n <- c(book$n,  "cmf")
   
    
    
    cat("here9")
    cat("\n")
    
    
    
    # chande momentum oscillator
    
    
    cmo <- CMO(symbol[, "Close"])
    
    book$i["cmo"] <- cmo
    book$d["cmo"] <- disc(cmo, nrow(cmo), discs)
    book$n <- c(book$n,  "cmo")
  
    
    
    cat("here10")
    cat("\n")
    
    
    
    # DonchianChannel
    
    
    dc <- DonchianChannel(symbol[, c("High", "Low")])
    
    book$i["donchian"] <- dc
    book$d["donchian"] <- disc(dc, nrow(dc), discs)
    book$n <- c(book$n,  "donchian")
    
    
    cat("11")
    cat("\n")
    
    
    
    # De-Trended Price Oscillator
    
    
    priceDPO <- DPO(symbol[, "Close"])
    
    book$i["priceDPO"] <- priceDPO
    book$d["priceDPO"] <- disc(priceDPO, nrow(priceDPO), discs)
    book$n <- c(book$n,  "priceDPO")
    
    volumeDPO <- DPO(symbol[, "Volume"])
    
    book$i["volumeDPO"] <- volumeDPO
    book$d["volumeDPO"] <- disc(volumeDPO, nrow(volumeDPO), discs)
    book$n <- c(book$n,  "volumeDPO")
    
    
    cat("dvi")
    cat("\n")
    
    
    
    # directional volume index
    
    
    dvi <- DVI(symbol[, "Close"])
    
    book$i["dvi"] <- dvi
    book$d["dvi"] <- disc(dvi, nrow(dvi), discs)
    book$n <- c(book$n,  "dvi")
    
    
    
    cat("gmma")
    cat("\n")
    
    
    
    # guppy
    
    
    gmma <- GMMA(symbol[, "Close"])
    
    book$i["guppy"] <- gmma
    book$d["guppy"] <- disc(gmma, nrow(gmma), discs)
    book$n <- c(book$n,  "guppy")
    
    
    cat("kst")
    cat("\n")
    
    
    
    # know sure thing
    
    
    kst <- KST(symbol[, "Close"])
    
    book$i["kst"] <- kst
    book$d["kst"] <- disc(kst, nrow(kst), discs)
    book$n <- c(book$n,  "kst")
    
    
    cat("kst.4ma")
    cat("\n")
    
    # know sure thing 4 moving average
    
    kst4MA <-
      KST(symbol[, "Close"], maType = list(list(SMA), list(EMA), list(DEMA), list(WMA)))
    
    book$i["kst.4MA"] <- kst4MA
    book$d["kst.4MA"] <- disc(kst4MA, nrow(kst4MA), discs)
    book$n <- c(book$n,  "kst.4MA")
    
    
    
    cat("macd")
    cat("\n")
    
    
    m1 <- mute(p1)
    book$m["macd1"] <- m1
    m2 <- mute(p2)
    book$m["macd2"] <- m2
    m3 <- mute(p3)
    book$m["macd3"] <- m3
    
    
    
    # moving average convergence/divergence
    
    macd <-
      MACD(symbol[, "Close"], 12 + m2, 26 + m1, 9 + m3, maType = list(list(SMA), list(EMA, wilder = TRUE), list(SMA)))
    
    book$i["macd"] <- macd
    book$d["macd"] <- disc(dvi, nrow(dvi), discs)
    book$n <- c(book$n,  "macd")
    
    
    cat("here18")
    cat("\n")
    
    
    # money flow index
    
    mfi <-
      MFI(symbol[, c("High", "Low", "Close")], symbol[, "Volume"])
    
    book$i["mfi"] <- mfi
    book$d["mfi"] <- disc(mfi, nrow(mfi), discs)
    book$n <- c(book$n,  "mfi")
    
    
    cat("here19")
    cat("\n")
    
    # on balance volume
    
    obv <- OBV(symbol[, "Close"], symbol[, "Volume"])
    
    book$i["obv"] <- obv
    book$d["obv"] <- disc(obv, nrow(obv), discs)
    book$n <- c(book$n,  "obv")
    
    cat("here20")
    cat("\n")
    
    
    # PBands
    # PBands(prices, n = 20, maType = "SMA", sd = 2, ..., fastn = 2, centered = FALSE, lavg = FALSE)
    
    pbands.close <- PBands(symbol[, "Close"])
    book$i["pb"] <- pbands.close
    book$d["pb"] <- disc(pbands.close, nrow(pbands.close), discs)
    book$n <- c(book$n,  "pb")
    
    
    cat("here21")
    cat("\n")
    
    
    # rate of change
    
    roc <- ROC(symbol[, "Close"])
    
    book$i["roc"] <- roc
    book$d["roc"] <- disc(roc, nrow(roc), discs)
    book$n <- c(book$n,  "roc")
    
    cat("here22")
    cat("\n")
    
    
    # momentum
    
    mom <- momentum(symbol[, "Close"])
    
    book$i["mom"] <- mom
    book$d["mom"] <- disc(mom, nrow(mom), discs)
    book$n <- c(book$n,  "mom")
    
    
    cat("mom")
    cat("\n")
    
    
    # relative strength index
    price <- symbol[, "Close"]
    
    cat("24")
    cat("\n")
    
    
    rsi <- RSI(price)
    
    
    book$i["rsi"] <- rsi
    book$d["rsi"] <- disc(rsi, nrow(rsi), discs)
    book$n <- c(book$n,  "rsi")
    
    
    cat("25")
    cat("\n")
    
    
    mutation <- mute(p2)
    book$m["rsiMA"] <- mutation
    
    rsiMA1 <-
      RSI(price,
          n = 14 + mutation,
          maType = "WMA",
          wts = symbol[, "Volume"])
    
    book$i["rsiMA1"] <- rsiMA1
    book$d["rsiMA1"] <- disc(rsiMA1, nrow(rsiMA1), discs)
    book$n <- c(book$n,  "rsiMA1")
    
    
    cat("26")
    cat("\n")
    
    
    rsiMA2 <-
      RSI(price,
          n = 14 + mutation,
          maType = list(
            maUp = list(EMA, ratio = 1 / 5),
            maDown = list(WMA, wts = 1:10)
          ))
    
    book$i["rsiMA2"] <- rsiMA2
    book$d["rsiMA2"] <- disc(rsiMA2, nrow(rsiMA2), discs)
    book$n <- c(book$n,  "rsiMA2")
    
    
    cat("27")
    cat("\n")
    
    
    # Parabolic Stop-and-Reverse
    
    sar <- SAR(symbol[, c("High", "Low")])
    
    book$i["sar"] <- sar
    book$d["sar"] <- disc(sar, nrow(sar), discs)
    book$n <- c(book$n,  "sar")
    
    
    cat("28")
    cat("\n")
    
    
    # Moving Averages
    
    mutation <- mute(p1)
    book$m["ema.20"] <- mutation
    
    ema.20 <- EMA(symbol[, "Close"], 20 + mutation)
    
    book$i["ema.20"] <-  ema.20
    book$d["ema.20"] <- disc(ema.20, nrow(ema.20), discs)
    book$n <- c(book$n,  "ema.20")
    
    
    # average directional index for ema20
    
    adx.ema <-
      ADX(c("High", "Low", "Close"), 20 + mutation, MATYPE = "EMA")
    
    book$i["adx.ema20"] <- adx.ema
    book$d["adx.ema20"] <- disc(adx.ema, nrow(adx.ema), discs)
    book$n <- c(book$n,  "adx.ema20")
    
    
    cat("29")
    cat("\n")
    
    
    mutation <- mute(p1)
    book$m["sma.20"] <- mutation
    
    sma.20 <- SMA(symbol[, "Close"], 20 + mutation)
    
    book$i["sma.20"] <- sma.20
    book$d["sma.20"] <- disc(sma.20, nrow(sma.20), discs)
    book$n <- c(book$n,  "sma.20")
    
    
    # average directional index for ema20
    
    adx.sma.20 <-
      ADX(c("High", "Low", "Close"), 20 + mutation, MATYPE = "SMA")
    
    book$i["adx.sma20"] <- adx.sma.20
    book$d["adx.sma20"] <- disc(adx.sma.20, nrow(adx.sma.20), discs)
    book$n <- c(book$n,  "adx.sma20")
    
    
    cat("30")
    cat("\n")
    
    
    mutation <- mute(p1)
    book$m["dema.20"] <- mutation
    
    dema.20 <- DEMA(symbol[, "Close"], 20 + mutation)
    
    book$i["dema.20"] <- dema.20
    book$d["dema.20"] <- disc(dema.20, nrow(dema.20), discs)
    book$n <- c(book$n,  "dema.20")
    
    adx.dema.20 <-
      ADX(c("High", "Low", "Close"), 20 + mutation, MATYPE = "DEMA")
    
    
    book$i["adx.dema.20"] <- adx.dema.20
    book$d["adx.dema.20"] <-
      disc(adx.dema.20, nrow(adx.dema.20), discs)
    book$n <- c(book$n,  "adx.dema.20")
    
    cat("31")
    cat("\n")
    
    
    
    mutation <- mute(p1)
    book$m["evwma.20"] <- mutation
    
    evwma.20 <-
      EVWMA(symbol[, "Close"], symbol[, "Volume"], 20 + mutation)
    
    
    book$i["evwma.20"] <- evwma.20
    book$d["evwma.20"] <- disc(evwma.20, nrow(evwma.20), discs)
    book$n <- c(book$n,  "evwma.20")
    
    
    adx.evwma.20 <-
      ADX(c("High", "Low", "Close"), 20 + mutation, MATYPE = "EVWMA")
    
    book$i["adx.evwma.20"] <- adx.evwma.20
    book$d["adx.evwma.20"] <-
      disc(adx.evwma.20, nrow(adx.evwma.20), discs)
    book$n <- c(book$n,  "adx.evwma.20")
    
    
    cat("32")
    cat("\n")
    
    mutation <- mute(p1)
    book$m["zlema.20"] <- mutation
    
    zlema.20 <- ZLEMA(symbol[, "Close"], 20 + mutation)
    
    
    book$i["zlema.20 "] <- zlema.20
    book$d["zlema.20 "] <- disc(zlema.20 , nrow(zlema.20), discs)
    book$n <- c(book$n,  "zlema.20 ")
    
    
    adx.zlema.20 <-
      ADX(c("High", "Low", "Close"), 20 + mutation, MATYPE = "ZLEMA")
    
    book$i["adx.zlema.20"] <- adx.zlema.20
    book$d["adx.zlema.20"] <-
      disc(adx.zlema.20, nrow(adx.zlema.20), discs)
    book$n <- c(book$n,  "adx.zlema.20")
    
    
    
    
    cat("33")
    cat("\n")
    
    hma <- HMA(symbol[, "Close"])
    
    
    book$i["hma"] <- hma
    book$d["hma"] <- disc(hma, nrow(hma), discs)
    book$n <- c(book$n,  "hma")
    
    
    adx.hma <- ADX(c("High", "Low", "Close"), MATYPE = "HMA")
    
    book$i["adx.hma"] <- adx.hma
    book$d["adx.hma"] <- disc(adx.hma, nrow(adx.hma), discs)
    book$n <- c(book$n,  "adx.hma.20")
    
    
    cat("34")
    cat("\n")
    
    
    
    ## Example of Tim Tillson's T3 indicator
    
    T3 <-
      function (x, n = 10, v = 1)
        DEMA(DEMA(DEMA(x, n, v), n, v), n, v)
    
    t3 <- T3(symbol[, "Close"])
    
    
    book$i["t3"] <- t3
    book$d["t3"] <- disc(t3, nrow(t3), discs)
    book$n <- c(book$n,  "t3")
    
    
    cat("35")
    cat("\n")
    
    
    # stochastic oscillators
    
    stochOSC <- stoch(symbol[, c("High", "Low", "Close")])
    
    
    book$i["stochOSC"] <- stochOSC
    book$d["stochOSC"] <- disc(stochOSC, nrow(stochOSC), discs)
    book$n <- c(book$n,  "stochOSC")
    
    
    cat("36")
    cat("\n")
    
    
    # williams percent r
    
    stochWPR <- WPR(symbol[, c("High", "Low", "Close")])
    
    
    book$i["stochWPR"] <- stochWPR
    book$d["stochWPR"] <- disc(stochWPR, nrow(stochWPR), discs)
    book$n <- c(book$n,  "stochWPR")
    
    
    
    cat("37")
    cat("\n")
    
    
    
    stoch2MA <-
      stoch(symbol[, c("High", "Low", "Close")], maType = list(list(SMA), list(EMA, wilder = TRUE), list(SMA)))
    
    
    book$i["stoch2MA"] <- stoch2MA
    book$d["stoch2MA"] <- disc(stoch2MA, nrow(stoch2MA), discs)
    book$n <- c(book$n,  "stoch2MA")
    
    
    cat("38")
    cat("\n")
    
    SMI3MA <-
      SMI(symbol[, c("High", "Low", "Close")], maType = list(list(SMA), list(EMA, wilder = TRUE), list(SMA)))
    
    
    book$i["SMI3MA"] <- SMI3MA
    book$d["SMI3MA"] <- disc(SMI3MA, nrow(SMI3MA), discs)
    book$n <- c(book$n,  "SMI3MA")
    
    
    cat("39")
    cat("\n")
    
    stochRSI <- stoch(RSI(symbol[, "Close"]))
    
    
    book$i["stochRSI"] <- stochRSI
    book$d["stochRSI"] <- disc(stochRSI, nrow(stochRSI), discs)
    book$n <- c(book$n,  "stochRSI")
    
    cat("40")
    cat("\n")
    
    # trend detection index
    
    mutation <- mute(p1)
    book$m["mdi"] <- mutation
    
    tdi <- TDI(symbol[, "Close"], n = 30 + mutation)
    
    book$i["tdi"] <- tdi
    book$d["tdi"] <- disc(tdi, nrow(tdi), discs)
    book$n <- c(book$n,  "tdi")
    
    
    cat("41")
    cat("\n")
    
    trix <- TRIX(symbol[, "Close"])
    
    book$i["trix"] <- trix
    book$d["trix"] <- disc(trix, nrow(trix), discs)
    book$n <- c(book$n,  "trix")
    
    
    cat("42")
    cat("\n")
    
    trix4 <-
      TRIX(symbol[, "Close"], maType = list(list(SMA), list(EMA, wilder = TRUE), list(SMA), list(DEMA)))
    
    book$i["trix4"] <- trix4
    book$d["trix4"] <- disc(trix4, nrow(trix4), discs)
    book$n <- c(book$n,  "trix4")
    
    
    cat("43")
    cat("\n")
    
    # ultimate oscillator
    
    ultOSC <-
      ultimateOscillator(symbol[, c("High", "Low", "Close")])
    
    book$i["ultOSC"] <- ultOSC
    book$d["ultOSC"] <- disc(ultOSC, nrow(ultOSC), discs)
    book$n <- c(book$n,  "ultOSC")
    
    cat("44")
    cat("\n")
    
    # vertical horizon filter
    
    vhf.close <- VHF(symbol[, "Close"])
    
    book$i["vhf.close"] <- vhf.close
    book$d["vhf.close"] <- disc(vhf.close, nrow(vhf.close), discs)
    book$n <- c(book$n,  "vhf.close")
    
    # custom
    
    cat("45")
    cat("\n")
    
    vhf.hilow <- VHF(symbol[, c("High", "Low", "Close")])
    
    
    book$i["vhf.hilow"] <- vhf.hilow
    book$d["vhf.hilow"] <- disc(vhf.hilow, nrow(vhf.hilow), discs)
    book$n <- c(book$n,  "vhf.hilow")
    
    # custom
    
    cat("46")
    cat("\n")
    
    
    
    
    # Volatility
    
    ohlc <- symbol[, c("Open", "High", "Low", "Close")]
    
    book$i["ohlc"] <- ohlc
    book$d["ohlc"] <- disc(ohlc, nrow(ohlc), discs)
    book$n <- c(book$n,  "ohlc")
    
    
    cat("47")
    cat("\n")
    
    vClose <- volatility(ohlc, calc = "close")
    
    
    book$i["vClose"] <-  vClose
    book$d["vClose"] <- disc(vClose, nrow(vClose), discs)
    book$n <- c(book$n,  "vClose")
    
    # custom
    
    cat("48")
    cat("\n")
    
    vGK <- volatility(ohlc, calc = "garman")
    
    
    book$i["vGK"] <- vGK
    book$d["vGK"] <- disc(vGK, nrow(vGK), discs)
    book$n <- c(book$n,  "vGK")
    
    # custom garman vol
    
    cat("49")
    cat("\n")
    
    vP <- volatility(ohlc, calc = "parkinson")
    
    
    book$i["vP"] <- vP
    book$d["vP"] <- disc(vP, nrow(vP), discs)
    book$n <- c(book$n,  "vP")
    
    # custom parkinson vol
    
    cat("50")
    cat("\n")
    
    vRS <- volatility(ohlc, calc = "rogers")
    
    
    book$i["vRS"] <- vRS
    book$d["vRS"] <- disc(vRS, nrow(book$i["vRS"]), discs)
    book$n <- c(book$n,  "vRS")
    
    cat("51")
    cat("\n")
    
    
    
    
    # williams a/d
    
    ad <- williamsAD(symbol[, c("High", "Low", "Close")])
    
    
    book$i["ad"] <- ad
    book$d["ad"] <- disc(ad, nrow(book$i["ad"]), discs)
    book$n <- c(book$n,  "ad")
    
    
    
    cat("52")
    cat("\n")
    
    # williams percent R
    
    
    
    
    stochWPR <- WPR(symbol[, c("High", "Low", "Close")])
    
    book$i["stochWPR"] <- stochWPR
    book$d["stochWPR"] <- disc(stochWPR, nrow(book$i["stochWPR"] ), discs)
    book$n <- c(book$n,  "stochWPR")
    
    
    
    cat("54")
    cat("\n")
    
    
    
    # zig zag
    
    mutation <- mute(p1)
    book$m["zz"] <- mutation
    
    zz <- ZigZag(symbol[, c("High", "Low")], change = 20 + mutation)
    
    book$i["zz"] <- zz
    book$d["zz"] <- disc(zz, nrow(zz), discs)
    book$n <- c(book$n,  "zz")
    
    
    cat("indicators")
    cat("\n")

  
  } # inds
