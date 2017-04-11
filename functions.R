
phi <- function(v, n) {
  phi = (v * (n + 1)) / (v * n)
  
  return(phi)
  
}



nas <- function() {
  nas <-
    read.csv(file = "~/Documents/R/NASDAQComposite.csv",
             header = TRUE,
             sep = ",")
  
  return(nas)
  
}

snp <- function() {
  snp <-
    read.csv(file = "~/Documents/R/SP500.csv",
             header = TRUE,
             sep = ",")
  
  return(snp)
}

nyse <- function() {

  Data <- read.zoo(
    "~/Documents/R/NYSEComposite.csv",
    index.column = 0,
    sep = ",",
    header = TRUE,
    FUN = as.POSIXct
  )
  nyse <- as.xts(Data)
  
  return(nyse)
  
}

import <- function(symbol) {
  
  Data <- read.zoo(symbol,
    index.column = 1,
    sep = ",",
    header = TRUE,
    FUN = as.POSIXct
  )
  
  d<-ncol(Data)
  cat(d)
  cat("d")
  cat("\n")
  
  file <- as.xts(Data)
  
  return(file)
  
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
      "treb[tree]",
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


makeModels <- function(ins) {
  
  modes <- models()
  operators <- operators()
  
  cat("modes, inds, operators")
  cat("\n")
  
  o <- length(operators)
  i = (o + 1)
  
  #set formula to compare every dimension
  
  form1 = "Y ~ ."
  #form2 <- c("Y ~ ., data = stock[[\"indicatorData\"]$D")
  
  cat("form1")
  cat("\n")
  
  
  # get random samples from both the operator list and the indicator list
  
  oSample <- sample(operators, o, replace = T)
  iSample <- sample(ins, i, replace = T)
  
  cat("samples")
  # interleave the operators and indicators to make a formula
  cat("\n")
  
  interleaved <-
    paste(order (c (
      seq_along(iSample), seq_along(oSample)
    )), sep = '')
  
  form3 <-
    paste("Y ~ ", interleaved, sep="")
  # form4 <-
  #  c("Y ~ ", interleaved, "data = stock[[\"indicatorData\"]$D")
  cat("interleaved")
  cat("\n")
  
  for (mo in modes) {
    
    # convert to someething the program can understand
    
    cat(mo)
    cat("model")
    cat("\n")
    
    f1 <- as.formula(form1)
    # f2 <- as.formula(form2)
    
    f3 <- as.formula(form3)
    # f4 <- as.formula(form4)
    
    cat("formulae")
    cat("\n")
    
    # run each formula through the models for feature selection and correlation
    mod1 <- specifyModel(f1, na.rm = T) #  <- don't include any incomplete rows
    
    #  mod2 <- specifyModel(f2, na.rm = T)
    cat("mod1")
    cat("\n")
    # mod3 <-
    #  specifyModel(f3, na.rm = T) #  <- don't include any incomplete rows
    
    mod4 <- specifyModel(f4, na.rm = T)
    
    cat("mod4")
    cat("\n")
    
    # all indicators
    
    m1 <-
      buildModel(m1, mo, training.per = c(d$head, d$tail))
    
    #   m2 <-
    #    buildModel(m2, mo, training.per = c(stock$head, stock$tail))
    
    # stochastic formula
    
    m3 <-
      buildModel(m3, mo, training.per = c(d$head, d$tail))
    
    cat("models built")
    cat("\n")
    
    #   m4 <-
    #    buildModel(m4, mo, training.per = c(stock$head, stock$tail))
    
    #add models to stock data
    
    modes<-list()
    modes<-list(m1, m3)
    
    d[["modes"]]= modes
    cat("models assigned")
    cat("\n")
    
    # write model summary data to file
    
    # txtStart(c(stock$symbol, txtStop(), "formulae.txt", sep = "_"))
    
    # txtStop()
    
    
  } # mo in modes
  
  cat("models returned")
  cat("\n")
  
  return(d)
  
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


radiance <- function (p, min, max) {
 
  m <- runif(1, min, max)
 
  rad <- floor(p - m)
  
  return(rad)
  
} # getRadiance


mute <- function (v) {
  
  mutation <- sample(v, 1)
  
  return(mutation)
  
}
