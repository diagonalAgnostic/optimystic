library(rpart)
library(ipred)
library(randomForest)
library(gbm)
library(C50)


source("stock.R") #  read(), indicatorss()

#################  BEGIN ML ###################

# run regression modeling for each symbol 

rpart<-function(symbol){

i<-indicators(symbol)

rt <- rpart(delta~., data=i)
  
predictions<- predict(rt, type="class") 

return(predictions)

} # rpart function


gbm<-function(symbol){
  
  i<-indicators(symbol)
  
  g <- gbm(delta~., data=i)
  
  predictions<- predict(g, type="class")
  
  return(predictions)
  
} # gbm


randomForest<-function(symbol){

  i<-indicators(symbol)
  
  rf <- randomForest(delta~., data=i)

  predictions<- predict(rf, type="class") 
  
  return(predictions)

} # random forest


bagging<-function(symbol){

  i<-indicators(symbol)
  
  bag <- bagging(delta~., data=i)
  
  predictions <- predict(bag, type="class") 
  
  return(predictions)

} # bagging


j48<-function(symbol){

  i<-indicators(symbol)
  
  j48 <- j48(delta~., data=i)
  
  predictions <- predict(j48, type="class") 
  
  return(predictions)
  
} # j48



PART<-function(symbol){

  i<-indicators(symbol)
  
  rt <- PART(delta~., data=i)
  
  predictions <- predict(rt,  type="class") 
  
  return(predictions)

} # PART


c5<-function(symbol){
  
  i<-indicators(symbol)
  
  c5 <- C5.0(delta~., data=i, trials=10)
  
  predictions<- predict(c5,  type="class") 
  
  return(predictions)
  
} # c5

# model data
learn <- function(data, star){
  
    # n, min, max, interval, radian
    symbol <- nuance(symbol, data, 10, 5, 1)
    cat("learnt")
    txtStart(paste(star, symbol, "_data.csv", sep = "."), commands=T, results=T, append=T, cmdfile, visible.only=T)
  
    summary( c5(symbol$i[["d"]]) )
    summary( PART(symbol$i[["d"]]) )
    summary( j48(symbol$i[["d"]]) )
    summary( bagging(symbol$i[["d"]]) )
    summary( randomForest(symbol$i[["d"]]) )
    summary( gbm(symbol$i[["d"]]) )
    summary( rpart(symbol$i[["d"]]) )
    
    summary( c5(symbol$p[["ep"]]) )
    summary( PART(symbol$p[["ep"]]) )
    summary( j48(symbol$p[["ep"]]) )
    summary( bagging(symbol$p[["ep"]]) )
    summary( randomForest(symbol$p[["ep"]]) )
    summary( gbm(symbol$p[["ep"]]) )
    summary( rpart(symbol$p[["ep"]]) )
    
    summary( c5(symbol$p[["ev"]]) )
    summary( PART(symbol$p[["ev"]]) )
    summary( j48(symbol$p[["ev"]]) )
    summary( bagging(symbol$p[["ev"]]) )
    summary( randomForest(symbol$p[["ev"]]) )
    summary( gbm(symbol$p[["ev"]]) )
    summary( rpart(symbol$p[["ev"]]) )
  
    #naive bayesian inference  
    summary( symbol$i[["pr"]] )
  
    
    
    txtStop()
    
   

}

# summarize the fit