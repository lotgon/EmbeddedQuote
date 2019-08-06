require(foreach)
require(data.table)
library(caret)
source("parameters.R")
source("DownloadQuotes.R")

GetData <- function(symbols, startDate, endDate, isNormalize = FALSE){
  
  d <- lapply(symbols, function(symbol){
    asks = GetQuotes(symbol, startDate, endDate, periodicity="S1", type="Asks")
    asks[, volume:=NULL]
    bids = GetQuotes(symbol, startDate, endDate, periodicity="S1", type="Bids")
    bids[, volume:=NULL]
    
    ab  <- merge(asks, bids, by="datetime", suffixes = c("_ask", "_bid"))
    ab[,datetime:=NULL]
    ab <- ab[,close_spread:=close_ask-close_bid]

    if( isNormalize ){
      print(paste0("close_bid sd=", ab[,sd(close_bid)], ", close_spread sd=", ab[,sd(close_spread)]))
      ab <- ab[,close_spread:=(close_spread-mean(close_spread))/sd(close_spread)]
      ab <- ab[,close_bid:=(close_bid-mean(close_bid))/sd(close_bid)]
      ab <- ab[,close_ask:=(close_ask-mean(close_ask))/sd(close_ask)]
    }
    
    ab[,.(close_bid, close_spread, close_ask)]
  })
  d
}

AutoEncoder_CreateXYGenerator <- function(listOfDT_bars){
  n_symbols <- length(listOfDT_bars)
  function(batchSize = 256){
    xBatch <- array(0, dim=c(batchSize, Tx, Nx))
    randSymbols <- sample.int(n_symbols, batchSize, replace = TRUE)
    randIndex <- sample.int(2^31, batchSize)
    
    for(i in 1:batchSize){
      data <- listOfDT_bars[[randSymbols[i]]]
      randomStart <- randIndex[[i]]%%data[,.N-Tx+1]+1
      data = data[randomStart:(randomStart+Tx-1),.(close_bid)]
      xBatch[i,,] <- as.matrix(data)
    }
    list(xBatch, xBatch)
  }
}

FutureBar_CreateXYGenerator <- function(listOfDT_bars){
  n_symbols <- length(listOfDT_bars)
  preObj <- lapply(listOfDT_bars, preProcess)
  
  function(batchSize = 256){
    xBatch <- array(0, dim=c(batchSize, Tx, bar_nx))
    yBatch <- array(0, dim=c(batchSize, bar_y))
    randSymbols <- sample.int(n_symbols, batchSize, replace = TRUE)
    randIndex <- sample.int(2^31, batchSize)
    
    for(i in 1:batchSize){
      data <- listOfDT_bars[[randSymbols[i]]]
      randomStart <- randIndex[[i]]%%data[,.N-Tx+1 - bar_ty ]+1
      
      dataX = predict( preObj[[randSymbols[i]]], data[randomStart:(randomStart+Tx-1)])
      xBatch[i,,] <- as.matrix(dataX[, .(close_bid, close_spread, symbolId=randSymbols[i])])
      
      dataY = data[(randomStart+Tx):(randomStart+Tx+bar_ty-1),.(close_bid, close_ask)]
      #!! error
      yBatch[i,1] <- dataY[1,close_bid] - dataY[.N,close_ask]
      yBatch[i,2] <- dataY[.N,close_bid] - dataY[1,close_ask]
      yBatch[i,3] <- data[randomStart+Tx-1, close_bid]
      yBatch[i,4] <- data[randomStart+Tx-1+bar_ty,close_ask]
    }
    list(xBatch, yBatch)
  }
}

  