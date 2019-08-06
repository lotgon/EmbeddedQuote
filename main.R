source("parameters.R")
source("PrepareData.R")
source("AutoEncoderModel.R")
#library(mlr)
library(xgboost)


quotes <- GetData(c("EURCHF"), startDate=ISOdate(2018, 1, 1, 0, 0, 0, tz = "UTC"), endDate=ISOdate(2019, 2, 1, 0, 0, 0, tz = "UTC"))
test_quotes <- GetData(c("EURCHF"), startDate=ISOdate(2019, 1, 1, 0, 0, 0, tz = "UTC"), endDate=ISOdate(2019, 2, 1, 0, 0, 0, tz = "UTC"))


gen <- FutureBar_CreateXYGenerator(quotes)
inputData <- gen(50000)

EncodeXGBInputData <- function(dataBatch){
  if( !exists("encodeModel"))
    encodeModel <- load_model_hdf5("encodeModelRates")
  rateShape <- encodeModel$predict(dataBatch[[1]][,,1,drop=F])
  spreadShape <- encodeModel$predict(dataBatch[[1]][,,2,drop=F])
  symbolsId <- matrix(dataBatch[[1]][,1,3], ncol=1)
  cbind( rateShape, spreadShape)
}

#xgb <- xgb.train(data = xgb.DMatrix(data = EncodeXGBInputData(inputData),label = inputData[[2]][,1]), nrounds = 200, verbose=1)
xgb <- xgboost(xgb.DMatrix(data = EncodeXGBInputData(inputData), label = inputData[[2]][,1]), nrounds = 2000)

test_gen <- FutureBar_CreateXYGenerator(test_quotes)
###!!!!!!!!!!
test_inputData <- gen(1000)

d<-data.table( test_inputData[[2]] )
d[,predicted:=predict(xgb, xgb.DMatrix(data = EncodeXGBInputData(test_inputData)))]
d[,profit:=0]
#d[predicted < V3 ,profit:= V3-V4]
#d[predicted > V3 ,profit:= V4-V3]
d[predicted > 0.0 ,profit:= V1]
d
summary(d$profit)
hist(d$profit)
