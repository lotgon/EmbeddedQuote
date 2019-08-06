rm(list=ls())
source("AutoEncoderModel.R")
source("PrepareData.R")
library(manipulate)

d <- GetData(c("EURCHF","EURUSD", "AUDNZD"), startDate=ISOdate(2013, 1, 1, 0, 0, 0, tz = "UTC"), endDate=ISOdate(2019, 1, 1, 0, 0, 0, tz = "UTC"), isNormalize = T) [.(close_bid)]
d_test <- GetData(c("EURCHF","EURUSD", "AUDNZD"), startDate=ISOdate(2019, 1, 1, 0, 0, 0, tz = "UTC"), endDate=ISOdate(2019, 3, 1, 0, 0, 0, tz = "UTC"), isNormalize = T)[.(close_bid)]
generator <- AutoEncoder_CreateXYGenerator(d)
generator_test <- AutoEncoder_CreateXYGenerator(d_test)

#models <- CreateModelDense()
models <- AutoEncoder_CreateModelConv()
models$autoEncoder%>% fit_generator(generator, 200, 800, validation_data = generator_test, validation_steps=30)

#save_model_hdf5(models$autoEncoder, "autoencoderRates")
#save_model_hdf5(models$encodeModel, "encodeModelRates")
#save_model_hdf5(models$decodeModel, "decodeModelRates")

models$autoEncoder <- load_model_hdf5("autoencoderRates")
models$encodeModel <- load_model_hdf5("encodeModelRates")
models$decodeModel <- load_model_hdf5("decodeModelRates")


par(mfrow=c(2,1))
testData <- generator(1)[[1]]
plot(( testData[1,,1]))
code  <- models$encodeModel$predict(testData)
generated <- models$decodeModel$predict(code)
plot(( generated[1,,1]))

draw <- function(models, code, value){
  code[3] = value
  generated <- models$decodeModel$predict(code)
  plot(generated[1,,1])
}
manipulate(draw(models, code, value), value=slider(-10, 10))

