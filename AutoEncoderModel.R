require(keras)
source("parameters.R")

AutoEncoder_CreateModelConv <- function () {
  encodeInput <- layer_input(shape = c(NULL, Tx, Nx))
  encodeOutput <-  encodeInput%>%
    layer_reshape(c(Tx, 1, Nx)) %>%
    layer_conv_2d(8, c(9,1), padding = "same", activation = 'elu') %>% 
    layer_max_pooling_2d(c(2,1)) %>%
    layer_conv_2d(16, c(7,1), padding = "same", activation = 'elu') %>% 
    layer_max_pooling_2d(c(2,1)) %>%
    layer_conv_2d(16, c(5,1), padding = "same", activation = 'elu') %>% 
    layer_max_pooling_2d(c(2,1)) %>%
#    layer_conv_2d(8, c(3,1), padding = "same", activation = 'elu') %>% 
#    layer_max_pooling_2d(c(2,1)) %>%
    layer_conv_2d(32, c(3,1), padding = "same", activation = 'elu') %>% 
    layer_max_pooling_2d(c(5,1)) %>%
    #layer_conv_2d(16, c(3,1), padding = "same", activation = 'elu') %>% 
    #layer_max_pooling_2d(c(5,1)) %>%
    layer_flatten() %>%
    layer_dense(units = 8*Ny, activation = 'elu') %>%
    layer_dense(units = 4*Ny, activation = 'elu') %>%
    layer_dense(units = Ny) 
  encodeModel <- keras_model(encodeInput, encodeOutput)
  
  decodeInput <- layer_input(shape = c(NULL, Ny))
  decodeOutput <-  decodeInput%>%
    layer_dense(units = 4*Ny, activation = 'elu') %>% 
    layer_dense(units = 8*Ny, activation = 'elu') %>%
    layer_dense(units = 32*Tx/8/5, activation = 'elu') %>%
    layer_reshape(c(Tx/8/5, 1, 32)) %>%
    layer_conv_2d_transpose(16, c(3, 1), strides = c(5, 1), activation = "elu", padding = "same") %>%
    layer_conv_2d_transpose(16, c(5, 1), strides = c(2, 1), activation = "elu", padding = "same") %>%
    layer_conv_2d_transpose(8, c(7, 1), strides = c(2, 1), activation = "elu", padding = "same") %>%
    layer_conv_2d_transpose(Nx, c(9, 1), strides = c(2, 1), padding = "same") %>%
    layer_reshape(c(Tx, Nx))%>%
    layer_conv_1d(Nx, 1, padding="same")
    
  decodeModel <- keras_model(decodeInput, decodeOutput)
  
  input <- layer_input(shape = c(NULL, Tx, Nx))
  code <- input %>% encodeModel
  reconstruction <- decodeModel(code)
  
  autoEncoder <- keras_model(input, reconstruction)
  summary(autoEncoder)
  autoEncoder %>% compile(
    loss = 'mean_absolute_error',
    optimizer = optimizer_adamax(),
    metrics = c('accuracy')
  )
  list(autoEncoder=autoEncoder,  encodeModel=encodeModel, decodeModel=decodeModel)
}