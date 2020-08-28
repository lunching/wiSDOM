library("rsample")      # data splitting 
library("randomForest") # basic implementation
library("ranger") 
library("caret")
library("mlbench")
library("e1071")
library("pROC")

RF_Var_Imp_Plot <- function(RF_input){
  varImpPlot(RF_input, type = 1)
}