library("rsample")      # data splitting 
library("randomForest") # basic implementation
library("ranger") 
library("caret")
library("mlbench")
library("e1071")
library("pROC")

RF_Err_Rate_Plot <- function(RF_input){
  plot(RF_input$err.rate[,1], type = "l", col = "red", main = "Estimated Performance", xlab = "Number of trees", ylab = "Error Rate", ylim = c(0, 0.6), cex.lab = 1.4,cex.axis = 1.3, cex.main = 2, cex = 2)
}