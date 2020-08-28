library("rsample")      # data splitting 
library("randomForest") # basic implementation
library("ranger") 
library("caret")
library("mlbench")
library("e1071")
library("pROC")

RF_ROC_Plot <- function(ROC_input){
  plot(ROC_input, main = "ROC_RF", cex.lab = 1.4,cex.axis = 1.3, cex.main = 2)
  text(x = .5, y = .25 ,paste("AUC = ", signif(auc(ROC_input)[1], 2), sep = ""), cex = 2)
}