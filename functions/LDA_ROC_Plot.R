
LDA_ROC <- function(ROC_input, AUC_input){
  plot(ROC_input, main = "ROC Curve of multiple biomarkers prediction", cex.lab = 1.4,cex.axis = 1.3, cex.main = 2)
  abline(a=0, b= 1)
  text(x = .85, y = .25 ,paste("AUC = ", round(AUC_input[[1]],3), sep = ""), cex = 2)
}