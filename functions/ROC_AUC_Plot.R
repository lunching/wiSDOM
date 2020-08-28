library("pROC")
library("verification")

ROC_AUC_Plot <- function(data_all, group_index, marker_name, ROC_group_setting){
  if(length(names(table(group_index))) > 2){
    new_group_index <- rep(0, length(group_index))
    for(i in 1: length(ROC_group_setting)){
      new_group_index[which(group_index == ROC_group_setting[i])] <- 1
    }
    index_match_tmp <- match(marker_name, rownames(data_all))
    prediction_tmp <- data_all[index_match_tmp, ]
    ROC_tmp1 <- roc(new_group_index, as.numeric(prediction_tmp), auc = T, ci = T, direction = ">")
    ROC_tmp2 <- roc(new_group_index, as.numeric(prediction_tmp), auc = T, ci = T, direction = "<")
    index_max_AUC_tmp <- which.max(c(ROC_tmp1$auc[1], ROC_tmp2$auc[1]))
    if(index_max_AUC_tmp == 1){
      ROC_tmp <- ROC_tmp1
    }
    else{
      ROC_tmp <- ROC_tmp2
    }
    plot.roc(ROC_tmp, main = paste("ROC_", marker_name, sep = ""), cex.lab = 1.4,cex.axis = 1.3, cex.main = 2)
  }
  if(length(names(table(group_index))) == 2){
    index_match_tmp <- match(marker_name, rownames(data_all))
    prediction_tmp <- data_all[index_match_tmp, ]
    ROC_tmp1 <- roc(as.factor(group_index), as.numeric(prediction_tmp), auc = T, ci = T, direction = ">")
    ROC_tmp2 <- roc(as.factor(group_index), as.numeric(prediction_tmp), auc = T, ci = T, direction = "<")
    index_max_AUC_tmp <- which.max(c(ROC_tmp1$auc[1], ROC_tmp2$auc[1]))
    if(index_max_AUC_tmp == 1){
      ROC_tmp <- ROC_tmp1
    }
    else{
      ROC_tmp <- ROC_tmp2
    }
    plot.roc(ROC_tmp, main = paste("ROC_", marker_name, sep = ""), cex.lab = 1.4,cex.axis = 1.3, cex.main = 2)
  }
}
