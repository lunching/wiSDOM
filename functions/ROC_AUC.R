library("pROC")
library("verification")

ROC_AUC <- function(data_all, group_index, marker_name, ROC_group_setting){
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
    summary_text1 <- paste(paste(ROC_group_setting, collapse = ","), " V.S. ", paste(setdiff(names(table(group_index)), ROC_group_setting), collapse = ","), sep = "")
    summary_text2 <- paste("AUC = ", signif(ROC_tmp$auc[1], 2), " and 95% confidence interval is (", signif(ROC_tmp$ci[1], 2), ", ", signif(ROC_tmp$ci[3], 2), ").", sep = "")
    return(list(summary_text1 = summary_text1, summary_text2 = summary_text2))
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
    summary_text1 <- paste(names(table(group_index))[1], " V.S. ", names(table(group_index))[2], sep = "")
    summary_text2 <- paste("AUC = ", signif(ROC_tmp$auc[1], 2), " and 95% confidence interval is (", signif(ROC_tmp$ci[1], 2), ", ", signif(ROC_tmp$ci[3], 2), ").", sep = "")
    return(list(summary_text1 = summary_text1, summary_text2 = summary_text2))
  }

}



