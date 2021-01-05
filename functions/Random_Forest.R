library("rsample")      # data splitting 
library("randomForest") # basic implementation
library("ranger") 
library("caret")
library("mlbench")
library("e1071")
library("pROC")

Random_Forest <- function(OTU_input, group_index, seed_num = 9527, RF_group_setting){
  if(length(names(table(group_index))) > 2){
    new_group_index <- rep(0, length(group_index))
    for(i in 1: length(RF_group_setting)){
      new_group_index[which(group_index == RF_group_setting[i])] <- 1
    }
    set.seed(seed_num)
    output_RF <- randomForest(t(OTU_input), as.factor(new_group_index), importance = T)
    candidate_markers <- names(sort(output_RF$importance[,"MeanDecreaseAccuracy"], decreasing = T)[1:min(30, nrow(output_RF$importance))])
    rf.roc<-roc(new_group_index,output_RF$votes[,2])
    summary_text1 <- paste(nrow(OTU_input), " biomarker(s) were analyzed.", sep ="")
    summary_text2 <- paste(paste(RF_group_setting, collapse = ","), " V.S. ", paste(setdiff(names(table(group_index)), RF_group_setting), collapse = ","), sep = "")
    set.seed(seed_num)
    control <- rfeControl(functions=rfFuncs, method="cv", number=5)
    results <- rfe(t(OTU_input),as.factor(new_group_index), sizes = c(1:30), rfeControl=control)
    return(list(summary_text1 = summary_text1, summary_text2 = summary_text2, output_RF = output_RF, rf.roc = rf.roc, candidate_markers = candidate_markers, RFE = results))
  }
  else{
    set.seed(seed_num)
    output_RF <- randomForest(t(OTU_input), as.factor(group_index), importance = T)
    candidate_markers <- names(sort(output_RF$importance[,"MeanDecreaseAccuracy"], decreasing = T)[1:min(30, nrow(output_RF$importance))])
    rf.roc<-roc(group_index,output_RF$votes[,2])
    summary_text1 <- paste(nrow(OTU_input), " biomarker(s) were analyzed.", sep ="")
    summary_text2 <- paste(names(table(group_index))[1], " V.S. ", names(table(group_index))[2], sep = "")
    set.seed(seed_num)
    control <- rfeControl(functions=rfFuncs, method="cv", number=5)
    results <- rfe(t(OTU_input),as.factor(group_index), sizes = c(1:30), rfeControl=control)
    return(list(summary_text1 = summary_text1, summary_text2 = summary_text2,  output_RF = output_RF, rf.roc = rf.roc, candidate_markers = candidate_markers, RFE = results))
  }

}