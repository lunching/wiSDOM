library("MASS")
library("ROCR")

LDA_ES <- function(OTU_input, group_index, cut_p, cut_LDA_log_score, prop_val = 0.25, seed_num = 9527){
  ## Kruskal-Wallis test ##
  summary_text1 <- paste(nrow(OTU_input), " biomarker(s) were analyzed.", sep ="")
  p_KW_tmp <- c()
  for(i in 1: nrow(OTU_input)){
    
    OTU_tmp <- OTU_input[i, ]
      test_tmp <- kruskal.test(as.numeric(OTU_tmp), as.factor(group_index))
      p_KW_tmp[i] <- signif(test_tmp$p.value, 4)
  }
  
  index_LDA <- which(p_KW_tmp < cut_p)
  if(length(index_LDA) == 0){
    summary_text2 <- paste("There are ", length(index_LDA), " biomarker(s) remain from p values of KW-test less than ", cut_p, ", please re-run by selecting more conservative p value threshold greater than ", cut_p, sep = "")
    summary_text3 <- c("")
    candidate_markers <- c("")
    summary_table <- c("")
  }
  else{
    summary_text2 <- paste("There are ", length(index_LDA), " biomarker(s) remain from p values of KW-test less than ", cut_p, ".", sep = "")
    new_LDA_data <- as.data.frame(cbind(group_index, t(OTU_input[index_LDA, ])) )
    p_pass <- signif(p_KW_tmp[index_LDA], 4) 
    ## Run LDA ##
    #new_LDA_data[,1] <- factor(new_LDA_data[,1])
    #colnames(new_LDA_data)[1] <- "Groups"
    ## Checking constant value within groups 
    
    LDA_output <- lda(as.factor(group_index) ~ t(OTU_input[index_LDA, ]), tol = 1.0e-8)
    LDA_scores <- log(abs(LDA_output$scaling[,"LD1"]))
    LDA_means <- signif(LDA_output$means, 4)
    index_LDA2 <- which(LDA_scores > cut_LDA_log_score)
    if(length(index_LDA2) == 0){
      summary_text3 <- paste("There are ", length(index_LDA2), " biomarker(s) passed log LDA score greater than ", cut_LDA_log_score, ", please re-run by selecting more conservative p value or log LDA score threshold!", sep = "")
      candidate_markers <- c("")
      summary_table <- c("")
    }
    else{
      summary_text3 <- paste("There are ", length(index_LDA2), " biomarker(s) passed log LDA score greater than ", cut_LDA_log_score, ". ROC for multiple biomarker(s) prediction can be generated here, you can also access interactive ROC and AUC for single markers in next module!", sep = "")
      candidate_markers <- rownames(OTU_input[index_LDA, ])[index_LDA2]
      summary_table <- cbind(candidate_markers, t(LDA_means[,index_LDA2]), LDA_scores[index_LDA2], p_pass[index_LDA2])
      colnames(summary_table) <- c("Biomarker", paste("Mean_", rownames(LDA_means), sep =""), "log_LDA_scores", "KW_P")
      ## Prepare data frame for ROC ##
      if(length(names(table(group_index))) ==2 ){
        index_matched <- match(candidate_markers, rownames(OTU_input))
        data_4_ROC <- as.data.frame(cbind(group_index, as.data.frame(as.matrix(t(OTU_input[index_matched, ]) ))))
        colnames(data_4_ROC)[2: ncol(data_4_ROC)] <- paste("v", 2:ncol(data_4_ROC), sep ="")
        colnames(data_4_ROC)[1] <- "group"
        smp_size_raw <- floor((1 - prop_val) * nrow(data_4_ROC))
        set.seed(seed_num)
        train_ind_raw <- sample(nrow(data_4_ROC), size = smp_size_raw)
        train_raw.df <- as.data.frame(data_4_ROC[train_ind_raw, ])
        test_raw.df <- as.data.frame(data_4_ROC[-train_ind_raw, ])
        f <- paste(names(train_raw.df)[1], "~", paste(names(train_raw.df)[-1], collapse=" + "))
        data_raw.lda <- lda(as.formula(paste(f)), data = train_raw.df, tol = 1.0e-8)
        data_raw.lda.predict <- predict(data_raw.lda, newdata = test_raw.df)
        data_raw.lda.predict.posteriors <- as.data.frame(data_raw.lda.predict$posterior)
        # Evaluate the model
        pred <- prediction(data_raw.lda.predict.posteriors[,2], test_raw.df[,1])
        roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
        auc.train <- performance(pred, measure = "auc")
        auc.train <- auc.train@y.values
      }
      else{
        roc.perf<- c("")
        auc.train <- c("")
      }

    }
  }
  return(list(summary_text1 = summary_text1, summary_text2 = summary_text2, summary_text3 = summary_text3, candidate_markers = candidate_markers, summary_table = summary_table, roc.perf = roc.perf, auc.train = auc.train))
}