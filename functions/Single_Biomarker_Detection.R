library("dunn.test")

Marker_detection_v3 <- function(OTU_input, group_index, method = "W", ad_hoc = "1", cut_type = "1", cut = 0.05){
  ## Method: "T-test": Two sample T-test, "Wilcoxon rank-sum test": Wilcoxon Rank Sum test 
  if(length(names(table(group_index))) == 2){
    if(method == "T-test"){
      ## Two groups: T-test ##
      test_summary <- matrix(NA, nrow = nrow (OTU_input), ncol = 7)
      colnames(test_summary) <- c("Biomarker", paste("No_0", names(table(group_index)), sep = "_"), paste("Mean", names(table(group_index)), sep = "_"), "P_value", "q_value")
      for(i in 1: nrow(OTU_input)){
        OTU_tmp <- OTU_input[i, ]
          test_tmp <- t.test(as.numeric(OTU_tmp[which(group_index == names(table(group_index))[1])]),  as.numeric(OTU_tmp[which(group_index == names(table(group_index))[2])]))
          test_summary[i, paste("Mean", names(table(group_index))[1], sep = "_")] <- signif(mean(as.numeric(OTU_tmp[which(group_index == names(table(group_index))[1])])), 4)
          test_summary[i, paste("Mean", names(table(group_index))[2], sep = "_")] <- signif(mean(as.numeric(OTU_tmp[which(group_index == names(table(group_index))[2])])), 4)
          test_summary[i, paste("No_0", names(table(group_index))[1], sep = "_")] <- length(which(as.numeric(OTU_tmp[which(group_index == names(table(group_index))[1])]) == 0))
          test_summary[i, paste("No_0", names(table(group_index))[2], sep = "_")] <- length(which(as.numeric(OTU_tmp[which(group_index == names(table(group_index))[2])]) == 0))
          test_summary[i, "Biomarker"] <- rownames(OTU_input)[i]
          test_summary[i, "P_value"] <- signif(test_tmp$p.value, 4)
      
      }
      test_summary2 <- test_summary[!is.na(test_summary[,"P_value"]), ]
      test_summary2[ , "q_value"] <- signif(p.adjust(as.numeric(test_summary2[ ,"P_value"]), "BH"), 4)
    }
    if(method == "Wilcoxon rank-sum test"){
      test_summary <- matrix(NA, nrow = nrow (OTU_input), ncol = 7)
      colnames(test_summary) <- c("Biomarker", paste("No_0", names(table(group_index)), sep = "_"), paste("Mean", names(table(group_index)), sep = "_"), "P_value", "q_value")
      for(i in 1: nrow(OTU_input)){
        OTU_tmp <- OTU_input[i, ]
          test_tmp <- wilcox.test(as.numeric(OTU_tmp[which(group_index == names(table(group_index))[1])]),  as.numeric(OTU_tmp[which(group_index == names(table(group_index))[2])]))
          test_summary[i, paste("Mean", names(table(group_index))[1], sep = "_")] <- signif(mean(as.numeric(OTU_tmp[which(group_index == names(table(group_index))[1])])), 4)
          test_summary[i, paste("Mean", names(table(group_index))[2], sep = "_")] <- signif(mean(as.numeric(OTU_tmp[which(group_index == names(table(group_index))[2])])), 4)
          test_summary[i, paste("No_0", names(table(group_index))[1], sep = "_")] <- length(which(as.numeric(OTU_tmp[which(group_index == names(table(group_index))[1])]) == 0))
          test_summary[i, paste("No_0", names(table(group_index))[2], sep = "_")] <- length(which(as.numeric(OTU_tmp[which(group_index == names(table(group_index))[2])]) == 0))
          test_summary[i, "Biomarker"] <- rownames(OTU_input)[i]
          test_summary[i, "P_value"] <- signif(test_tmp$p.value, 4)
      }
      test_summary2 <- test_summary[!is.na(test_summary[,"P_value"]), ]
      test_summary2[ , "q_value"] <- signif(p.adjust(as.numeric(test_summary2[ ,"P_value"]), "BH"), 4)
    }
    
  }
  if(length(names(table(group_index))) > 2){
    ## More than two groups: "Analysis of variance (one-way ANOVA)": One-way anova, "Kruskal-Wallis test": Kruskal-Wallis Test 
    if(method == "Analysis of variance (one-way ANOVA)"){
      test_summary <- matrix(NA, nrow = nrow (OTU_input), ncol = 3 + (length(names(table(group_index)))*2) )
      colnames(test_summary) <- c("Biomarker", paste("No_0", names(table(group_index)), sep = "_"), paste("Mean", names(table(group_index)), sep = "_"), "P_value", "q_value")
      for(i in 1: nrow(OTU_input)){
        OTU_tmp <- OTU_input[i, ]
        #if(sd(OTU_tmp[which(group_index == names(table(group_index))[1])])!= 0& sd(OTU_tmp[which(group_index == names(table(group_index))[2])])!= 0&sd(OTU_tmp[which(group_index == names(table(group_index))[3])])!= 0){ #need modify
        if(all(sapply(1: length(names(table(group_index))), function(k) sd(OTU_tmp[which(group_index == names(table(group_index))[k])]))!=0)){
          test_tmp <- aov(as.numeric(OTU_tmp)~as.factor(group_index))
          for(j in 1:length(names(table(group_index)))){
            test_summary[i, paste("Mean", names(table(group_index))[j], sep = "_")] <- signif(mean(as.numeric(OTU_tmp[which(group_index == names(table(group_index))[j])])), 4)
            test_summary[i, paste("No_0", names(table(group_index))[j], sep = "_")] <- length(which(as.numeric(OTU_tmp[which(group_index == names(table(group_index))[j])]) == 0))
          }
          test_summary[i, "Biomarker"] <- rownames(OTU_input)[i]
          test_summary[i, "P_value"] <- signif(summary(test_tmp)[[1]][["Pr(>F)"]][1], 4)
        }
      }
      
      test_summary2 <- test_summary[!is.na(test_summary[,"P_value"]), ]
      test_summary2[ , "q_value"] <- signif(p.adjust(as.numeric(test_summary2[ ,"P_value"]), "BH"), 4)
      if(ad_hoc == "2"){
        ## Turkey HSD Test (post-hoc test)
        pairwise_p_matrix <- matrix(NA, nrow = nrow(test_summary2), ncol = cumsum(1: (length(names(table(group_index))) - 1))[(length(names(table(group_index))) - 1)])
        for(i in 1: nrow(test_summary2)){
          OTU_tmp <- OTU_input[match(test_summary2[i, "Biomarker"], rownames(OTU_input)), ]
          #p_matrix_tmp2 <- dunn.test(as.numeric(OTU_tmp), group_index, method = "bh")
          p_matrix_tmp <- TukeyHSD(aov(as.numeric(OTU_tmp)~as.factor(group_index)))
          pairwise_p_tmp <- signif(p_matrix_tmp$`as.factor(group_index)`[,"p adj"], 4)
          names(pairwise_p_tmp) <- names(p_matrix_tmp$`as.factor(group_index)`[,1])
          pairwise_p_matrix[i, ] <- pairwise_p_tmp
        }
        colnames(pairwise_p_matrix) <- names(pairwise_p_tmp)
        test_summary2 <- cbind(test_summary2, pairwise_p_matrix)
      }
      
    }
    if(method == "Kruskal-Wallis test"){
      test_summary <- matrix(NA, nrow = nrow (OTU_input), ncol = 3 + (length(names(table(group_index)))*2) )
      colnames(test_summary) <- c("Biomarker", paste("No_0", names(table(group_index)), sep = "_"), paste("Mean", names(table(group_index)), sep = "_"), "P_value", "q_value")
      for(i in 1: nrow(OTU_input)){
        OTU_tmp <- OTU_input[i, ]
        #if(sd(OTU_tmp[which(group_index == names(table(group_index))[1])])!= 0& sd(OTU_tmp[which(group_index == names(table(group_index))[2])])!= 0&sd(OTU_tmp[which(group_index == names(table(group_index))[3])])!= 0){ #need modify
        if(all(sapply(1: length(names(table(group_index))), function(k) sd(OTU_tmp[which(group_index == names(table(group_index))[k])]))!=0)){
          test_tmp <- kruskal.test(as.numeric(OTU_tmp), as.factor(group_index))
          for(j in 1:length(names(table(group_index)))){
            test_summary[i, paste("Mean", names(table(group_index))[j], sep = "_")] <- signif(mean(as.numeric(OTU_tmp[which(group_index == names(table(group_index))[j])])), 4)
            test_summary[i, paste("No_0", names(table(group_index))[j], sep = "_")] <- length(which(as.numeric(OTU_tmp[which(group_index == names(table(group_index))[j])]) == 0))
          }
          test_summary[i, "Biomarker"] <- rownames(OTU_input)[i]
          test_summary[i, "P_value"] <- signif(test_tmp$p.value, 4)
        }
      }
      test_summary2 <- test_summary[!is.na(test_summary[,"P_value"]), ]
      test_summary2[ , "q_value"] <- signif(p.adjust(as.numeric(test_summary2[ ,"P_value"]), "BH"), 4)
      if(ad_hoc == "2"){
        ## Dunn's Test (post-hoc test)
        pairwise_p_matrix <- matrix(NA, nrow = nrow(test_summary2), ncol = cumsum(1: (length(names(table(group_index))) - 1))[(length(names(table(group_index))) - 1)])
        for(i in 1: nrow(test_summary2)){
          OTU_tmp <- OTU_input[match(test_summary2[i, "Biomarker"], rownames(OTU_input)), ]
          #p_matrix_tmp <- pairwise.t.test(as.numeric(OTU_tmp), group_index, p.adjust.method = "none")$p.value
          p_matrix_tmp <- dunn.test(as.numeric(OTU_tmp), group_index, method = "bh")
          pairwise_p_tmp <- signif(p_matrix_tmp$P, 4)
          names(pairwise_p_tmp) <- p_matrix_tmp$comparisons
          pairwise_p_matrix[i, ] <- pairwise_p_tmp
        }
        colnames(pairwise_p_matrix) <- names(pairwise_p_tmp)
        test_summary2 <- cbind(test_summary2, pairwise_p_matrix)
      }
    }
  }
  #return(test_summary2)
  ## Filter out by p or FDR and generate message and table ##
  if(cut_type == "1"){
    index_keep <- which(as.numeric(test_summary2[,"P_value"]) < cut)
    if(length(index_keep) > 0){
      test_summary2 <- as.data.frame(test_summary2)
      test_summary3 <- test_summary2[index_keep, ]
      summary_text1 <- paste("There are ", nrow(OTU_input), " biomarker(s) in your data set, and ", length(index_keep)," biomarker's p value less than ", cut, sep ="")
      summary_text2 <- paste("The list of selected biomarker(s) are ready for interactive ROC and AUC in next module!", sep ="")
      candidate_markers <- as.character(unlist(test_summary3["Biomarker"]))
      summary_table <- test_summary3
    }
    else{
      summary_text1 <- paste("There are ", nrow(OTU_input), " biomarker(s) in your data set, and 0 biomarker's p value less than ", cut, sep ="")
      summary_text2 <- paste("Please re-run this procedure using more conservative threshold!", sep ="")
      candidate_markers <- "No biomarker available, please run one biomarker discovery method in step 2 under Statistical Analysis Module"
      summary_table <- c("")
    }
  }
  if(cut_type == "2"){
    index_keep <- which(as.numeric(test_summary2[,"q_value"]) < cut)
    if(length(index_keep) > 0){
      test_summary2 <- as.data.frame(test_summary2)
      test_summary3 <- test_summary2[index_keep, ]
      summary_text1 <- paste("There are ", nrow(OTU_input), " biomarker(s) in your data set, and ", length(index_keep)," biomarker's FDR (q value) less than ", cut, sep ="")
      summary_text2 <- paste("The list of selected biomarker(s) are ready for interactive ROC and AUC in next module!", sep ="")
      candidate_markers <- as.character(unlist(test_summary3["Biomarker"]))
      summary_table <- test_summary3
    }
    else{
      summary_text1 <- paste("There are ", nrow(OTU_input), " biomarker(s) in your data set, and 0 biomarker's FDR (q value) less than ", cut, sep ="")
      summary_text2 <- paste("Please re-run this procedure using more conservative threshold!", sep ="")
      candidate_markers <- "No biomarker available, please run one biomarker discovery method in step 2 under Statistical Analysis Module"
      summary_table <- c("")
    }
  }
  return(list(summary_text1 = summary_text1, summary_text2 = summary_text2, candidate_markers = candidate_markers, summary_table = summary_table))
}

