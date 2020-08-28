
Test_alpha_diversity <- function(OTU_input, group_index, index_method = "1"){
  #index_method: chao1 or Shannon
  if(index_method == "1"){
    alpha_index <- sapply(1: ncol(OTU_input), function(k) chao1(OTU_input[,k]))
  }
  if(index_method == "2"){
    alpha_index <- diversity(t(OTU_input))
  }
  if(length(names(table(group_index))) == 2){
    p_2g_tmp <- t.test(as.numeric(alpha_index[which(group_index == names(table(group_index))[1])]),  as.numeric(alpha_index[which(group_index == names(table(group_index))[2])]))$p.value
    return(list(signif(p_2g_tmp, 4), "NA") )
  }
  if(length(names(table(group_index))) > 2){
    #p_KW_tmp <- signif(kruskal.test(as.numeric(alpha_index), as.factor(group_index))$p.value, 4)
    p_KW_tmp <- signif(summary(aov(as.numeric(alpha_index)~as.factor(group_index)))[[1]][["Pr(>F)"]][1], 4)
    names(p_KW_tmp) <- "P_value_KW"
    p_matrix_tmp <- pairwise.t.test(as.numeric(alpha_index), group_index, p.adjust.method = "none")$p.value
    pairwise_p_tmp <- c()
    for(j in 1: nrow(p_matrix_tmp)){
      for(k in 1: ncol(p_matrix_tmp)){
        if(j >= k){
          p_tmp <- signif(p_matrix_tmp[j,k], 4)
          names(p_tmp) <- paste("P",rownames(p_matrix_tmp)[j], "vs", colnames(p_matrix_tmp)[k], sep = "_")
          pairwise_p_tmp <- c(pairwise_p_tmp, p_tmp)
        }
      }
    }
    return(list(p_KW_tmp, pairwise_p_tmp))
  }
}
