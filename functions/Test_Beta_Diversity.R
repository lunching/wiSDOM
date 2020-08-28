Test_beta_diversity <- function(Beta_diversity_input, group_index){
  group_tmp <- table(group_index)
  Beta_div <- c()
  group <- c()
  for(i in 1: length(group_tmp)){
    index_group_tmp <- which(group_index == names(group_tmp)[i])
    Beta_div_tmp <- as.matrix(Beta_diversity_input)[index_group_tmp, index_group_tmp]
    Beta_div_tmp2 <- Beta_div_tmp[lower.tri(Beta_div_tmp)]
    Beta_div <- c(Beta_div, Beta_div_tmp2)
    group <- c(group, rep(names(group_tmp)[i], length(Beta_div_tmp2)))
  }
  if(length(names(table(group_index))) == 2){
    p_2g_tmp <- t.test(as.numeric(Beta_div[which(group == names(table(group_index))[1])]),  as.numeric(Beta_div[which(group == names(table(group_index))[2])]))$p.value
    return(list(signif(p_2g_tmp, 4), "NA" ))
  }
  if(length(names(table(group_index))) > 2){
    #p_KW_tmp <- signif(kruskal.test(as.numeric(Beta_div), as.factor(group))$p.value, 4)
    p_KW_tmp <- signif(summary(aov(as.numeric(Beta_div)~as.factor(group)))[[1]][["Pr(>F)"]][1], 4)
    names(p_KW_tmp) <- "P_value_KW"
    p_matrix_tmp <- pairwise.t.test(as.numeric(Beta_div), group, p.adjust.method = "none")$p.value
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


