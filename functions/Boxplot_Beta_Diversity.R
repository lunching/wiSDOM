Boxplot_beta_diversity <- function(Beta_diversity_input, group_index){
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
  ## data structure for ggplot ##
  data_gg = data.frame(group, Beta_div)
  ggplot(data_gg, aes(x = group, y = Beta_div, fill = group)) + geom_boxplot() +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=20))
}