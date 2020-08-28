PCoA_biplot <- function(beta_div_input, group_index){
  PCA_unweighted_2g <- prcomp(beta_div_input)
  data_qq_PCA_unweighted_2g <- data.frame(PCA_unweighted_2g$rotation[,1:2])
  ggplot(data_qq_PCA_unweighted_2g, aes(x = PC1, y = PC2, color = factor(group_index))) + 
    geom_point(size = 4) +
    stat_ellipse() +
    labs(color = "Group") +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=20))
}
