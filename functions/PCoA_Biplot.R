PCoA_biplot <- function(beta_div_input, group_index){
  PCoA_unweighted_2g <- data.frame(cmdscale(beta_div_input))
  ggplot(PCoA_unweighted_2g, aes(x = X1, y = X2, color = factor(group_index))) + 
    labs(x = "MDS1", y = "MDS2") +
    geom_point(size = 4) +
    stat_ellipse() +
    labs(color = "Group") +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=20))
}





