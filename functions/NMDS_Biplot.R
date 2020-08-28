NMDS_biplot <- function(beta_div_input, group_index){
  NMDS_unweighted_2g <-  metaMDS(beta_div_input)
  stress_text <- paste("Stress = ", signif(NMDS_unweighted_2g$stress, 4), sep = "")
  NMDS_unweighted_scores_2g <- as.data.frame(scores(NMDS_unweighted_2g))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
  ggplot(NMDS_unweighted_scores_2g, aes(x = NMDS1, y = NMDS2, color = factor(group_index))) + 
    geom_point(size = 4) +
    stat_ellipse() +
    labs(color = "Group", size = 6) +
    geom_text(data = NMDS_unweighted_scores_2g,aes(x=max(NMDS1),y=max(NMDS2),label = stress_text), size = 6) +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=20))
}

