library("fossil")
library("vegan")
library("plotly")

Boxplot_alpha_diversity <- function(OTU_input, group_index, index_method = "1"){
  #index_method: chao1 or Shannon
  if(index_method == "1"){
    alpha_index <- sapply(1: ncol(OTU_input), function(k) chao1(OTU_input[,k]))
  }
  if(index_method == "2"){
    alpha_index <- diversity(t(OTU_input))
  }
  if(index_method == "3"){
    alpha_index <- diversity(t(OTU_input), index = "simpson")
  }
  if(index_method == "4"){
    alpha_index <- diversity(t(OTU_input), index = "invsimpson")
  }
  ## data structure for ggplot ##
  group <- group_index
  data_gg = data.frame(group, alpha_index)
  ggplot(data_gg, aes(x = group, y = alpha_index, fill = group)) + geom_boxplot() + labs(y = index_method) +
    labs(y = "Alpha Div") +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=20))
  #ggplotly(p)
}