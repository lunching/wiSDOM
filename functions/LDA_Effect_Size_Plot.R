library("ggplot2")

LDA_effect_size_plot <- function(LDA_output){
  groupname_tmp <- gsub("Mean_", "",colnames(LDA_output)[grep( "Mean",colnames(LDA_output))])
  LDA_plot <- data.frame(OTU = LDA_output[,"Biomarker"], group = groupname_tmp[sapply(1: nrow(LDA_output), function(k) which.max(as.numeric(LDA_output[k,grep( "Mean",colnames(LDA_output))])))], LDA = as.numeric(LDA_output[,"log_LDA_scores"]))
  
  LDA_plot_sorted <- c()
  for(i in 1: length(groupname_tmp)){
    index_tmp <- which(LDA_plot[,"group"] == groupname_tmp[i])
    LDA_tmp <- LDA_plot[index_tmp,]
    if(length(groupname_tmp)==2&i == 1){
      LDA_tmp[,"LDA"] <- -LDA_tmp[,"LDA"]
    }
    LDA_tmp_sort <- LDA_tmp[order(LDA_tmp[,"LDA"], decreasing = F), ]
    LDA_plot_sorted <- rbind(LDA_plot_sorted, LDA_tmp_sort)
  }
  ggplot(LDA_plot_sorted, aes(x=OTU, y=LDA, fill=group)) +
    geom_bar(stat="identity")+theme_minimal() +
    coord_flip() + 
    scale_x_discrete(limits=LDA_plot_sorted[,"OTU"]) +
    theme(axis.text=element_text(size=15), legend.title = element_text(size=20), legend.text = element_text(size=20))
}



