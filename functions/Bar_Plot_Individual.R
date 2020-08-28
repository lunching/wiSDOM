library("RColorBrewer")

Bar_Plot_Individual_OTU <- function(OTU_input, group_index, n_top = 10, names="OTU"){
  if(n_top > nrow(OTU_input)){
    print("Number of selected top OTU number is greater than the total OTU numbers")
  }
  else{
    if(ncol(OTU_input)!=length(group_index)){
      print("The length of the group index does not match the number of columns in OTU table")
    }
    else{
      Overall_mean <- apply(OTU_input, 1, mean, na.rm = T)
      Top_index <- order(Overall_mean, decreasing = T)[1: n_top]
      Top_OTU_names <- c(rownames(OTU_input)[Top_index], "OTHERS")
      group_tmp <- names(table(group_index))
      sample <- c()
      OTU <- c()
      Abundance <- c()
      for(i in 1: length(group_tmp)){
        matrix_tmp <- OTU_input[ , which(group_index == group_tmp[i])]
        for(j in 1: ncol(matrix_tmp)){
          top_OTU_abd_tmp <- matrix_tmp[Top_index, j] 
          #others_OTU_abd_tmp <- mean(matrix_tmp[setdiff(c(1: nrow(OTU_input)), Top_index), j], na.rm = T)
          others_OTU_abd_tmp <- 1 - sum(top_OTU_abd_tmp)
          if(others_OTU_abd_tmp < 0){others_OTU_abd_tmp <- 0}
          sample <- c(sample, rep(colnames(matrix_tmp)[j], n_top + 1))
          OTU <- c(OTU, Top_OTU_names)
          Abundance <- c(Abundance, c(top_OTU_abd_tmp, others_OTU_abd_tmp))
        }
        #top_means <- apply(matrix_tmp[Top_index, ], 1, mean, na.rm = T)
        #other_mean <- mean(unlist(matrix_tmp[setdiff(c(1: nrow(OTU_input)), Top_index), ]))
        #group <- c(group, rep(group_tmp[i], n_top + 1))
        #OTU <- c(OTU, Top_OTU_names)
        #Abundance <- c(Abundance, c(top_means, other_mean))
      }
      ## data structure for ggplot ##
      colourCount = length(unique(mtcars$hp))
      getPalette = colorRampPalette(brewer.pal(9, "Set1"))
      data_gg = data.frame(sample, OTU, Abundance)
      level_order <- unique(as.character(data_gg$sample))
      level_OTU <- Top_OTU_names[length(Top_OTU_names):1]
      ggplot(data_gg, aes(fill = factor(OTU, level = level_OTU), y = Abundance, x = factor(sample, level = level_order))) + 
        geom_bar( stat="identity", position="fill") +
        theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
        labs(y = "Relative Abundance(%)", fill = names) +
        scale_fill_manual(values = getPalette(colourCount)) +
        theme(axis.text=element_text(size=15), axis.title=element_text(size=20), legend.title = element_text(size=15), legend.text = element_text(size=10))
        #scale_fill_brewer(palette="Spectral")
    }
  }
}