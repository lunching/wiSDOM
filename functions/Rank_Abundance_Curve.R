Rank_abundance_curve <- function(OTU_input, group_index, by = "group"){
  # by: group or sample
  Overall_mean <- apply(OTU_input, 1, mean, na.rm = T)
  Top_index <- order(Overall_mean, decreasing = T)
  OTU_sort <- OTU_input[Top_index, ]
  group_tmp <- table(group_index)
  if(by == "group"){
    RA_rank <- matrix(0, nrow = length(group_tmp), ncol = length(Top_index))
    for(i in 1: length(group_tmp)){
      index_group_tmp <- which(group_index == names(group_tmp)[i])
      OTU_sort_g_tmp <- OTU_sort[ , index_group_tmp]
      withProgress(message = 'Progress:', value = 0, { ##pb
        n <- ncol(RA_rank) ##pb
      for(j in 1: ncol(RA_rank)){
        RA_rank[i, j] <- mean(as.numeric(unlist(OTU_sort_g_tmp[1:j, ])), na.rm = T)/(sum(OTU_sort_g_tmp)/length(index_group_tmp))
        incProgress(1/n, detail = paste("Group", i, sep = " ")) ## pb
      }
      }) ##pb
    }
    ## Plot ##
    for(i in 1: length(group_tmp)){
      plot(1: ncol(RA_rank), RA_rank[i, ], type = "s", col = i, xlab = "Species Rank", ylab = "Relative Abundance(%)", main = "Rank Abundance Curve (by group)", cex.lab = 1.4,cex.axis = 1.3, cex.main = 2, xlim = c(0, ncol(RA_rank)), ylim = c(0, max(RA_rank)))
      par(new = T)
    }
    legend("topright", names(group_tmp), pch = 16, col = 1: length(group_tmp), cex = 1.5)
  }
}

