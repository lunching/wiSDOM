data_input_pre_proc <- function(Input, Index, NA_cut, zero_cut){
  data_tmp <- read.delim(Input, header = T, sep = "\t")
  data_index <- read.delim(Index, header = F, sep = "\t")
  NA_cut_tmp <- NA_cut/100
  zero_cut_tmp <- zero_cut/100
  data_OTU <- data_tmp[,2: ncol(data_tmp)]
  marker_name <- data_tmp[, 1]
  text_OTU_summary <- paste("There are ", nrow(data_OTU), " Biomarker(s) before filtering step.", sep="")
  #text_subject_summary <- paste("Number of ", names(table(data_index[,2])), ": ", table(data_index[,2]), collapse = ", ", sep ="" )
  index_tmp <- names(table(data_index[,2]))
  matrix_tmp <- matrix(0, nrow(data_OTU), ncol = length(index_tmp))
  for(i in 1: length(index_tmp)){
    index_tmp2 <- which(data_index[,2] == index_tmp[i])
    matrix_tmp[,i] <- sapply(1: nrow(data_OTU), function(k) sum(is.na(data_OTU[k, index_tmp2])))/length(index_tmp2)
  }
  NA_check <- sapply(1: nrow(matrix_tmp), function(k) any(matrix_tmp[k,] > NA_cut_tmp) ) 
  index_NA <- which(NA_check == TRUE)
  if(length(index_NA) > 0){
    data_OTU2 <- data_OTU[-index_NA, ]
    marker_name2 <- marker_name[-index_NA]
  }
  else{
    data_OTU2 <- data_OTU
    marker_name2 <- marker_name
  }
  text_NA_summary <- paste("There are ", length(index_NA), " Biomarker(s) wiht missing rate greater than ", NA_cut_tmp, " in any one of ", length(index_tmp), " groups", sep="")
  
  matrix_zero_tmp <- matrix(0, nrow(data_OTU2), ncol = length(index_tmp))
  for(i in 1: length(index_tmp)){
    index_tmp2 <- which(data_index[,2] == index_tmp[i])
    matrix_zero_tmp[,i] <- sapply(1: nrow(data_OTU2), function(k) length(which(data_OTU2[k, index_tmp2] ==0)))/length(index_tmp2)
  }
  zero_check <- sapply(1: nrow(matrix_zero_tmp), function(k) any(matrix_zero_tmp[k,] > zero_cut_tmp) ) 
  index_zero <- which(zero_check == TRUE)
  if(length(index_zero) > 0){
    data_OTU3 <- data_OTU2[-index_zero, ]
    marker_name3 <- marker_name2[-index_zero]
  }
  else{
    data_OTU3 <- data_OTU2
    marker_name3 <- marker_name2
  }
  number_samples <- nrow(data_index)
  
  text_zero_summary <- paste("There are ", length(index_zero), " Biomarker(s) wiht number of 0 rate greater than ", zero_cut_tmp, " in any one of ", length(index_tmp), " groups", sep="")
  if(nrow(data_OTU3) == 0){
    text_sd_summary <- paste("There are no Biomarker(s) for checking the standard deviation, please make more conservative filtering parameters", sep="")
    text_n_biomarker_summary <- paste("After filtering step, there are ", nrow(data_OTU), " remains, please make more conservative filtering parameters.", sep ="")
  }
  else{
    matrix_sd_tmp <- matrix(0, nrow(data_OTU3), ncol = length(index_tmp))
    for(i in 1: length(index_tmp)){
      index_tmp2 <- which(data_index[,2] == index_tmp[i])
      matrix_sd_tmp[,i] <- sapply(1: nrow(data_OTU3), function(k) sd(data_OTU3[k, index_tmp2]))
    }
    sd_check <- sapply(1: nrow(matrix_sd_tmp), function(k) any(matrix_sd_tmp[k,] ==0) ) 
    index_sd <- which(sd_check == TRUE)
    if(length(index_sd) > 0){
      data_OTU4 <- data_OTU3[-index_sd, ]
      marker_name4 <- marker_name3[-index_sd]
    }
    else{
      data_OTU4 <- data_OTU3
      marker_name4 <- marker_name3
    }
    text_sd_summary <- paste("There are ", length(index_sd), " Biomarker(s) wiht standard deviation equals to 0 in any one of ", length(index_tmp), " groups", sep="")
    text_n_biomarker_summary <- paste("After filtering step, there are ", nrow(data_OTU4), " remains, filtered data was saved for biomarker discivery in step 2.", sep ="")
    rownames(data_OTU4) <- marker_name4
    return(list(text_OTU_summary = text_OTU_summary, text_NA_summary = text_NA_summary, text_zero_summary = text_zero_summary,text_sd_summary = text_sd_summary, text_n_biomarker_summary = text_n_biomarker_summary, number_of_samples = number_samples, marker_name = marker_name4, data_OTU = data_OTU4, Data_Index = data_index[,2]))
  }

}
