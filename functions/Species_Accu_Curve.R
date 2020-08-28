Species_Accu_Curve <- function(OTU_input, n_rep = 5, n_sample = 20){
  matrix_obs_tmp <- matrix(0, nrow = n_rep, ncol = n_sample)
  for(i in 1: n_sample){
    for(j in 1: n_rep){
      index_sample_tmp <- sample(1: ncol(OTU_input), i)
      matrix_tmp <- as.matrix(OTU_input[ , index_sample_tmp])
      matrix_obs_tmp[j,i] <- length(which(apply(matrix_tmp, 1, sum, na.rm = T)!=0))
    }
  }
  boxplot(matrix_obs_tmp, xlab = "Number of samples", ylab = "Observed species", main = "Species Accumulation Curve", cex.lab = 1.4,cex.axis = 1.3, cex.main = 2)
}