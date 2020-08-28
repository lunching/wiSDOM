library("vegan")

Test_ANOSIM <- function(beta_div_input, group_index, n_perm = 1000, seed = 9527){
  set.seed(seed)
  dune.ano1 <- anosim(beta_div_input, factor(group_index), permutations = n_perm)
  plot(dune.ano1, cex.lab = 1.4,cex.axis = 1.3, xlab = "Class Vector", ylab = "Dissimlarity Rank")
}
