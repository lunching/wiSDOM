library("ggplot2")
library("phyloseq")
library("ape")
library("plotly")

Calculate_Beta_div <- function(otumat_2g, div_type){
  taxmat_2g = matrix(NA, nrow = nrow(otumat_2g), ncol = 7)
  rownames(taxmat_2g) <-  rownames(otumat_2g)
  colnames(taxmat_2g) <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  text_all_2g <- rownames(otumat_2g)
  for(i in 1: nrow (taxmat_2g)){
    text_tmp <- unlist(strsplit(text_all_2g[i], ";"))
    text_tmp <- gsub("D_0__", "d_", text_tmp)
    text_tmp <- gsub("D_1__", "p_", text_tmp)
    text_tmp <- gsub("D_2__", "c_", text_tmp)
    text_tmp <- gsub("D_3__", "o_", text_tmp)
    text_tmp <- gsub("D_4__", "f_", text_tmp)
    text_tmp <- gsub("D_5__", "g_", text_tmp)
    text_tmp <- gsub("D_6__", "s_", text_tmp)
    taxmat_2g[i, 1: length(text_tmp)] <- text_tmp
    OTU_2g = otu_table(otumat_2g, taxa_are_rows = TRUE)
    TAX_2g = tax_table(taxmat_2g)
    physeq_2g = phyloseq(OTU_2g, TAX_2g)
    random_tree_2g = rtree(ntaxa(physeq_2g), rooted=TRUE, tip.label=taxa_names(physeq_2g))
    physeq2_2g = phyloseq(OTU_2g, TAX_2g, random_tree_2g)
    if(div_type == "1"){
      Beta_diver_unweighted_2g <- UniFrac(physeq2_2g)
      text_tmp <- "Beta diversity on unweighted UniFrac is calculated"
      return(list(beta_div = Beta_diver_unweighted_2g, log_text = text_tmp))
    }
    if(div_type == "2"){
      Beta_diver_weighted_2g <- UniFrac(physeq2_2g, weighted = T)
      text_tmp <- "Beta diversity on weighted UniFrac is calculated"
      return(list(beta_div = Beta_diver_weighted_2g, log_text = text_tmp))
    }
  }
}