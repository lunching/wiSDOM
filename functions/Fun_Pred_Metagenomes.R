library("themetagenomics")

Fun_Pred_Metagenomes <- function(OTU_input_dir, group_index_dir, ref_database, ref_path, OTU_ID_dir){
  data_tmp<- read.delim(OTU_input_dir, header = T, sep = "\t")
  OTU_input <- data_tmp[,2: ncol(data_tmp)]
  rownames(OTU_input) <- data_tmp[,1]
  group_index <- read.delim(group_index_dir, header = F, sep = "\t")
  group_index <- group_index[,2]
  if(ref_database == "1"){ ## green gene
    OTU_ID <- read.delim(OTU_ID_dir, header = F, sep = "\t")
    OTU_ID_tmp <- unlist(OTU_ID)
    taxa_input_tmp <- matrix(NA, nrow = nrow(OTU_input), ncol = 7)
    colnames(taxa_input_tmp) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
    text_all_2g <- rownames(OTU_input)
    for(i in 1: nrow (taxa_input_tmp)){
      text_tmp <- unlist(strsplit(text_all_2g[i], ";"))
      text_tmp <- gsub("D_0__", "k__", text_tmp)
      text_tmp <- gsub("D_1__", "p__", text_tmp)
      text_tmp <- gsub("D_2__", "c__", text_tmp)
      text_tmp <- gsub("D_3__", "o__", text_tmp)
      text_tmp <- gsub("D_4__", "f__", text_tmp)
      text_tmp <- gsub("D_5__", "g__", text_tmp)
      text_tmp <- gsub("D_6__", "s__", text_tmp)
      taxa_input_tmp[i, ] <- text_tmp
    }
    rownames(taxa_input_tmp) <- OTU_ID_tmp
    ## Abundance ##
    abundance_tmp <- t(OTU_input)
    colnames(abundance_tmp) <- OTU_ID_tmp
    ## Meta
    Meta_tmp <- cbind(colnames(OTU_input), group_index)
    rownames(Meta_tmp) <- colnames(OTU_input)
    colnames(Meta_tmp) <- c("SampleID", "DIAGNOSIS")
    ##
    Fun_Pred_input <- list(OTU = abundance_tmp, TAX = taxa_input_tmp, META = Meta_tmp)
    #tmp <- getwd()
    download_ref(ref_path,reference='gg_ko',overwrite = FALSE)
    FUNCTIONS <- picrust(Fun_Pred_input$OTU,rows_are_taxa=FALSE,
                         reference='gg_ko',reference_path=ref_path,
                         cn_normalize=TRUE,sample_normalize=FALSE,
                         drop=TRUE)
    #colsum <- apply(FUNCTIONS$fxn_table, 1, sum)
    #data_output_prop <- FUNCTIONS$fxn_table/colsum
    data_output <- t(FUNCTIONS$fxn_table)
    PW_level123_summary <- c()
    for(i in 1: length(FUNCTIONS$fxn_meta$KEGG_Pathways)){
      list_tmp <- FUNCTIONS$fxn_meta$KEGG_Pathways[[i]]
      tmp_ko <- matrix(NA ,nrow = length(list_tmp), ncol = 4)
      tmp_ko[,1] <- names(FUNCTIONS$fxn_meta$KEGG_Pathways)[i]
      for(j in 1:length(list_tmp)){
        tmp_ko[j,2] <- list_tmp[[j]][1]
        tmp_ko[j,3] <- list_tmp[[j]][2]
        tmp_ko[j,4] <- list_tmp[[j]][3]
      }
      PW_level123_summary <- rbind(PW_level123_summary, tmp_ko)
    }
    ## L1 ##
    PW_list_L1 <- unique(PW_level123_summary[,2])
    
    summary_l1_t4f <- matrix(0, nrow = length(PW_list_L1), ncol = ncol(data_output))
    colnames(summary_l1_t4f) <- colnames(data_output)
    rownames(summary_l1_t4f) <- PW_list_L1
    for(i in 1: length(PW_list_L1)){
      pw_tmp <- PW_list_L1[i]
      ko_tmp <- unique(PW_level123_summary[which(PW_level123_summary[,2] == pw_tmp), 1])
      index_ko_tmp <- match(ko_tmp, rownames(data_output))
      if(length(index_ko_tmp) == 1){
        sum_ko_tmp <- as.numeric(data_output[index_ko_tmp, ])
      }
      else{
        sum_ko_tmp <- as.numeric(apply(data_output[index_ko_tmp, ], 2, sum))
      }
      summary_l1_t4f[i, ] <- as.numeric(sum_ko_tmp)
    }
    
    colsum <- apply(t(summary_l1_t4f), 1, sum)
    summary_l1_t4f_prop <- t(t(summary_l1_t4f)/colsum)
    summary_l1_t4f2 <- cbind(rownames(summary_l1_t4f), summary_l1_t4f_prop)
    colnames(summary_l1_t4f2)[1] <- "level_1"
    
    ## L2 ##
    PW_list_L2 <- unique(PW_level123_summary[,3])
    
    summary_l2_t4f <- matrix(0, nrow = length(PW_list_L2), ncol = ncol(data_output))
    colnames(summary_l2_t4f) <- colnames(data_output)
    rownames(summary_l2_t4f) <- PW_list_L2
    for(i in 1: length(PW_list_L2)){
      pw_tmp <- PW_list_L2[i]
      ko_tmp <- unique(PW_level123_summary[which(PW_level123_summary[,3] == pw_tmp), 1])
      index_ko_tmp <- match(ko_tmp, rownames(data_output))
      if(length(index_ko_tmp) == 1){
        sum_ko_tmp <- as.numeric(data_output[index_ko_tmp, ])
      }
      else{
        sum_ko_tmp <- as.numeric(apply(data_output[index_ko_tmp, ], 2, sum))
      }
      summary_l2_t4f[i, ] <- as.numeric(sum_ko_tmp)
    }
    colsum <- apply(t(summary_l2_t4f), 1, sum)
    summary_l2_t4f_prop <- t(t(summary_l2_t4f)/colsum)
    summary_l2_t4f2 <- cbind(rownames(summary_l2_t4f), summary_l2_t4f_prop)
    colnames(summary_l2_t4f2)[1] <- "level_2"
    
    ## L3 ##
    PW_list_L3 <- unique(PW_level123_summary[,4])
    
    summary_l3_t4f <- matrix(0, nrow = length(PW_list_L3), ncol = ncol(data_output))
    colnames(summary_l3_t4f) <- colnames(data_output)
    rownames(summary_l3_t4f) <- PW_list_L3
    for(i in 1: length(PW_list_L3)){
      pw_tmp <- PW_list_L3[i]
      ko_tmp <- unique(PW_level123_summary[which(PW_level123_summary[,4] == pw_tmp), 1])
      index_ko_tmp <- match(ko_tmp, rownames(data_output))
      if(length(index_ko_tmp) == 1){
        sum_ko_tmp <- as.numeric(data_output[index_ko_tmp, ])
      }
      else{
        sum_ko_tmp <- as.numeric(apply(data_output[index_ko_tmp, ], 2, sum))
      }
      summary_l3_t4f[i, ] <- as.numeric(sum_ko_tmp)
    }
    colsum <- apply(t(summary_l3_t4f), 1, sum)
    summary_l3_t4f_prop <- t(t(summary_l3_t4f)/colsum)
    summary_l3_t4f2 <- cbind(rownames(summary_l3_t4f), summary_l3_t4f_prop)
    colnames(summary_l3_t4f2)[1] <- "level_3"
    #######
    #data_output2 <- cbind(rownames(data_output ), data_output)
    #colnames(data_output2)[1] <- "KO"
    summary_text1 <- paste("Green Gene reference was used, and the proportion of reads that mapped to pathways in different levels during OTU picking were successfully generated.", sep = "")
    summary_text2 <- paste("Now you can export the output for different levels and can be use as input for module #4: Statistical Analysis!")
    summary_text3 <- paste("There are ", nrow(summary_l1_t4f2), " pathways in level 1", sep = "")
    summary_text4 <- paste("There are ", nrow(summary_l2_t4f2), " pathways in level 2", sep = "")
    summary_text5 <- paste("There are ", nrow(summary_l3_t4f2), " pathways in level 3", sep = "")
    return(list(summary_text1 = summary_text1, summary_text2 = summary_text2, summary_text3 = summary_text3, summary_text4 = summary_text4, summary_text5 = summary_text5, summary_l1_t4f2 = summary_l1_t4f2, summary_l2_t4f2 = summary_l2_t4f2, summary_l3_t4f2 = summary_l3_t4f2))
  } 
  if(ref_database == "2"){ ## silva
    taxa_input_tmp <- matrix(NA, nrow = nrow(OTU_input), ncol = 7)
    colnames(taxa_input_tmp) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
    text_all_2g <- rownames(OTU_input)
    for(i in 1: nrow (taxa_input_tmp)){
      text_tmp <- unlist(strsplit(text_all_2g[i], ";"))
      text_tmp <- gsub("D_0__", "", text_tmp)
      text_tmp <- gsub("D_1__", "", text_tmp)
      text_tmp <- gsub("D_2__", "", text_tmp)
      text_tmp <- gsub("D_3__", "", text_tmp)
      text_tmp <- gsub("D_4__", "", text_tmp)
      text_tmp <- gsub("D_5__", "", text_tmp)
      text_tmp <- gsub("D_6__", "", text_tmp)
      taxa_input_tmp[i, ] <- text_tmp
    }
    rownames(taxa_input_tmp) <- 1: nrow(taxa_input_tmp)
    ## Abundance ##
    abundance_tmp <- t(OTU_input)
    colnames(abundance_tmp) <- 1: ncol(abundance_tmp)
    ##
    Fun_Pred_input <- list(ABUND = abundance_tmp, TAX = taxa_input_tmp)
    #tmp <- getwd()
    download_ref(ref_path,reference='silva_ko',overwrite=FALSE)
    FUNCTIONS <- t4f(Fun_Pred_input$ABUND,rows_are_taxa=FALSE,tax_table=Fun_Pred_input$TAX,
                     reference_path=ref_path,type='uproc',short=TRUE,
                     cn_normalize=TRUE,sample_normalize=TRUE,drop=TRUE)
    data_output <- t(FUNCTIONS$fxn_table)
    PW_level123_summary <- c()
    for(i in 1: length(FUNCTIONS$fxn_meta$KEGG_Pathways)){
      list_tmp <- FUNCTIONS$fxn_meta$KEGG_Pathways[[i]]
      tmp_ko <- matrix(NA ,nrow = length(list_tmp), ncol = 4)
      tmp_ko[,1] <- names(FUNCTIONS$fxn_meta$KEGG_Pathways)[i]
      for(j in 1:length(list_tmp)){
        tmp_ko[j,2] <- list_tmp[[j]][1]
        tmp_ko[j,3] <- list_tmp[[j]][2]
        tmp_ko[j,4] <- list_tmp[[j]][3]
      }
      PW_level123_summary <- rbind(PW_level123_summary, tmp_ko)
    }
    ## L1 ##
    PW_list_L1 <- unique(PW_level123_summary[,2])
    
    summary_l1_t4f <- matrix(0, nrow = length(PW_list_L1), ncol = ncol(data_output))
    colnames(summary_l1_t4f) <- colnames(data_output)
    rownames(summary_l1_t4f) <- PW_list_L1
    for(i in 1: length(PW_list_L1)){
      pw_tmp <- PW_list_L1[i]
      ko_tmp <- unique(PW_level123_summary[which(PW_level123_summary[,2] == pw_tmp), 1])
      index_ko_tmp <- match(ko_tmp, rownames(data_output))
      if(length(index_ko_tmp) == 1){
        sum_ko_tmp <- data_output[index_ko_tmp, ]
      }
      else{
        sum_ko_tmp <- apply(data_output[index_ko_tmp, ], 2, sum)
      }
      summary_l1_t4f[i, ] <- sum_ko_tmp
    }
    
    summary_l1_t4f2 <- cbind(rownames(summary_l1_t4f), summary_l1_t4f)
    colnames(summary_l1_t4f2)[1] <- "level_1"
    
    ## L2 ##
    PW_list_L2 <- unique(PW_level123_summary[,3])
    
    summary_l2_t4f <- matrix(0, nrow = length(PW_list_L2), ncol = ncol(data_output))
    colnames(summary_l2_t4f) <- colnames(data_output)
    rownames(summary_l2_t4f) <- PW_list_L2
    for(i in 1: length(PW_list_L2)){
      pw_tmp <- PW_list_L2[i]
      ko_tmp <- unique(PW_level123_summary[which(PW_level123_summary[,3] == pw_tmp), 1])
      index_ko_tmp <- match(ko_tmp, rownames(data_output))
      if(length(index_ko_tmp) == 1){
        sum_ko_tmp <- data_output[index_ko_tmp, ]
      }
      else{
        sum_ko_tmp <- apply(data_output[index_ko_tmp, ], 2, sum)
      }
      summary_l2_t4f[i, ] <- sum_ko_tmp
    }
    
    summary_l2_t4f2 <- cbind(rownames(summary_l2_t4f), summary_l2_t4f)
    colnames(summary_l2_t4f2)[1] <- "level_2"
    
    ## L3 ##
    PW_list_L3 <- unique(PW_level123_summary[,4])
    
    summary_l3_t4f <- matrix(0, nrow = length(PW_list_L3), ncol = ncol(data_output))
    colnames(summary_l3_t4f) <- colnames(data_output)
    rownames(summary_l3_t4f) <- PW_list_L3
    for(i in 1: length(PW_list_L3)){
      pw_tmp <- PW_list_L3[i]
      ko_tmp <- unique(PW_level123_summary[which(PW_level123_summary[,4] == pw_tmp), 1])
      index_ko_tmp <- match(ko_tmp, rownames(data_output))
      if(length(index_ko_tmp) == 1){
        sum_ko_tmp <- data_output[index_ko_tmp, ]
      }
      else{
        sum_ko_tmp <- apply(data_output[index_ko_tmp, ], 2, sum)
      }
      summary_l3_t4f[i, ] <- sum_ko_tmp
    }
    
    summary_l3_t4f2 <- cbind(rownames(summary_l3_t4f), summary_l3_t4f)
    colnames(summary_l3_t4f2)[1] <- "level_3"
    ####
    #data_output2 <- cbind(rownames(data_output ), data_output)
    #colnames(data_output2)[1] <- "KO"
    summary_text1 <- paste("Silva reference was used, and the proportion of reads that mapped to pathways in different levels during OTU picking were successfully generated.", sep = "")
    summary_text2 <- paste("Now you can export the output for different levels and can be use as input for module #4: Statistical Analysis!")
    summary_text3 <- paste("There are ", nrow(summary_l1_t4f2), " pathways in level 1", sep = "")
    summary_text4 <- paste("There are ", nrow(summary_l2_t4f2), " pathways in level 2", sep = "")
    summary_text5 <- paste("There are ", nrow(summary_l3_t4f2), " pathways in level 3", sep = "")
    return(list(summary_text1 = summary_text1, summary_text2 = summary_text2, summary_text3 = summary_text3, summary_text4 = summary_text4, summary_text5 = summary_text5, summary_l1_t4f2 = summary_l1_t4f2, summary_l2_t4f2 = summary_l2_t4f2, summary_l3_t4f2 = summary_l3_t4f2))
  }
  
}

