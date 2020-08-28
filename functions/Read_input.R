data_input_OTU <- function(Input, Index){
    data_tmp <- read.delim(Input, header = T, sep = "\t")
    data_index <- read.delim(Index, header = F, sep = "\t")
    data_OTU <- data_tmp[,2: ncol(data_tmp)]
    text_OTU_summary<- paste("There are ", nrow(data_tmp), " OTUs.", sep="")
    text_subject_summary <- paste("Number of ", names(table(data_index[,2])), ": ", table(data_index[,2]), collapse = ", ", sep ="" )
    number_samples <- nrow(data_index)
    return(list(text_OTU_summary = text_OTU_summary, Subject_summary = text_subject_summary, Number_of_samples = number_samples, Data_OTU = data_OTU, Data_Index = data_index[,2]))
}