## Check packages ##
if (!require('DT')) install.packages("DT")
if (!require('shiny')) install.packages("shiny") 
if (!require('shinythemes')) install.packages("shinythemes") 
if (!require('shinyFiles')) install.packages("shinyFiles") 

if (!require('httpuv')){
  install.packages("httpuv")
}

## Alpha diversity ##

if (!require('fossil')){
  install.packages("fossil")
}


if (!require('vegan')){
  install.packages("vegan") ## ANOSIM
}

if (!require('plotly')){
  install.packages("plotly")
}

## Setting more color ##

if (!require('RColorBrewer')){
  install.packages("RColorBrewer")
}

## Beta diversity ##

if (!require('ggplot2')){
  install.packages("ggplot2")
}

if (!require('phyloseq')){
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  BiocManager::install("phyloseq")
}

if (!require('ape')){
  install.packages("ape")
}

if (!require('plotly')){
  install.packages("plotly")
}

## Biomarker detection

if (!require('dunn.test')){
  install.packages("dunn.test")
}

## ROC and AUC

if (!require('pROC')){
  install.packages("pROC")
}

if (!require('verification')){
  install.packages("verification")
}

## LDA

if (!require('MASS')){
  install.packages("MASS")
}

if (!require('ROCR')){
  install.packages("ROCR")
}

if (!require('ggplot2')){
  install.packages("ggplot2")
}

## Random Forest ##

if (!require('rsample')){
  install.packages("rsample")
}

if (!require('randomForest')){
  install.packages("randomForest")
}

if (!require('ranger')){
  install.packages("ranger")
}

if (!require('caret')){
  install.packages("caret")
}

if (!require('mlbench')){
  install.packages("mlbench")
}

if (!require('e1071')){
  install.packages("e1071")
}

## Functional Prediction of Metagenomes ##

if (!require('themetagenomics')){
  install.packages("themetagenomics")
}

## R functions ##
#source("functions/data_preproc.R")
#source("functions/HCMM_CNVs.R")
#source("functions/plot_HCMMCNVs.R")
#source("functions/Mixture_Model.R")
source("functions/Read_input.R")
source("functions/Read_input_RA.R")
source("functions/Data_Input_Preprocessing.R")
