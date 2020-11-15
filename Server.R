source("global.R")

# Setting the maximum file upload limit to 1 GB
options(shiny.maxRequestSize=1000*1024^2)

server = function(input, output, session){
  ################
  ##  Module 1  ##
  ################
  ## Data ##
  ## OTUs ##
  dataInput_bar1<- eventReactive(input$action_OTU,{
    inFile1 <- input$input_OTU
    inFile2 <- input$input_Index
    if(is.null(input$input_OTU)|is.null(input$input_Index)){
      return(list("Input is missing", "Input is missing", 30, "Input is missing"))
    }
    else{
      data_input_OTU(Input = inFile1$datapath, Index = inFile2$datapath)
    }  
  })
  
  
  ## Module 1: Data Summary ##
  output$text_OTU_dir<- renderText({
    paste(dataInput_bar1()[[1]])
  })
  
  output$text_OTU_Individual<- renderText({
    paste(dataInput_bar1()[[2]])
  })
  
  ## individual RA levels ##
  
  dataInput_RA_level<- eventReactive(input$action_RA_level,{
    inFile1 <- input$input_RA_level
    inFile2 <- input$input_RA_level_Index
    if(is.null(input$input_RA_level)|is.null(input$input_RA_level_Index)){
      return(list("Input is missing", "Input is missing", 30, "Input is missing"))
    }
    else{
      data_input_RA(Input = inFile1$datapath, Index = inFile2$datapath, type = input$select_RA_type)
    }  
  })
  
  output$text_RA_dir<- renderText({
    paste(dataInput_RA_level()[[1]])
  })
  
  output$text_RA_Individual<- renderText({
    paste(dataInput_RA_level()[[2]])
  })
  
  ## Module 1: Bar-Plot: group level
  
  observe({
      labels_data_type <- input$input_RA_level$name
    updateSelectInput(session, "input_RA_bar_plot_group",
                      choices = labels_data_type)
    updateSelectInput(session, "input_RA_bar_plot_individual",
                      choices = labels_data_type)
  })
  
  data_bar_plot_group <- eventReactive(input$action_m1_bar_plot_group,{
    source("functions/Bar_Plot_Group.R")
    Bar_Plot_Group_OTU(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[5]], n_top = input$top_n_bar_plot_group, names = dataInput_RA_level()[[6]])
  })
  
  output$bar_plot_group <- renderPlot({
    data_bar_plot_group()
  })
  
  output$download_bar_plot_group<- downloadHandler(
    filename = function(){
      paste("Bar_Plot_Group_Top_", input$top_n_bar_plot_group,"_", dataInput_RA_level()[[6]],".png", sep="")
    },
    content = function(file){
      ggsave(file,plot = data_bar_plot_group(), width = input$bar_plot_group_output_width, height = input$bar_plot_group_output_height)
    }
  )
  
  ## Module 1: Bar-Plot: individual level
  
  data_bar_plot_individual <- eventReactive(input$action_m1_bar_plot_individual,{
    source("functions/Bar_Plot_Individual.R")
    Bar_Plot_Individual_OTU(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[5]], n_top = input$top_n_bar_plot_individual, names = dataInput_RA_level()[[6]])
  })
  
  output$bar_plot_individual <- renderPlot({
    data_bar_plot_individual()
  })
  
  output$download_bar_plot_individual<- downloadHandler(
    filename = function(){
      paste("Bar_Plot_Individual_Top_", input$top_n_bar_plot_individual,"_", dataInput_RA_level()[[6]],".png", sep="")
    },
    content = function(file){
      ggsave(file,plot = data_bar_plot_individual(), width = input$bar_plot_individual_output_width, height = input$bar_plot_individual_output_height)
    }
  )
  
  ## Module 1: Species Accumulation Curve (SAC)
  observe({
    labels_data_type<- input$input_OTU$name
    updateSelectInput(session, "input_count_SAC",
                         choices = labels_data_type)
  })
  
  observeEvent(input$action_OTU, {
    val <- dataInput_bar1()[[3]]
     #Control the value, min, max, and step.
     #Step size is 2 when input value is even; 1 when value is odd.
     updateSliderInput(session, "n_sample_m1_SAC", value = floor(val* 0.6),
                      min = 5, max = val, step = 1)
  })
  
  data_SCA_plot <- eventReactive(input$action_m1_SAC,{
    source("functions/Species_Accu_Curve.R")
    Species_Accu_Curve(OTU_input = dataInput_bar1()[[4]], n_rep = input$n_replications_m1_SAC, n_sample = input$n_sample_m1_SAC)
  })
  
  output$plot_SAC <- renderPlot({
    data_SCA_plot()
  })
  
  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$download_SAC <- downloadHandler(
    filename =  function() {
      paste("Species_Accumulation_Curve_Plot_rep_", input$n_replications_m1_SAC,"_n_", input$n_sample_m1_SAC, ".png", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      png(file, width = input$SAC_output_width, height = input$SAC_output_height)
      #plot(data()[[1]], xmaploc = T ) # draw the plot
      Species_Accu_Curve(OTU_input = dataInput_bar1()[[4]], n_rep = input$n_replications_m1_SAC, n_sample = input$n_sample_m1_SAC)
      dev.off()  # turn the device off
      
    } 
  )
  
  ## Module 1: Rank Abundance Curve (RAC)
  
  observe({
      labels_data_type<- input$input_OTU$name
    updateSelectInput(session, "input_count_RAC",
                      choices = labels_data_type)
  })
  
  data_RAC_plot <- eventReactive(input$action_m1_RAC,{
    source("functions/Rank_Abundance_Curve.R")
    Rank_abundance_curve(OTU_input = dataInput_bar1()[[4]], group_index = dataInput_bar1()[[5]], by = "group")
  })
  
  output$plot_RAC <- renderPlot({
    data_RAC_plot()
  })

  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$download_RAC <- downloadHandler(
    filename =  function() {
      paste("Rank_Abundance_Curve_Plot", ".png", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      png(file, width = input$RAC_output_width, height = input$RAC_output_height)
      #plot(data()[[1]], xmaploc = T ) # draw the plot
      Rank_abundance_curve(OTU_input = dataInput_bar1()[[4]], group_index = dataInput_bar1()[[5]], by = "group")
      dev.off()  # turn the device off
      
    } 
  )  
  ################
  ##  Module 2  ##
  ################
  # Alpha Diversity
  
  observe({
      labels_data_type<- input$input_OTU$name
    updateSelectInput(session, "input_RA_Alpha",
                      choices = labels_data_type)
  })
  
  data_Alpha_Div_plot <- eventReactive(input$action_m2_alpha_diversity,{
    source("functions/Boxplot_Alpha_Diversity.R")
    Boxplot_alpha_diversity(OTU_input = dataInput_bar1()[[4]], group_index = dataInput_bar1()[[5]], index_method = input$select_alpha)
  })
  
  output$boxplot_Alpha_Div <- renderPlot({
    data_Alpha_Div_plot()
  })
  
  Test_Alpha_Div <- eventReactive(input$action_m2_alpha_diversity,{
    source("functions/Test_Alpha_Diversity.R")
    Test_alpha_diversity(OTU_input = dataInput_bar1()[[4]], group_index = dataInput_bar1()[[5]], index_method = input$select_alpha)
  })
  
  output$test_result_alpha_div<- renderText({
    paste("P-value = ",  Test_Alpha_Div()[[1]], sep = "")
  })
  
  output$test_result_alpha_div_post<- renderText({
    paste("P-value", "(",names(Test_Alpha_Div()[[2]]), ") = ", Test_Alpha_Div()[[2]], sep = "", collapse = ", ")
  })
  
  output$download_Boxplot_Alpha_Div<- downloadHandler(
    filename = function(){
      method_tmp <- c("Chao1", "Shannon")
      paste("Alpha_Diversity_Boxlot_", method_tmp[as.numeric(input$select_alpha)],"_index", ".png", sep="")
      #paste("input$boxplot_Alpha_Div",'.png',sep='')
      },
    content = function(file){
      ggsave(file,plot = data_Alpha_Div_plot(), width = input$Boxplot_alpha_div_output_width, height = input$Boxplot_alpha_div_output_height)
    }
  )
  ################
  ##  Module 3  ##
  ################ 
  # Beta Diversity
  
  observe({
    labels_data_type <- "No data selected! please load the RA data with full information of taxonomy first"
    if(input$select_RA_type != "1"){
      labels_data_type <- "No data selected! please load the RA data with full information of taxonomy first"
    }
    else{
      labels_data_type<- input$input_RA_level$name
    }
    updateSelectInput(session, "input_RA_Beta",
                      choices = labels_data_type)
  })
  
  dataInput_Beta_Div<- eventReactive(input$action_m3_beta_diversity,{
    source("functions/Calc_Beta_Div.R")
    Calculate_Beta_div(otumat_2g = dataInput_RA_level()[[4]], div_type =  input$select_beta)
  })
  
  output$text_Beta_Div<- renderText({
    paste(dataInput_Beta_Div()[[2]])
  })
  
  data_Beta_Div_plot <- eventReactive(input$action_m3_beta_diversity,{
    source("functions/Boxplot_Beta_Diversity.R")
    Boxplot_beta_diversity(Beta_diversity_input = dataInput_Beta_Div()[[1]], group_index = dataInput_RA_level()[[5]])
  })
  
  output$boxplot_Beta_Div <- renderPlot({
    data_Beta_Div_plot()
  })
  
  Test_Beta_Div <- eventReactive(input$action_m3_beta_diversity,{
    source("functions/Test_Beta_Diversity.R")
    Test_beta_diversity(Beta_diversity_input = dataInput_Beta_Div()[[1]], group_index = dataInput_RA_level()[[5]])
  })
  
  output$test_result_beta_div<- renderText({
    paste("P-value = ",  Test_Beta_Div()[[1]], sep = "")
  })
  
  output$test_result_beta_div_post<- renderText({
    paste("P-value", "(",names(Test_Beta_Div()[[2]]), ") = ", Test_Beta_Div()[[2]], sep = "", collapse = ", ")
  })
  
  output$download_Boxplot_Beta_Div<- downloadHandler(
    filename = function(){
      method_tmp <- c("UniFrac_unweight", "UniFrac_weight")
      paste("Beta_Diversity_Boxlot_", method_tmp[as.numeric(input$select_beta)], ".png", sep="")
      #paste("input$boxplot_Alpha_Div",'.png',sep='')
    },
    content = function(file){
      ggsave(file,plot = data_Beta_Div_plot(), width = input$Boxplot_beta_div_output_width, height = input$Boxplot_beta_div_output_height)
    }
  )
  
  ## ANOSIM ##
  
  data_ANOSIM_plot <- eventReactive(input$action_m3_ANOSIM,{
    source("functions/Test_ANOSIM.R")
    Test_ANOSIM(beta_div_input = dataInput_Beta_Div()[[1]], group_index = dataInput_RA_level()[[5]], n_perm = input$number_permute_m3_ANOSIM, seed = input$num_seed_m3_ANOSIM)
  })
  
  output$plot_ANOSIM <- renderPlot({
    data_ANOSIM_plot()
  })

  output$download_ANOSIM <- downloadHandler(
    filename =  function() {
      method_tmp <- c("UniFrac_unweight", "UniFrac_weight")
      paste("ANOSIM_", method_tmp[as.numeric(input$select_beta)], ".png", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      png(file, width = input$ANOSIM_output_width, height = input$ANOSIM_output_height)
      #plot(data()[[1]], xmaploc = T ) # draw the plot
      Test_ANOSIM(beta_div_input = dataInput_Beta_Div()[[1]], group_index = dataInput_RA_level()[[5]], n_perm = input$number_permute_m3_ANOSIM, seed = input$num_seed_m3_ANOSIM)
      dev.off()  # turn the device off
    } 
  )
  
  ## PCoA (MDS) biplot ##
  
  data_PCoA_biplot <- eventReactive(input$action_m3_PCoA,{
    source("functions/PCoA_Biplot.R")
    PCoA_biplot(beta_div_input = dataInput_Beta_Div()[[1]], group_index = dataInput_RA_level()[[5]])
  })
  
  output$biplot_PCoA <- renderPlot({
    data_PCoA_biplot()
  })
  
  output$download_Biplot_PCoA<- downloadHandler(
    filename = function(){
      method_tmp <- c("UniFrac_unweight", "UniFrac_weight")
      paste("Beta_Diversity_PCoA_Biplot_", method_tmp[as.numeric(input$select_beta)], ".png", sep="")
      #paste("input$boxplot_Alpha_Div",'.png',sep='')
    },
    content = function(file){
      ggsave(file,plot = data_PCoA_biplot(), width = input$Biplot_PCoA_width, height = input$Biplot_PCoA_height)
    }
  )
  
  ## PCA ##
  
  data_PCA_biplot <- eventReactive(input$action_m3_PCA,{
    source("functions/PCA_Biplot.R")
    PCoA_biplot(beta_div_input = dataInput_Beta_Div()[[1]], group_index = dataInput_RA_level()[[5]])
  })
  
  output$biplot_PCA <- renderPlot({
    data_PCA_biplot()
  })
  
  output$download_Biplot_PCA<- downloadHandler(
    filename = function(){
      method_tmp <- c("UniFrac_unweight", "UniFrac_weight")
      paste("Beta_Diversity_PCA_Biplot_", method_tmp[as.numeric(input$select_beta)], ".png", sep="")
      #paste("input$boxplot_Alpha_Div",'.png',sep='')
    },
    content = function(file){
      ggsave(file,plot = data_PCA_biplot(), width = input$Biplot_PCA_width, height = input$Biplot_PCA_height)
    }
  )
  
  ## NMDS ##
 
  data_NMDS_biplot <- eventReactive(input$action_m3_NMDS,{
    source("functions/NMDS_Biplot.R")
    NMDS_biplot(beta_div_input = dataInput_Beta_Div()[[1]], group_index = dataInput_RA_level()[[5]])
  })
  
  output$biplot_NMDS <- renderPlot({
    data_NMDS_biplot()
  })
  
  output$download_Biplot_NMDS<- downloadHandler(
    filename = function(){
      method_tmp <- c("UniFrac_unweight", "UniFrac_weight")
      paste("Beta_Diversity_NMDS_Biplot_", method_tmp[as.numeric(input$select_beta)], ".png", sep="")
    },
    content = function(file){
      ggsave(file,plot = data_NMDS_biplot(), width = input$Biplot_NMDS_width, height = input$Biplot_NMDS_height)
    }
  )
  
  ################
  ##  Module 4  ##
  ################ 
  ## Statistical Analysis ##
  ## Data pre-processing ##
  
  dataInput_BD<- eventReactive(input$action_m4_input,{
    inFile1 <- input$input_m4_data
    inFile2 <- input$input_m4_Index
    if(is.null(input$input_m4_data)|is.null(input$input_m4_Index)){
      return(list("Input is missing", "Input is missing", "Input is missing", "Input is missing", "Input is missing"))
    }
    else{
      data_input_pre_proc(Input = inFile1$datapath, Index = inFile2$datapath, NA_cut = input$input_m4_missing_rate, zero_cut = input$input_m4_zero_rate)
    }  
  })
  
  output$BD_summary_1<- renderText({
    paste(dataInput_BD()[[1]])
  })
  
  output$BD_summary_2<- renderText({
    paste(dataInput_BD()[[2]])
  })
  
  output$BD_summary_3<- renderText({
    paste(dataInput_BD()[[3]])
  })
  
  output$BD_summary_4<- renderText({
    paste(dataInput_BD()[[4]])
  })
  
  output$BD_summary_5<- renderText({
    paste(dataInput_BD()[[5]])
  })
  
  observe({
    labels_m4_data_type<- input$input_m4_data$name
    ## Data for single marker detection ##
    updateSelectInput(session, "input_m4_single_marker_dect_data",
                      choices = labels_m4_data_type)
    ## Data for LDA ##
    updateSelectInput(session, "input_m4_LDA_data",
                      choices = labels_m4_data_type)
    if(length(names(table(dataInput_BD()[[9]]))) == 2){
      labels_m4_method<- c("T-test", "Wilcoxon rank-sum test")
      updateRadioButtons(session, "radio_m4_posthoc",
                         label = "No post-hoc option",
                         choices = list("No" = 1),
                         selected = 1
      )
    }
    else{
      labels_m4_method<- c("Analysis of variance (one-way ANOVA)", "Kruskal-Wallis test")
      updateRadioButtons(session, "radio_m4_posthoc", 
                         label = "Post-hoc test?",
                         choices = list("No" = 1, "Yes" = 2), 
                         selected = 1
      )
    }
    updateSelectInput(session, "input_m4_single_marker_dect_method",
                      choices = labels_m4_method)
    ## Data for RF
    updateSelectInput(session, "input_m4_RF_data",
                      choices = labels_m4_data_type)
  })
  ## Single Biomarker detection ##
  
  data_output_SBD <- eventReactive(input$action_single_marker_dect_input,{
    source("functions/Single_Biomarker_Detection.R")
    Marker_detection_v3(OTU_input = dataInput_BD()[[8]], group_index = dataInput_BD()[[9]], method = input$input_m4_single_marker_dect_method, ad_hoc = input$radio_m4_posthoc, cut_type = input$select_m4_single_marker_dect_cut, cut = input$select_m4_single_marker_dect_cut_value) 
  })
  
  output$SMD_summary_1<- renderText({
    paste(data_output_SBD()[[1]])
  })
  
  output$SMD_summary_2<- renderText({
    paste(data_output_SBD()[[2]])
  })
  
  output$SMD_summary_Table <- renderTable({
    data_output_SBD()[[4]]
  })
  
  # Downloadable csv of selected dataset ----
  output$download_m4_SMD_table <- downloadHandler(
    filename = function() {
      paste("Single_Biomarker_Detection", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_output_SBD()[[4]], file, row.names = FALSE)
    }
  )
  
  ## LDA ##
  
  data_output_LDA <- eventReactive(input$action_LDA_ES_input,{
    source("functions/LDA_Score.R")
    LDA_ES(OTU_input = dataInput_BD()[[8]], group_index = dataInput_BD()[[9]], cut_p = input$select_m4_LDA_p_cut_value, cut_LDA_log_score = input$select_m4_LDA_score_cut_value, prop_val = input$slider_select_m4_LDA_validate, seed_num = input$num_seed_m4_LDA)
  })
  
  output$LDA_summary_1<- renderText({
    paste(data_output_LDA()[[1]])
  })
  
  output$LDA_summary_2<- renderText({
    paste(data_output_LDA()[[2]])
  })
  
  output$LDA_summary_3<- renderText({
    paste(data_output_LDA()[[3]])
  })
  
  output$LDA_summary_Table <- renderTable({
    data_output_LDA()[[5]]
  })
  
  output$download_m4_LDA_table <- downloadHandler(
    filename = function() {
      paste("LDA_score", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_output_LDA()[[5]], file, row.names = FALSE)
    }
  )
  
  ## LDA effect size plot ##
  
  plot_LDA_effect_size <- eventReactive(input$action_LDA_effect_size,{
    source("functions/LDA_Effect_Size_Plot.R")
    LDA_effect_size_plot(LDA_output = data_output_LDA()[[5]])
  })
  
  output$plot_LDA_effect_size <- renderPlot({
    plot_LDA_effect_size()
  })
  
  output$download_LDA_effect_size_plot<- downloadHandler(
    filename = function(){
      paste("LDA_Effect_Size_Plot.png", sep="")
    },
    content = function(file){
      ggsave(file,plot = plot_LDA_effect_size(), width = input$Plot_LDA_effect_size_width, height = input$Plot_LDA_effect_size_height)
    }
  )
  
  ## Multiple biomarker ROC and AUC ##
  plot_LDA_ROC <- eventReactive(input$action_LDA_ROC_input,{
    source("functions/LDA_ROC_Plot.R")
    LDA_ROC(ROC_input = data_output_LDA()[[6]], AUC_input = data_output_LDA()[[7]])
  })
  
  output$plot_LDA_ROC <- renderPlot({
    plot_LDA_ROC()
  })
  
  output$download_LDA_ROC <- downloadHandler(
    filename =  function() {
      paste("LDA_ROC_Curve.png", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      png(file, width = input$LDA_ROC_output_width, height = input$LDA_ROC_output_height)
      LDA_ROC(ROC_input = data_output_LDA()[[6]], AUC_input = data_output_LDA()[[7]])
      dev.off()  # turn the device off
    } 
  )
  
  ## Random Forest ##
  
  observeEvent(input$action_m4_input,{
    labels_RF_group <- character(0)
    if(length(table(dataInput_BD()[[9]]))>2 ){
      labels_RF_group <- names(table(dataInput_BD()[[9]]))
    }
    else{
      labels_RF_group <- character(0)
    }
    updateSelectizeInput(session, "input_m4_group_RF", choices = labels_RF_group, server = TRUE)
  })
  
  data_output_RF <- eventReactive(input$action_RF_input,{
    source("functions/Random_Forest.R")
    Random_Forest(OTU_input = dataInput_BD()[[8]], group_index = dataInput_BD()[[9]], seed_num = input$num_seed_m4_RF, RF_group_setting = input$input_m4_group_RF)
  })
  
  output$RF_summary_1<- renderText({
    paste(data_output_RF()[[1]])
  })
  
  output$RF_summary_2<- renderText({
    paste(data_output_RF()[[2]])
  })
  
  ## Var Imp Plot
  
  plot_RF_Var_Imp <- eventReactive(input$action_RF_VarImpPlot,{
    source("functions/RF_Var_Imp_Plot.R")
    RF_Var_Imp_Plot(RF_input = data_output_RF()[[3]])
  })
  
  output$plot_RF_Var_Imp <- renderPlot({
    plot_RF_Var_Imp()
  })
  
  output$download_RF_Var_Imp <- downloadHandler(
    filename =  function() {
      paste("RF_Var_Imp_Plot.png", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      png(file, width = input$RF_Var_Imp_output_width, height = input$RF_Var_Imp_output_height)
      RF_Var_Imp_Plot(RF_input = data_output_RF()[[3]])
      dev.off()  # turn the device off
    } 
  )
  
  ## Err Rate plot ##
  
  plot_RF_Err_Rate <- eventReactive(input$action_RF_Err_Rate_Plot,{
    source("functions/RF_Error_Rate_Plot.R")
    RF_Err_Rate_Plot(RF_input = data_output_RF()[[3]])
  })  
  
  output$plot_RF_Err_Rate <- renderPlot({
    plot_RF_Err_Rate()
  })
  
  output$download_RF_Err_Rate <- downloadHandler(
    filename =  function() {
      paste("RF_Error_Rate_Plot.png", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      png(file, width = input$RF_Err_Rate_output_width, height = input$RF_Err_Rate_output_height)
      RF_Err_Rate_Plot(RF_input = data_output_RF()[[3]])
      dev.off()  # turn the device off
    } 
  )
  
  ## RF Recursive Feature Elimination ##
  
  plot_RF_RFE <- eventReactive(input$action_RF_RFE_Plot,{
    source("functions/RF_RFE_Plot.R")
    RF_RFE_Plot(RF_input = data_output_RF()[[6]])
  })  
  
  output$plot_RF_RFE <- renderPlot({
    plot_RF_RFE()
  })
  
  output$download_RF_RFE <- downloadHandler(
    filename =  function() {
      paste("RF_RFE_Plot.png", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      png(file, width = input$RF_RFE_output_width, height = input$RF_RFE_output_height)
      RF_RFE_Plot(RF_input = data_output_RF()[[6]])
      dev.off()  # turn the device off
    } 
  )
  
  ## RF ROC ##
  plot_RF_ROC <- eventReactive(input$action_RF_ROC_input,{
    source("functions/RF_ROC_Plot.R")
    RF_ROC_Plot(ROC_input = data_output_RF()[[4]])
  })
  
  output$plot_RF_ROC <- renderPlot({
    plot_RF_ROC()
  })
  
  output$download_RF_ROC <- downloadHandler(
    filename =  function() {
      paste("RF_ROC_Curve.png", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      png(file, width = input$RF_ROC_output_width, height = input$RF_ROC_output_height)
      RF_ROC_Plot(ROC_input = data_output_RF()[[4]])
      dev.off()  # turn the device off
    } 
  )
  
  
  ################
  ##  Module 5  ##
  ################
  ## ROC ##
  
  ## Update biomarker list from single marker detection method ##
  observeEvent(input$action_single_marker_dect_input,{
    labels_ROC_marker <- "No biomarker available, please run one biomarker discovery method in step 2 under Statistical Analysis Module"
    if(is.null(data_output_SBD()[[3]])){
      labels_ROC_type <- "No biomarker available, please run one biomarker discovery method in step 2 under Statistical Analysis Module"
    }
    else{
      labels_ROC_marker<- data_output_SBD()[[3]]
    }
    updateSelectInput(session, "input_m5_Biomarker_ROC",
                      choices = labels_ROC_marker)
  })
  
  ## Update biomarker list from LDA ##
  
  observeEvent(input$action_LDA_ES_input,{
    labels_ROC_marker <- "No biomarker available, please run one biomarker discovery method in step 2 under Statistical Analysis Module"
    if(is.null(data_output_LDA()[[4]])){
      labels_ROC_type <- "No biomarker available, please run one biomarker discovery method in step 2 under Statistical Analysis Module"
    }
    else{
      labels_ROC_marker<- data_output_LDA()[[4]]
    }
    updateSelectInput(session, "input_m5_Biomarker_ROC",
                      choices = labels_ROC_marker)
  })
  
  ## Update biomarker list from RF ##
  
  observeEvent(input$action_RF_input,{
    labels_ROC_marker <- "No biomarker available, please run one biomarker discovery method in step 2 under Statistical Analysis Module"
    if(is.null(data_output_RF()[[5]])){
      labels_ROC_type <- "No biomarker available, please run one biomarker discovery method in step 2 under Statistical Analysis Module"
    }
    else{
      labels_ROC_marker<- data_output_RF()[[5]]
    }
    updateSelectInput(session, "input_m5_Biomarker_ROC",
                      choices = labels_ROC_marker)
  })
  
  #########################################
  
  data_AUC <- eventReactive(input$action_m5_ROC_AUC,{
    source("functions/ROC_AUC.R")
    ROC_AUC(data_all = dataInput_BD()[[8]], group_index = dataInput_BD()[[9]], marker_name = input$input_m5_Biomarker_ROC, ROC_group_setting = input$input_m5_group_ROC)
  })
  
  output$AUC_CI_summary1 <- renderText({
    data_AUC()[[1]]
  })
  
  output$AUC_CI_summary2 <- renderText({
    data_AUC()[[2]]
  })
  
  plot_ROC <- eventReactive(input$action_m5_ROC_AUC,{
    source("functions/ROC_AUC_Plot.R")
    ROC_AUC_Plot(data_all = dataInput_BD()[[8]], group_index = dataInput_BD()[[9]], marker_name = input$input_m5_Biomarker_ROC, ROC_group_setting = input$input_m5_group_ROC)
  })
  
  output$plot_ROC <- renderPlot({
    plot_ROC()
  })
  
  output$download_ROC <- downloadHandler(
    filename =  function() {
      paste("ROC_Curve_", input$input_m5_Biomarker_ROC, ".png", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      png(file, width = input$ROC_output_width, height = input$ROC_output_height)
      ROC_AUC_Plot(data_all = dataInput_BD()[[8]], group_index = dataInput_BD()[[9]], marker_name = input$input_m5_Biomarker_ROC, ROC_group_setting = input$input_m5_group_ROC)
      dev.off()  # turn the device off
    } 
  )
  
  observeEvent(input$action_m4_input,{
    labels_ROC_group <- character(0)
    if(length(table(dataInput_BD()[[9]]))>2 ){
      labels_ROC_group <- names(table(dataInput_BD()[[9]]))
    }
    else{
      labels_ROC_group <- character(0)
    }
    updateSelectizeInput(session, "input_m5_group_ROC", choices = labels_ROC_group, server = TRUE)
  })
  
  ################
  ##  Module 6  ##
  ################  
  ## Functional prediction of metagenomes 
  shinyDirChoose(
    input,
    'folder',
    roots = c(home = '~')
  )
  global <- reactiveValues(datapath = getwd())
  
  dir <- reactive(input$folder)
  
  output$dir <- renderText({
    global$datapath
  })
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$folder
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 global$datapath <-
                   file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
               })

  #output$filepaths <- renderPrint({parseDirPath(roots, input$folder)})
  
  dataInput_Fun_Pred<- eventReactive(input$action_m6_Fun_Pred,{
    source("functions/Fun_Pred_Metagenomes.R")
    inFile1 <- input$input_m6_OTU_Fun_Pred
    inFile2 <- input$input_m6_Index_Fun_Pred
    inFile3 <- input$input_m6_OTU_ID
    Fun_Pred_Metagenomes(OTU_input_dir = inFile1$datapath, group_index_dir = inFile2$datapath, ref_database = input$select_m6_KO_Ref,ref_path = global$datapath, OTU_ID_dir = inFile3$datapath)
  })
  
  output$Fun_Pred_summary1 <- renderText({
    dataInput_Fun_Pred()[[1]]
  })
  
  output$Fun_Pred_summary2 <- renderText({
    dataInput_Fun_Pred()[[2]]
  })
  
  output$Fun_Pred_summary3 <- renderText({
    dataInput_Fun_Pred()[[3]]
  })
  
  output$Fun_Pred_summary4 <- renderText({
    dataInput_Fun_Pred()[[4]]
  })
  
  output$Fun_Pred_summary5 <- renderText({
    dataInput_Fun_Pred()[[5]]
  })
  
  #output$Fun_Pred_summary_Table <- renderTable({
  #  dataInput_Fun_Pred()
  #})
  
  output$download_m6_Fun_Pred_L1 <- downloadHandler(
    filename = function() {
      paste("Fun_Pred_L1", ".txt", sep = "")
    },
    content = function(file) {
      write.table(dataInput_Fun_Pred()[[6]], file, row.names = F, col.names = T, quote = F, sep ="\t")
    }
  )
  
  output$download_m6_Fun_Pred_L2 <- downloadHandler(
    filename = function() {
      paste("Fun_Pred_L2", ".txt", sep = "")
    },
    content = function(file) {
      write.table(dataInput_Fun_Pred()[[7]], file, row.names = F, col.names = T, quote = F, sep ="\t")
    }
  )
  
  output$download_m6_Fun_Pred_L3 <- downloadHandler(
    filename = function() {
      paste("Fun_Pred_L3", ".txt", sep = "")
    },
    content = function(file) {
      write.table(dataInput_Fun_Pred()[[8]], file, row.names = F, col.names = T, quote = F, sep ="\t")
    }
  )
  
}
