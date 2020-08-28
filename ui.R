library(shiny)
library(shinythemes)
library(shinyFiles)

shinythemes::themeSelector()
navbarPage(
  theme = shinytheme("cerulean"),
  "wiSDOM",
  ################
  ##  Module 1  ##
  ################
  navbarMenu("Data",
             "1. Inputs",
             tabPanel("OTUs (Count)",
                      sidebarPanel(
                        tags$div(tags$label(h3("Load OTU Count"))),
                        tags$div(tags$label(h3("1. Choose OTU file (Count)"))),
                        fileInput("input_OTU", label = ""),
                        #radioButtons("OTU_type", label = h4("OTU Type"),
                        #             choices = list("Count" = 1, "RA" = 2), 
                        #             selected = 1),
                        tags$div(tags$label(h3("2. Choose an index file"))),
                        fileInput("input_Index", label = ""),
                        actionButton("action_OTU", "Load")
                      ),
                      mainPanel(theme = "bootstrap.css",
                                includeScript("./www/text.js"),
                                tags$div(tags$label(h3("Logs"))),
                                verbatimTextOutput("text_OTU_dir"),
                                verbatimTextOutput("text_OTU_Individual")
                                #tableOutput("contents")
                      ),
                      ),
             tabPanel("RA (Whole or individual level)",
                      sidebarPanel(
                        tags$div(tags$label(h3("Load RA"))),
                        tags$div(tags$label(h3("1. Choose RA file"))),
                        fileInput("input_RA_level", label = ""),
                        tags$div(tags$label(h3("2. Choose level"))),
                        selectInput("select_RA_type", label = h4("Select level"), 
                                    choices = list("Whole" = 1,"Phylum" = 2, "Class" = 3, "Order" = 4, "Family" = 5,  "Genus" = 6, "Species" = 7), 
                                    selected = 1),
                        tags$div(tags$label(h3("3. Choose an index file"))),
                        fileInput("input_RA_level_Index", label = ""),
                        actionButton("action_RA_level", "Load")
                      ),
                      mainPanel(theme = "bootstrap.css",
                                includeScript("./www/text.js"),
                                tags$div(tags$label(h3("Logs"))),
                                verbatimTextOutput("text_RA_dir"),
                                verbatimTextOutput("text_RA_Individual")
                                #tableOutput("contents")
                      ),
                      ),
             "----",
             "2. Visualization - RA",
             tabPanel("Distribution of Top Bacterial Taxa (groups)",
                      sidebarPanel(
                        tags$div(tags$label(h3("Distribution of top bacterial taxa (groups)"))),
                        tags$div(tags$label(h3("1. Choose RA file"))),
                        selectInput("input_RA_bar_plot_group", h4("RA Data"),
                                    choices = "No data selected! please load the RA data first"),
                        numericInput("top_n_bar_plot_group", label = h4("Number of top bacterial taxa (Max = 21)"), value = 10),
                        actionButton("action_m1_bar_plot_group", "Plot"),
                      ),
                      mainPanel(
                        plotOutput("bar_plot_group"),
                        downloadButton(outputId = "download_bar_plot_group", label = "Download the plot"),
                        numericInput("bar_plot_group_output_width", label = h4("Width of output figure (inch)"), value = 10, width = "400px"),
                        numericInput("bar_plot_group_output_height", label = h4("Height of output figure (inch)"), value = 10, width = "400px")
                      ),
                      ),
             tabPanel("Distribution of Top Bacterial Taxa (samples)",
                      sidebarPanel(
                        tags$div(tags$label(h3("Distribution of top bacterial taxa (samples)"))),
                        tags$div(tags$label(h3("1. Choose RA file"))),
                        selectInput("input_RA_bar_plot_individual", h4("RA Data"),
                                    choices = "No data selected! please load the RA data first"),
                        numericInput("top_n_bar_plot_individual", label = h4("Number of top bacterial taxa (Max = 21)"), value = 10),
                        actionButton("action_m1_bar_plot_individual", "Plot"),
                      ),
                      mainPanel(
                        plotOutput("bar_plot_individual"),
                        downloadButton(outputId = "download_bar_plot_individual", label = "Download the plot"),
                        numericInput("bar_plot_individual_output_width", label = h4("Width of output figure (inch)"), value = 10, width = "400px"),
                        numericInput("bar_plot_individual_output_height", label = h4("Height of output figure (inch)"), value = 10, width = "400px")
                      ),
                      ),
             "----",
             "3. Visualization - OTU Count",
             tabPanel("Species Accumulation Curve",
                      sidebarPanel(
                        tags$div(tags$label(h3("Species Accumulation Curve"))),
                        tags$div(tags$label(h3("1. Select Input"))),
                        selectInput("input_count_SAC", h4("OTU Count Data"),
                                       choices = "No data selected! please load the count data first"),
                        numericInput("n_replications_m1_SAC", label = h4("Number of Replication"), value = 5),
                        #numericInput(n_sample_m1_SAC", label = h4("Number of Samples in Each Replication"), value = 20),
                        sliderInput("n_sample_m1_SAC", label = h4("Number of Samples in Each Replication"), min = 5, max = 30, step = 1, value = 20),
                        actionButton("action_m1_SAC", "Plot")
                      ),
                      mainPanel(
                        plotOutput("plot_SAC"),
                        downloadButton(outputId = "download_SAC", label = "Download the plot"),
                        numericInput("SAC_output_width", label = h4("Width of output figure"), value = 480, width = "200px"),
                        numericInput("SAC_output_height", label = h4("Height of output figure"), value = 480, width = "200px")
                      ),
             ),
             tabPanel("Rank Abundance Curve",
                      sidebarPanel(
                        tags$div(tags$label(h3("Rank Abundance Curve"))),
                        tags$div(tags$label(h3("1. Select Input"))),
                        selectInput("input_count_RAC", h4("OTU Count Data"),
                                    choices = "No data selected! please load the count data first"),
                        actionButton("action_m1_RAC", "Plot"),
                        tags$div(tags$label("May take few minutes"))
                      ),
                      mainPanel(
                        plotOutput("plot_RAC"),
                        downloadButton(outputId = "download_RAC", label = "Download the plot"),
                        tags$div(tags$label("May take few minutes")),
                        numericInput("RAC_output_width", label = h4("Width of output figure"), value = 480, width = "200px"),
                        numericInput("RAC_output_height", label = h4("Height of output figure"), value = 480, width = "200px")
                      ),
             )
             
        ),
  ################
  ##  Module 2  ##
  ################
  tabPanel('\\( \\alpha \\) Diversity',
            withMathJax(),
            sidebarPanel(
              tags$div(tags$label(h3("\\( \\alpha \\) Diversity"))),
              tags$div(tags$label(h3("1. Select Input"))),
              selectInput("input_RA_Alpha", h4("Count Data"),
                          choices = "No data selected! please load the count data first"),
              selectInput("select_alpha", label = h3("2. Select Method"), 
                          choices = list("Chao1 index" = 1, "Shannon index" = 2), 
                          selected = 1),
              actionButton("action_m2_alpha_diversity", "Access Result")
             #numericInput("number_clusters", label = h4("5. Number of clusters"), value = 3)
             #radioButtons("gender_bar1", label = h3("Gender"),
             #choices = list("Men" = 1, "Women" = 2), 
             #selected = 1),
             #sliderInput("yr_range_bar1", h3("Year Range:"),
             #min = 1968, max = 2015, value = c(2009,2010)),
             #textInput("player_name_bar1", h3("Player's Name"), "Rafael Nadal"),
             #actionButton("action_bar1", "Update")
             #submitButton("Update")
           ),
           mainPanel(
             tags$div(tags$label(h3("1. Testing result of \\( \\alpha \\) Diversity"))),
             verbatimTextOutput("test_result_alpha_div"),
             tags$div(tags$label(h3("Post-Hoc P-values"))),
             verbatimTextOutput("test_result_alpha_div_post"),
             tags$div(tags$label(h3("2. Boxplot of \\( \\alpha \\) Diversity"))),
             plotOutput("boxplot_Alpha_Div"),
             downloadButton(outputId = "download_Boxplot_Alpha_Div", label = "Download the plot"),
             numericInput("Boxplot_alpha_div_output_width", label = h4("Width of output figure (inch)"), value = 10, width = "400px"),
             numericInput("Boxplot_alpha_div_output_height", label = h4("Height of output figure (inch)"), value = 10, width = "400px")
           )
  ),
  ################
  ##  Module 3  ##
  ################
  navbarMenu('\\( \\beta \\) Diversity',
             "1. Statistical Analysis of \\( \\beta \\) Diversity",
             tabPanel("Step 1: \\( \\beta \\) Diversity Box-plot",
                      sidebarPanel(
                        tags$div(tags$label(h3("\\( \\beta \\) Diversity"))),
                        tags$div(tags$label(h3("1. Select Input"))),
                        selectInput("input_RA_Beta", h4("RA"),
                                    choices = "No data selected! please load the RA data with full information of taxonomy first"),
                        selectInput("select_beta", label = h3("2. Select Method"), 
                                    choices = list("UniFrac (unweighted)" = 1, "UniFrac (weighted)" = 2), 
                                    selected = 1),
                        actionButton("action_m3_beta_diversity", "Access Result")
                      ),
                      mainPanel(
                        tags$div(tags$label(h3("1. Testing result of \\( \\beta \\) Diversity"))),
                        verbatimTextOutput("text_Beta_Div"),
                        verbatimTextOutput("test_result_beta_div"),
                        #verbatimTextOutput("test_result_alpha_div"),
                        tags$div(tags$label(h3("Post-Hoc P-values"))),
                        verbatimTextOutput("test_result_beta_div_post"),
                        tags$div(tags$label(h3("2. Boxplot of \\( \\beta \\) Diversity"))),
                        plotOutput("boxplot_Beta_Div"),
                        #plotOutput("boxplot_Alpha_Div"),
                        downloadButton(outputId = "download_Boxplot_Beta_Div", label = "Download the plot"),
                        numericInput("Boxplot_beta_div_output_width", label = h4("Width of output figure (inch)"), value = 10, width = "400px"),
                        numericInput("Boxplot_beta_div_output_height", label = h4("Height of output figure (inch)"), value = 10, width = "400px")
                      ),
                      ),
 
             tabPanel("Step 2: Analysis of Similarity (ANOSIM)",
                      sidebarPanel(
                        tags$div(tags$label(h3("Analysis of Similarity (ANOSIM)"))),
                        tags$div(tags$label(h3("1. Select Number of Permutations (suggested > 1000)"))),
                        numericInput("number_permute_m3_ANOSIM", label = h3("Number of permutations"), value = 1000),
                        tags$div(tags$label(h3("2. Set seed Numbers"))),
                        numericInput("num_seed_m3_ANOSIM", label = h3("Seed Number"), value = 9527),
                        actionButton("action_m3_ANOSIM", "Access Result"),
                      ),
                      mainPanel(
                        plotOutput("plot_ANOSIM"),
                        downloadButton(outputId = "download_ANOSIM", label = "Download the plot"),
                        numericInput("ANOSIM_output_width", label = h4("Width of output figure"), value = 480, width = "200px"),
                        numericInput("ANOSIM_output_height", label = h4("Height of output figure"), value = 480, width = "200px")
                      ),
             ),
             "----",
             "2. Dimension Reduction and Clustering",
             tabPanel("Principal Coordinate Analysis (PCoA/MDS)",
                      sidebarPanel(
                        tags$div(tags$label(h3("Principal Coordinate Analysis (PCoA/MDS)"))),
                        actionButton("action_m3_PCoA", "Access Bi-plot"),
                      ),
                      mainPanel(
                        plotOutput("biplot_PCoA"),
                        downloadButton(outputId = "download_Biplot_PCoA", label = "Download the plot"),
                        numericInput("Biplot_PCoA_width", label = h4("Width of output figure (inch)"), value = 10, width = "400px"),
                        numericInput("Biplot_PCoA_height", label = h4("Height of output figure (inch)"), value = 10, width = "400px")
                      ),
                      ),
             tabPanel("Principal Component Analysis (PCA)",
                      sidebarPanel(
                        tags$div(tags$label(h3("Principal Component Analysis (PCA)"))),
                        actionButton("action_m3_PCA", "Access Bi-plot"),
                      ),
                      mainPanel(
                        plotOutput("biplot_PCA"),
                        downloadButton(outputId = "download_Biplot_PCA", label = "Download the plot"),
                        numericInput("Biplot_PCA_width", label = h4("Width of output figure (inch)"), value = 10, width = "400px"),
                        numericInput("Biplot_PCA_height", label = h4("Height of output figure (inch)"), value = 10, width = "400px")
                      ),
                      ),
             tabPanel("Non-metric Multidimentional Scaling (NMDS)",
                      sidebarPanel(
                        tags$div(tags$label(h3("Non-metric Multidimentional Scaling (NMDS)"))),
                        actionButton("action_m3_NMDS", "Access Bi-plot"),
                      ),
                      mainPanel(
                        plotOutput("biplot_NMDS"),
                        downloadButton(outputId = "download_Biplot_NMDS", label = "Download the plot"),
                        numericInput("Biplot_NMDS_width", label = h4("Width of output figure (inch)"), value = 10, width = "400px"),
                        numericInput("Biplot_NMDS_height", label = h4("Height of output figure (inch)"), value = 10, width = "400px")
                      ),
                      )
             ),
  ################
  ##  Module 4  ##
  ################
  navbarMenu('Statistical Analysis',
             "Step 1: Data Pre-processing",
             tabPanel("Upload the Data",
                      sidebarPanel(
                        tags$div(tags$label(h3("1. Choose a file"))),
                        fileInput("input_m4_data", label = ""),
                        tags$div(tags$label(h3("2. Choose an index file"))),
                        fileInput("input_m4_Index", label = ""),
                        tags$div(tags$label(h3("3. Pre-processing Settings"))),
                        tags$div(tags$label(h4("Biomarker will be filtered out if: "))),
                        sliderInput("input_m4_missing_rate", label = h4("Missing Rate (%) in each group greater than"), min = 0, max = 100, value = 20),
                        sliderInput("input_m4_zero_rate", label = h4("Zero Rate (%) in each group greater than"), min = 0, max = 100, value = 50),
                        tags$div(tags$label(h4("Biomarker with 0 Standard deviation in any one group will be filtered out!"))),
                        actionButton("action_m4_input", "Action"),
                      ),
                      mainPanel(theme = "bootstrap.css",
                                includeScript("./www/text.js"),
                                tags$div(tags$label(h3("Logs"))),
                                verbatimTextOutput("BD_summary_1"),
                                verbatimTextOutput("BD_summary_2"),
                                verbatimTextOutput("BD_summary_3"),
                                verbatimTextOutput("BD_summary_4"),
                                verbatimTextOutput("BD_summary_5")
                                #tableOutput("contents")
                      ),
                      ),
             "----",
             "Step 2: Biomarker Discovery",
             tabPanel("Single Biomarker Detection Method",
                      sidebarPanel(
                        tags$div(tags$label(h3("Single Biomarker Detection Method"))),
                        tags$div(tags$label(h3("1. Input"))),
                        selectInput("input_m4_single_marker_dect_data", h4("Choose Filtered Data from Step 1"),
                                    choices = "Data is not available! Please load the data and run pre-processing in step 1 first"),
                        tags$div(tags$label(h3("2. Methods"))),
                        selectInput("input_m4_single_marker_dect_method", h4("Choose parametric or non-parametric approach"),
                                    choices = "Please load the data and run pre-processing in step 1 first"),
                        radioButtons("radio_m4_posthoc", label = "Post-hoc test?",
                                     choices = list("No" = 1, "Yes" = 2), 
                                     selected = 1),
                        tags$div(tags$label(h3("3. Select Detection Criteria"))),
                        selectInput("select_m4_single_marker_dect_cut", label = h4("Select P-value or false discovery rate (FDR) from Benjamini-Hochberg procedure"), 
                                    choices = list("P-value" = 1, "FDR" = 2), 
                                    selected = 1),
                        #tags$div(tags$label(h3("<"))),
                        numericInput("select_m4_single_marker_dect_cut_value", label = h4("Less than (<)"), value = 0.05, width = "200px"),
                        actionButton("action_single_marker_dect_input", "Run"),
                      ),
                      mainPanel(theme = "bootstrap.css",
                                includeScript("./www/text.js"),
                                tags$div(tags$label(h3("Logs"))),
                                verbatimTextOutput("SMD_summary_1"),
                                verbatimTextOutput("SMD_summary_2"),
                                downloadButton("download_m4_SMD_table", "Download the table"),
                                tableOutput("SMD_summary_Table"),
                      ),
                      ),
             
             tabPanel("Linear Discriminant Analysis (LDA) Effect Size",
                      sidebarPanel(
                        tags$div(tags$label(h3("Linear Discriminant Analysis (LDA) Effect Size"))),
                        tags$div(tags$label(h3("1. Input"))),
                        selectInput("input_m4_LDA_data", h4("Choose Filtered Data from Step 1"),
                                    choices = "Data is not available! Please load the data and run pre-processing in step 1 first"),
                        tags$div(tags$label(h3("2. Select p-value threshold of Kruskal-Wallis test"))),
                        numericInput("select_m4_LDA_p_cut_value", label = h4("p-value less than"), value = 0.05, width = "300px"),
                        tags$div(tags$label(h3("3. Threshold of log LDA score"))),
                        numericInput("select_m4_LDA_score_cut_value", label = h4("log LDA score greater than"), value = 2, width = "300px"),
                        tags$div(tags$label(h3("4. Select % of samples in validation data set and seed number"))),
                        sliderInput("slider_select_m4_LDA_validate", label = h4("% of samples (suggested between 20% to 30%)"), min = 0.2, 
                                    max = 0.3, value = 0.25, step = 0.01),
                        numericInput("num_seed_m4_LDA", label = h4("Seed Number"), value = 9527),
                        actionButton("action_LDA_ES_input", "Run"),
                      ),
                      mainPanel(theme = "bootstrap.css",
                                includeScript("./www/text.js"),
                                tags$div(tags$label(h3("Logs"))),
                                verbatimTextOutput("LDA_summary_1"),
                                verbatimTextOutput("LDA_summary_2"),
                                verbatimTextOutput("LDA_summary_3"),
                                tags$div(tags$label(h3("LDA result and ROC Curve"))),
                                downloadButton("download_m4_LDA_table", "Download the table"),
                                tableOutput("LDA_summary_Table"),
                                actionButton("action_LDA_ROC_input", "ROC Curve and AUC (two groups only)"),
                                plotOutput("plot_LDA_ROC"),
                                downloadButton(outputId = "download_LDA_ROC", label = "Download the plot"),
                                numericInput("LDA_ROC_output_width", label = h4("Width of output figure"), value = 480, width = "200px"),
                                numericInput("LDA_ROC_output_height", label = h4("Height of output figure"), value = 480, width = "200px")
                      ),
                      ),
             tabPanel("Random Forest (RF)",
                      sidebarPanel(
                        tags$div(tags$label(h3("Random Forest (RF)"))),
                        tags$div(tags$label(h3("1. Input"))),
                        selectInput("input_m4_RF_data", h4("Choose Filtered Data from Step 1"),
                                    choices = "Data is not available! Please load the data and run pre-processing in step 1 first"),
                        tags$div(tags$label(h3("2. Set seed Numbers"))),
                        numericInput("num_seed_m4_RF", label = h3("Seed Number"), value = 9527),
                        tags$div(tags$label(h3("3. Determine group index for ROC"))),
                        selectizeInput("input_m4_group_RF", h4("Select first index group for ROC (please do not check all groups)"), choices = NULL, multiple = TRUE),
                        actionButton("action_RF_input", "Run"),
                      ),
                      mainPanel(theme = "bootstrap.css",
                                includeScript("./www/text.js"),
                                tags$div(tags$label(h3("Logs"))),
                                verbatimTextOutput("RF_summary_1"),
                                verbatimTextOutput("RF_summary_2"),
                                tags$div(tags$label(h3("Variance Importance Plot"))),
                                actionButton("action_RF_VarImpPlot", "Access Variance Importance Plot"),
                                plotOutput("plot_RF_Var_Imp"),
                                downloadButton(outputId = "download_RF_Var_Imp", label = "Download the plot"),
                                numericInput("RF_Var_Imp_output_width", label = h4("Width of output figure"), value = 480, width = "200px"),
                                numericInput("RF_Var_Imp_output_height", label = h4("Height of output figure"), value = 480, width = "200px"),
                                tags$div(tags$label(h3("ROC Curve and AUC"))),
                                actionButton("action_RF_ROC_input", "ROC Curve and AUC"),
                                plotOutput("plot_RF_ROC"),
                                downloadButton(outputId = "download_RF_ROC", label = "Download the plot"),
                                numericInput("RF_ROC_output_width", label = h4("Width of output figure"), value = 480, width = "200px"),
                                numericInput("RF_ROC_output_height", label = h4("Height of output figure"), value = 480, width = "200px"),
                                
                      ),
                      
                      )
             
             ),
  ################
  ##  Module 5  ##
  ################
  ## ROC Curve and AUC
  tabPanel('ROC Curve and AUC',
           sidebarPanel(
             tags$div(tags$label(h3("Interactive ROC and AUC"))),
             tags$div(tags$label(h3("1. Select Biomarker"))),
             selectInput("input_m5_Biomarker_ROC", h4("Select one biomarker for ROC and AUC"),
                         choices = "No biomarker available, please run one biomarker discovery method in step 2 under Statistical Analysis Module"),
             tags$div(tags$label(h3("2. Determine group index for ROC"))),
             selectizeInput("input_m5_group_ROC", h4("Select first index group for ROC (please do not check all groups)"), choices = NULL, multiple = TRUE),
             actionButton("action_m5_ROC_AUC", "Plot"),
           ),
           mainPanel(theme = "bootstrap.css",
                     includeScript("./www/text.js"),
                     tags$div(tags$label(h3("ROC Setting"))),
                     verbatimTextOutput("AUC_CI_summary1"),
                     tags$div(tags$label(h3("AUC and 95% C.I."))),
                     verbatimTextOutput("AUC_CI_summary2"),
                     tags$div(tags$label(h3("ROC Curve"))),
                     plotOutput("plot_ROC"),
                     downloadButton(outputId = "download_ROC", label = "Download the plot"),
                     numericInput("ROC_output_width", label = h4("Width of output figure"), value = 480, width = "200px"),
                     numericInput("ROC_output_height", label = h4("Height of output figure"), value = 480, width = "200px")
           ),
           
           
  ),
  ################
  ##  Module 6  ##
  ################  
  ## Functional prediction of metagenomes 
  tabPanel('Functional Prediction of Metagenomes',
           sidebarPanel(
             tags$div(tags$label(h3("Functional Prediction of Metagenomes"))),
             tags$div(tags$label(h3("1. Upload OTUs file (Count)"))),
             fileInput("input_m6_OTU_Fun_Pred", label = ""),
             #radioButtons("OTU_type", label = h4("OTU Type"),
             #             choices = list("Count" = 1, "RA" = 2), 
             #             selected = 1),
             tags$div(tags$label(h3("2. Upload an index file"))),
             fileInput("input_m6_Index_Fun_Pred", label = ""),
             tags$div(tags$label(h3("3. Choose KO terms reference"))),
             selectInput("select_m6_KO_Ref", label = h4("Select KO terms reference"), 
                         choices = list("GreenGenes" = 1, "Silva" = 2), 
                         selected = 1),
             tags$div(tags$label(h3("4. Choose directory to save the reference"))),
             verbatimTextOutput("dir", placeholder = TRUE), 
             shinyDirButton("folder", "Folder select", "Please select a folder"),
             tags$div(tags$label(h3("5. Upload OTU ID (Green Genes reference only)"))),
             fileInput("input_m6_OTU_ID", label = ""),
             actionButton("action_m6_Fun_Pred", "Run"),
           ),
           mainPanel(theme = "bootstrap.css",
                     includeScript("./www/text.js"),
                     tags$div(tags$label(h3("Logs"))),
                     verbatimTextOutput("Fun_Pred_summary1"),
                     verbatimTextOutput("Fun_Pred_summary2"),
                     verbatimTextOutput("Fun_Pred_summary3"),
                     verbatimTextOutput("Fun_Pred_summary4"),
                     verbatimTextOutput("Fun_Pred_summary5"),
                     #downloadButton("download_m6_Fun_Pred", "Download the table"),
                     downloadButton("download_m6_Fun_Pred_L1", "Download level 1 table"),
                     downloadButton("download_m6_Fun_Pred_L2", "Download level 2 table"),
                     downloadButton("download_m6_Fun_Pred_L3", "Download level 3 table")
           ),
             
  )
)

