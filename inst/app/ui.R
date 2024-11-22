ui <- shiny::fluidPage(
  theme = bslib::bs_theme(bootswatch = "zephyr"), # need to use a bslib theme for bslib tooltip to work
  # theme = bslib::bs_theme(bootswatch = "cerulean"), # need to use a bslib theme for bslib tooltip to work
  shiny::tagList(
    shinyjs::useShinyjs(),
    navbarPage(
      "BertopicR",
      id = "main_navpage",
      tabPanel("Uploading",
               titlePanel("Upload Data"),
               uploadUi("upload_panel")
      ),
      tabPanel("Reduce Embeddings",
               titlePanel("Reduce Embeddings"),
               embedReduceUi("embedding_reducing_panel")
      ),
      # tabPanel("Embedding",
      #          titlePanel("Embedding"),
      #          embedReduceUi("embedding_reducing_panel")
      # ),
      tabPanel("Clustering",
               titlePanel("Clustering"),
               clusteringUi("clustering_panel")
      ),
      
      tabPanel("Explore the Model",
               modelExploreUi("explore_model_panel")
      ),
      
      tabPanel("Outlier Manipulation",
               id = "outlier_manipulation",
               titlePanel("Outlier Manipulation"),
               
               outlierUi("outlier_panel")
      )
    ) 
  )
) 