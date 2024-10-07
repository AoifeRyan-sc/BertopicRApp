ui <- fluidPage(
  theme = shinythemes::shinytheme("readable"),
  
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