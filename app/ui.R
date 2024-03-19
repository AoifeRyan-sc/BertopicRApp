df <- data

ui <- fluidPage(
  theme = shinythemes::shinytheme("readable"),
  
  shiny::tagList(
    shinyjs::useShinyjs(),
    navbarPage(
      "BertopicR",
      
      tabPanel("Clustering",
               titlePanel("Clustering"),
               clusteringUi("clustering_panel")
      ),
      
      tabPanel("Explore the Model",
               modelExploreUi("explore_model_panel")
      ),
      
      tabPanel("Outlier Manipulation",
               titlePanel("Outlier Manipulation"),
               
               outlierUi("outlier_panel")
      )
    ) 
  )
) 