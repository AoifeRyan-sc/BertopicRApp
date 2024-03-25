
server <- function(input, output, session) {
  
  df <- reactive({
    data
  })
  
  clustering_output <- clusteringServer("clustering_panel", df = df)
  clusters <- clustering_output$clusters
  model <- clustering_output$model
  cluster_model <- clustering_output$cluster_model

  shiny::observe({
    if(!is.null(model()) && cluster_model() == "K-Means"){
      shiny::hideTab(inputId = "main_navpage", target = "Outlier Manipulation")
    }
  })
  
  shiny::observe({
    if(is.null(model())){
      shiny::showTab(inputId = "main_navpage", target = "Outlier Manipulation")
    }
  })

  modelExploreServer("explore_model_panel", model = model, df= df)
  outlierServer("outlier_panel", df = df, model = model, clusters = clusters, embedder = embedder)
}
