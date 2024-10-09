
server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2)
  
  r <- shiny::reactiveValues(model = NULL,
                             reduced_embeddings = NULL,
                             embedding_happening = FALSE,
                             reducing_happening = FALSE,
                             clusters = NULL)
  
  # reducingServer("reducing_panel", df = df)
  uploadServer("upload_panel", r)
  embedReduceServer("embedding_reducing_panel", r)
  clusteringServer("clustering_panel", r)
  # observe( {
  #   clusters <- clustering_output$clusters()
  #   model <- clustering_output$model()
  #   cluster_model <- clustering_output$cluster_model()
  #   df <- clustering_output$df()
  # })
  # 
  shiny::observe({
    if(!is.null(r$model) && r$cluster_model == "K-Means"){
      shiny::hideTab(inputId = "main_navpage", target = "Outlier Manipulation")
    }
  })
  # 
  shiny::observe({
    if(is.null(r$model)){
      shiny::showTab(inputId = "main_navpage", target = "Outlier Manipulation")
    }
  })

  modelExploreServer("explore_model_panel", r)
  outlierServer("outlier_panel", r)
}
