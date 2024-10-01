
server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2)
  
  r <- shiny::reactiveValues(model = NULL,
                             reduced_embeddings = NULL)
  
  # reducingServer("reducing_panel", df = df)
  
  clustering_output <- clusteringServer("clustering_panel", r)
  # observe({
  #   clusters <- clustering_output$clusters()
  #   model <- clustering_output$model()
  #   cluster_model <- clustering_output$cluster_model()
  #   df <- clustering_output$df()
  # })
  # 
  # shiny::observe({
  #   if(!is.null(model()) && cluster_model() == "K-Means"){
  #     shiny::hideTab(inputId = "main_navpage", target = "Outlier Manipulation")
  #   }
  # })
  # 
  # shiny::observe({
  #   if(is.null(model())){
  #     shiny::showTab(inputId = "main_navpage", target = "Outlier Manipulation")
  #   }
  # })
  # 
  # modelExploreServer("explore_model_panel", model = model, df= df)
  # outlierServer("outlier_panel", df = df, model = model, clusters = clusters, embedder = embedder)
}
