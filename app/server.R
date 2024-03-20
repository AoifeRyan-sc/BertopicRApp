
server <- function(input, output, session) {
  
  clustering_output <- clusteringServer("clustering_panel", df = df)
  clusters <- clustering_output$clusters
  model <- clustering_output$model

  modelExploreServer("explore_model_panel", model = model, df= df)
  outlierServer("outlier_panel", df = df, model = model, clusters = clusters, embedder = embedder)
}
