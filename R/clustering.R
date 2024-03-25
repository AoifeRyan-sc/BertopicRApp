#' Ui specs for clustering tab
#'
#' @param id parameter for shiny identification
#'
#' @noRd
#' 
clusteringUi <- function(id) {
  
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      modellingUi(ns("modelling_selection"))
       ),
    shiny::mainPanel(
      plotly::plotlyOutput(ns("cluster_plot"))
      )
    ) 
  
}

#' Clustering UI Server Function
#'
#' @param id parameter for shiny identification
#' @param df reactive dataframe containing docs and embedding info 
#'
#' @noRd

clusteringServer <- function(id, df = df){
  
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    modelling_outputs <- modellingServer("modelling_selection", df = df)
    
    clusters <- modelling_outputs$clusters
    model <- modelling_outputs$model
    cluster_model <- modelling_outputs$cluster_model

    output$cluster_plot <- umapServer("umap_clustering", df = df, colour_var = clusters)

    list(clusters = clusters,
         model = model,
         cluster_model = cluster_model)

  })
}






