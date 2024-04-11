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
      # shiny::conditionalPanel(
        # condition = "input.reducing_clustering_panels == 1", ns = ns,
        # reducingUi(ns("reduction_ui"))
      # ),
      shiny::conditionalPanel(
        condition = "input.reducing_clustering_panels == 2", ns = ns,
        modellingUi(ns("modelling_selection"))
      )
       ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "Reducing",
          plotly::plotlyOutput(ns("reduce_plot")),
          value = 1
        ),
        shiny::tabPanel(
          "Clustering",
          plotly::plotlyOutput(ns("cluster_plot")),
          value = 2
          ),
        id = ns("reducing_clustering_panels")
        ) 
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
    
    # reduced_embeddings <- reducingServer("reduction_ui", df = df)
    # output$reduced_embeddings_sample <- renderPrint({
    #   reduced_embeddings()
    # })
    
    reduced_embeddings <- reactive({reduced_embeddings})
    
    modelling_outputs <- modellingServer("modelling_selection", df = df, reduced_embeddings = reduced_embeddings)
    
    clusters <- modelling_outputs$clusters
    model <- modelling_outputs$model
    cluster_model <- modelling_outputs$cluster_model
    pseudo_cluster <- shiny::reactive({rep(1, nrow(df()))})

    output$cluster_plot <- umapServer("umap_clustering", df = df, colour_var = clusters)
    output$reduce_plot <- umapServer("umap_reducing", df = df, colour_var = pseudo_cluster)

    list(clusters = clusters,
         model = model,
         cluster_model = cluster_model)

  })
}






