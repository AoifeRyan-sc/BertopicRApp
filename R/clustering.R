#' Ui specs for clustering tab
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
clusteringUi <- function(id) {
  
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
       modellingUi(ns("modelling_selection"))
       ),
    mainPanel(
      # umapUi(ns("umap_clustering"))
      # shiny::plotOutput(ns("cluster_plot"))
      plotly::plotlyOutput(ns("cluster_plot"))
      )
    ) 
  
}

clusteringUi_save <- function(id) {
  
  ns <- NS(id)
  # Sidebar layout
  # tagList(
  sidebarLayout(
    # Sidebar panel with inputs
    sidebarPanel(
      selectInput(ns("cluster_method"), "Clustering Method", choices = c("HDBSCAN", "K-Means")),
      conditionalPanel(
        condition = "input.cluster_method == 'K-Means'", ns = ns,
        numericInput(ns("n_clusters"), "Number of Clusters", value = 10)
      ),
      conditionalPanel(
        condition = "input.cluster_method == 'HDBSCAN'", ns = ns,
        sliderInput(ns("min_cluster_size"), "Minimum cluster size:",
                    # min = 2, max = nrow(df)/2, value = 20
                    min = 2, max = 20, value = 20), # arbitrarily setting the defaults
        sliderInput(ns("min_sample_size"), "Minimum number of samples:",
                    min = 1, max = 10, value = 1), # these values update as defined in the server
        # radioButtons("hdbscan_metric", "Metric", choices = c("cosine", "euclidean")),
        selectInput(ns("hdbscan_metric"), "Metric", choices = c(
          # "cosine",
          "braycurtis", "canberra", "chebyshev", "cityblock", "correlation",  "dice", "euclidean", "hamming", "jaccard", "jensenshannon", "kulczynski1", "mahalanobis", "matching", "minkowski", "rogerstanimoto", "russellrao", "seuclidean", "sokalmichener", "sokalsneath", "sqeuclidean", "yule")),
        radioButtons(ns("hdbscan_cluster_selection"), "Cluster Selection Method", choices = c("eom", "leaf"))
      ),
      actionButton(ns("do_modelling"), "Model", class = "btn-succes"),
      actionButton(ns("reset_model"), "Reset", classs = "btn-danger"),
      verbatimTextOutput(ns("complete_message")),
      verbatimTextOutput(ns("selection_method")),
      verbatimTextOutput(ns("testing"))
    ),
    
    # Main panel with the plot
    mainPanel(
      umapUi(ns("umap_clustering"))
    )
  ) # sidebarLayout
  # )
  
}

#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
clusteringServer <- function(id, df = df){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    modelling_outputs <- modellingServer_ReavtiveValues("modelling_selection", df = df)
    
    clusters <- modelling_outputs$clusters
    model <- modelling_outputs$model
    
    output$cluster_plot <- umapServer("umap_clustering", df = df, colour_var = clusters)
    
    list(clusters = clusters,
         model = model)

  })
}






