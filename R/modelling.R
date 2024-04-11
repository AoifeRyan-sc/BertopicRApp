#' Modelling UI Features Specs
#'
#' @param id parameter for shiny identification
#'
#' @noRd
#' 
modellingUi <- function(id){
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::selectInput(ns("cluster_method"), "Clustering Method", choices = c("HDBSCAN", "K-Means")),
    shiny::conditionalPanel(
      condition = "input.cluster_method == 'K-Means'", ns = ns,
      shiny::numericInput(ns("n_clusters"), "Number of Clusters", value = 10)
    ),
    shiny::conditionalPanel(
      condition = "input.cluster_method == 'HDBSCAN'", ns = ns,
      shiny::sliderInput(ns("min_cluster_size"), "Minimum cluster size:",
                  min = 2, max = 20, value = 20), # arbitrarily setting the defaults
      shiny::sliderInput(ns("min_sample_size"), "Minimum number of samples:",
                  min = 1, max = 10, value = 1), # these values update as defined in the server
      shiny::selectInput(ns("hdbscan_metric"), "Clustering Metric", choices = c(
        # "cosine", - Not supported by sklearn
        "euclidean", "braycurtis", "canberra", "chebyshev", "cityblock", "correlation",  "dice", "hamming", "jaccard", "jensenshannon", "kulczynski1", "mahalanobis", "matching", "minkowski", "rogerstanimoto", "russellrao", "seuclidean", "sokalmichener", "sokalsneath", "sqeuclidean", "yule")),
      shiny::radioButtons(ns("hdbscan_cluster_selection"), "Cluster Selection Method", choices = c("eom", "leaf"))
    ),
    shiny::actionButton(ns("do_modelling"), "Model", class = "btn-succes"),
    shiny::actionButton(ns("reset_model"), "Reset", classs = "btn-danger"),
    shiny::verbatimTextOutput(ns("complete_message"))
  )
}

#' Modelling UI Features Server Function
#'
#' @param id parameter for shiny identification
#' @param df reactive dataframe containing docs and embedding info 
#'
#' @noRd
#' 
modellingServer <- function(id, df, reduced_embeddings){ # do I need df?

  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    r <- shiny::reactiveValues(model = NULL)
    
    min_cluster <- shiny::reactive(input$min_cluster_size)
    min_samples <- shiny::reactive(input$min_sample_size)
    num_clusters <- shiny::reactive(input$n_clusters)
    select_method <- shiny::reactive(input$hdbscan_cluster_selection)
    hdb_metric <- shiny::reactive(input$hdbscan_metric)

    clusterer <- shiny::reactive({
      if (input$cluster_method == "HDBSCAN"){
        BertopicR::bt_make_clusterer_hdbscan(min_cluster_size = min_cluster(), min_samples = min_samples(), cluster_selection_method = select_method(), metric = hdb_metric())
      } else if (input$cluster_method == "K-Means"){
        BertopicR::bt_make_clusterer_kmeans(n_clusters = num_clusters())
      }
    })
    
    # I NEED TO REMOVE THIS
    reduced_embeddings <- reactive({
      df()$reduced_embeddings
    })

    clusters <- shiny::reactive({
      BertopicR::bt_do_clustering(clusterer(), reduced_embeddings())
      })

    shiny::observeEvent(min_cluster(), {
      shiny::updateSliderInput(inputId = "min_sample_size", max = min_cluster(), value = min_cluster()*0.5)
    }) # updating slider range

    shiny::observeEvent(input$do_modelling, {
      r$model <- BertopicR::bt_compile_model(embedding_model = BertopicR::bt_empty_embedder(),
                                reduction_model = BertopicR::bt_empty_reducer(),
                                clustering_model = clusterer())
      BertopicR::bt_fit_model(model = r$model, documents = df()$docs, embeddings = reduced_embeddings())
    }) # model

shiny::observeEvent(input$reset_model, {
  r$model <- NULL
}) # remove model on reset

shiny::observeEvent(input$do_modelling, {
  elements_to_disable <- c("do_modelling", "min_cluster_size", "min_sample_size", "n_clusters",
                           "hdbscan_metric", "hdbscan_cluster_selection", "cluster_method")

  purrr::map(elements_to_disable, ~ shinyjs::disable(.x))
}) # disable buttons

shiny::observeEvent(input$reset_model, {
  elements_to_enable <- c("do_modelling", "min_cluster_size", "min_sample_size", "n_clustsers",
                          "hdbscan_metric", "hdbscan_cluster_selection", "cluster_method")

  purrr::map(elements_to_enable, ~ shinyjs::enable(.x))
}) # enable buttons on reset

output$complete_message <- shiny::renderPrint({
  if (input$do_modelling) {
    shiny::isolate("Model generated with paramters...need to complete this")# NEED TO POPULATE THIS
  } else {
    return("No model generated.")
  }
})

model <- shiny::reactive({
  r$model
})

cluster_model <- shiny::reactive({
  input$cluster_method
})

    list(clusters = clusters,
         model = model,
         cluster_model = cluster_model)

  })
}

