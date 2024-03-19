#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
modellingUi <- function(id){
  ns <- NS(id)
  
  tagList(
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
    verbatimTextOutput(ns("complete_message"))
    # verbatimTextOutput(ns("selection_method")),
    # verbatimTextOutput(ns("testing"))
  )
}

#' Title
#'
#' @param id 
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
modellingServer <- function(id, df){
  moduleServer(id, function(input, output, server){
    
    min_cluster <- reactive(input$min_cluster_size)
    min_samples <- reactive(input$min_sample_size)
    num_clusters <- reactive(input$n_clusters)
    select_method <- reactive(input$hdbscan_cluster_selection)
    hdb_metric <- reactive(input$hdbscan_metric)
    
    clusterer <- reactive({
      if (input$cluster_method == "HDBSCAN"){
        BertopicR::bt_make_clusterer_hdbscan(min_cluster_size = min_cluster(), min_samples = min_samples(), cluster_selection_method = select_method(), metric = hdb_metric())
      } else if (input$cluster_method == "K-Means"){
        BertopicR::bt_make_clusterer_kmeans(n_clusters = num_clusters())
      }
    })
    
    clusters <- reactive(BertopicR::bt_do_clustering(clusterer(), df$reduced_embeddings))
    
    shiny::observeEvent(min_cluster(), {
      shiny::updateSliderInput(inputId = "min_sample_size", max = min_cluster(), value = min_cluster()*0.5)
    })
    # 
    # model <- shiny::eventReactive(input$do_modelling, {
    #   compiled_model <- bt_compile_model(embedding_model = BertopicR::bt_empty_embedder(),
    #                    reduction_model = BertopicR::bt_empty_reducer(),
    #                    clustering_model = clusterer())
    # })
    
    shiny::observeEvent(input$do_modelling, {
      model <- bt_compile_model(embedding_model = BertopicR::bt_empty_embedder(),
                                         reduction_model = BertopicR::bt_empty_reducer(),
                                         clustering_model = clusterer())
      bt_fit_model(model = model(), documents = df$docs, embeddings = df$reduced_embeddings)
    })
    
    shiny::observeEvent(input$reset_model, {
      model <- bt_compile_model(embedding_model = BertopicR::bt_empty_embedder(),
                                reduction_model = BertopicR::bt_empty_reducer(),
                                clustering_model = clusterer())
    })
    
    # disable inputs
    shiny::observeEvent(input$do_modelling, { 
      elements_to_disable <- c("do_modelling", "min_cluster_size", "min_sample_size", "n_clustsers", 
                               "hdbscan_metric", "hdbscan_cluster_selection", "cluster_method")
      
      purrr::map(elements_to_disable, ~ shinyjs::disable(.x))
    })
    
    shiny::observeEvent(input$reset_model, { 
      elements_to_enable <- c("min_cluster_size", "min_sample_size", "n_clustsers", 
                               "hdbscan_metric", "hdbscan_cluster_selection", "cluster_method")
      
      purrr::map(elements_to_enable, ~ shinyjs::enable(.x))
    })
    
    # model <- shiny::eventReactive(input$reset_model, {
    #   BertopicR::bt_compile_model(embedding_model = BertopicR::bt_empty_embedder(),
    #                                        reduction_model = BertopicR::bt_empty_reducer(),
    #                                        clustering_model = BertopicR::bt_empty_clusterer())
    # })
    
    # reset button ----
    # model <- shiny::eventReactive(input$reset_model, {
    #   rm(model)
    # })

    # output message to show modelling complete
    output$complete_message <- renderPrint({
      if (input$do_modelling) { 
        isolate("Model generated with paramters...")# NEED TO POPULATE THIS
      } else {
        return("No model generated.")
      }
    })
    
    
    list(clusters = clusters, 
         model = model)
    
  })
}

#' Title
#'
#' @param id 
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
modellingServer_ReavtiveValues <- function(id, df){
  moduleServer(id, function(input, output, server){
    
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
    
    clusters <- shiny::reactive(BertopicR::bt_do_clustering(clusterer(), df$reduced_embeddings))
    
    shiny::observeEvent(min_cluster(), {
      shiny::updateSliderInput(inputId = "min_sample_size", max = min_cluster(), value = min_cluster()*0.5)
    })
    # 
    # model <- shiny::eventReactive(input$do_modelling, {
    #   compiled_model <- bt_compile_model(embedding_model = BertopicR::bt_empty_embedder(),
    #                    reduction_model = BertopicR::bt_empty_reducer(),
    #                    clustering_model = clusterer())
    # })
    
    shiny::observeEvent(input$do_modelling, {
      r$model <- BertopicR::bt_compile_model(embedding_model = BertopicR::bt_empty_embedder(),
                                reduction_model = BertopicR::bt_empty_reducer(),
                                clustering_model = clusterer())
      BertopicR::bt_fit_model(model = model(), documents = df$docs, embeddings = df$reduced_embeddings)
    })
    
    shiny::observeEvent(input$reset_model, {
      r$model <- bt_compile_model(embedding_model = BertopicR::bt_empty_embedder(),
                                reduction_model = BertopicR::bt_empty_reducer(),
                                clustering_model = clusterer())
    })
    
    # disable inputs
    shiny::observeEvent(input$do_modelling, { 
      elements_to_disable <- c("do_modelling", "min_cluster_size", "min_sample_size", "n_clustsers", 
                               "hdbscan_metric", "hdbscan_cluster_selection", "cluster_method")
      
      purrr::map(elements_to_disable, ~ shinyjs::disable(.x))
    })
    
    shiny::observeEvent(input$reset_model, { 
      elements_to_enable <- c("min_cluster_size", "min_sample_size", "n_clustsers", 
                              "hdbscan_metric", "hdbscan_cluster_selection", "cluster_method")
      
      purrr::map(elements_to_enable, ~ shinyjs::enable(.x))
    })
    
    # model <- shiny::eventReactive(input$reset_model, {
    #   BertopicR::bt_compile_model(embedding_model = BertopicR::bt_empty_embedder(),
    #                                        reduction_model = BertopicR::bt_empty_reducer(),
    #                                        clustering_model = BertopicR::bt_empty_clusterer())
    # })
    
    # reset button ----
    # model <- shiny::eventReactive(input$reset_model, {
    #   rm(model)
    # })
    
    # output message to show modelling complete
    output$complete_message <- renderPrint({
      if (input$do_modelling) { 
        isolate("Model generated with paramters...")# NEED TO POPULATE THIS
      } else {
        return("No model generated.")
      }
    })
    
    
    list(clusters = clusters, 
         model = r$model)
    
  })
}


