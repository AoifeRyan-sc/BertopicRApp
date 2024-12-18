#' Ui specs for clustering tab
#'
#' @param id parameter for shiny identification
#' 
#'
#' @noRd
#' 
clusteringUi <- function(id) {

ns <- shiny::NS(id)

shiny::tagList(
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      # modellingUi(ns("modelling_selection"))
      shiny::selectInput(ns("cluster_method"), "Clustering Method", choices = c("HDBSCAN", "K-Means")),
      shiny::conditionalPanel(
        condition = "input.cluster_method == 'K-Means'", ns = ns,
        shiny::numericInput(ns("n_clusters"), "Number of Clusters", value = 10)
      ),
      shiny::conditionalPanel(
        condition = "input.cluster_method == 'HDBSCAN'", ns = ns,
        shiny::div(
          style = "position: relative;",
          shiny::sliderInput(ns("min_cluster_size"), "Minimum cluster size:",
                             min = 2, max = 20, value = 20, round = TRUE
          ), 
          shiny::div(
            style = "position: absolute; top: 0; right: 5px; transform: translateX(-20%);",
            bslib::tooltip(
              bsicons::bs_icon("question-circle-fill"),
              "Dictates the minimum allowable posts per cluster."
            )
          )
        ),
        shiny::div(
          style = "position: relative;",
          shiny::sliderInput(ns("min_sample_size"), "Minimum number of samples:",
                             min = 1, max = 20, value = 20), # these values update as defined in the server
          shiny::div(
            style = "position: absolute; top: 0; right: 5px; transform: translateX(-20%);",
            bslib::tooltip(
              bsicons::bs_icon("question-circle-fill"),
              htmltools::span(
                "Higher min_samples = more conservative clustering and more outliers, and vice versa for lower min_samples. 
                  You can find more information about hdbscan clustering and min samples ",
                tags$a(href = "https://hdbscan.readthedocs.io/en/latest/parameter_selection.html#selecting-min-samples", "here"),
                "."
              )
            )
          )
        ),
        shiny::selectInput(ns("hdbscan_metric"), "Clustering Metric", choices = c(
          "euclidean", "braycurtis", "canberra", "chebyshev", "cityblock", "correlation",  "dice", "hamming", "jaccard", "jensenshannon", "kulczynski1", "mahalanobis", "matching", "minkowski", "rogerstanimoto", "russellrao", "seuclidean", "sokalmichener", "sokalsneath", "sqeuclidean", "yule")),
        shiny::div(
          style = "position: relative;",
          shiny::radioButtons(ns("hdbscan_cluster_selection"), "Cluster Selection Method", choices = c("eom", "leaf")),
          shiny::div(
            style = "position: absolute; top: 0; right: 5px; transform: translateX(-20%);",
            bslib::tooltip(
              bsicons::bs_icon("question-circle-fill"),
              htmltools::span(
                "Leaf tends to give smaller, more evenly sized clusters, where excess of mass tends to pick a few larger clusters. you can find some more info ",
                tags$a(href = "https://hdbscan.readthedocs.io/en/latest/parameter_selection.html", "here"),
                "."
              )
            )
          )
        ),
      ),
      shiny::actionButton(ns("do_modelling"), "Model", class = "btn-succes"),
      shiny::actionButton(ns("reset_model"), "Reset", classs = "btn-danger"),
      shinycssloaders::withSpinner(shiny::uiOutput(ns("complete_message")))

    ),
  shiny::mainPanel(
   # clusteringMainPanelUi(ns("clustering_main_panel"))
    shiny::uiOutput(ns("data_display")),
    DT::dataTableOutput(ns("selected_data_df")),
    shiny::downloadButton(ns("data_download_clustering"), label = "Download Data Table")
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

# clusteringServer <- function(id, df = df){
clusteringServer <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    shiny::observe({
      r$min_cluster <- input$min_cluster_size
      r$min_samples <- input$min_sample_size
      r$num_clusters <- input$n_clusters
      r$select_method <- input$hdbscan_cluster_selection
      r$hdb_metric <- input$hdbscan_metric
      r$cluster_model <- input$cluster_method
    })
    
    clusterer <- shiny::reactive({
      if (r$cluster_model == "HDBSCAN"){
        BertopicR::bt_make_clusterer_hdbscan(min_cluster_size = r$min_cluster, min_samples = r$min_samples, cluster_selection_method = r$select_method, metric = r$hdb_metric)
      } else if (r$cluster_model == "K-Means"){
        BertopicR::bt_make_clusterer_kmeans(n_clusters = r$num_clusters)
      }
    })
    
    r$clusters <- shiny::reactive({
      req(is.array(r$reduced_embeddings) | is.data.frame(r$reduced_embeddings))
      BertopicR::bt_do_clustering(clusterer(), r$reduced_embeddings)
    })
    
    
    output$data_display <- renderUI({ # idk why I can't get the conditional panels working with the namespacing
      if (is.data.frame(r$df) & is.null(r$reduced_embeddings)){
        DT::dataTableOutput((ns("uploaded_data")))
      } else if (!is.null(r$reduced_embeddings)){
        shinycssloaders::withSpinner(plotly::plotlyOutput(ns("cluster_plot")))
      }
    })
    
    output$uploaded_data <- DT::renderDataTable({
      req(is.data.frame(r$df))
      printdf <- r$df %>% dplyr::select(-embeddings)
      printdf
    })
    
    output$cluster_plot <- plotly::renderPlotly({
      shiny::req(is.array(r$reduced_embeddings2d) | is.data.frame(r$reduced_embeddings2d))
      r$df$v1 <- r$reduced_embeddings2d[,1]
      r$df$v2 <- r$reduced_embeddings2d[,2]
      createUmap("umap_clustering", df = r$df, colour_var = r$clusters(),
                 title = "UMAP of document embeddings: Cluster investigation") # can remove the id here
      
    })
    
    display_data <- shiny::reactive({
      shiny::req(is.array(r$reduced_embeddings) | is.data.frame(r$reduced_embeddings))
      selected <- plotly::event_data(event = "plotly_selected", source = "umap_clustering")
      r$df[r$df$rowid %in% selected$customdata, ] %>%
        dplyr::select(-any_of(c("rowid", "embeddings", "v1", "v2"))) %>%
        dplyr::relocate("docs", "topics")
      
    })
    
    output$selected_data_df <- DT::renderDataTable({
      display_data()
    })
    
    output$data_download_clustering <- shiny::downloadHandler(
      filename = title_file("clustering_data", ".csv"),
      content = function(file) {
        utils::write.csv(display_data(), file, row.names = FALSE)
      }
    )
    
        shiny::observeEvent(r$min_cluster, { 
          shiny::updateSliderInput(inputId = "min_sample_size", max = r$min_cluster, value = r$min_cluster)
        }) # updating slider range
      

        shiny::observeEvent(r$df, {
          max_val <- ceiling(nrow(r$df)/2)
          min_val <- 2
          # range <- max_val - min_val
          shiny::updateSliderInput(inputId = "min_cluster_size", max = max_val,
                                   step = 1
                                   # step = ceiling(range/5)
                                   )
        }) # updating slider range

        shiny::observeEvent(input$do_modelling, {
          r$model <- BertopicR::bt_compile_model(embedding_model = BertopicR::bt_empty_embedder(),
                                    reduction_model = BertopicR::bt_empty_reducer(),
                                    clustering_model = clusterer())
          BertopicR::bt_fit_model(model = r$model, documents = r$df$docs, embeddings = r$reduced_embeddings)
          r$df$topics <- r$model$topics_
        }) # model

    shiny::observeEvent(input$reset_model, {
      r$model <- NULL
    }) # remove model on reset
    
    elements_to_turn_on_off <- c("do_modelling", "min_cluster_size", "min_sample_size", "n_clusters",
                             "hdbscan_metric", "hdbscan_cluster_selection", "cluster_method")
    
    shiny::observeEvent(input$do_modelling, {
      purrr::map(elements_to_turn_on_off, ~ shinyjs::disable(.x))
    }) # disable buttons

    shiny::observeEvent(input$reset_model, {
      purrr::map(elements_to_turn_on_off, ~ shinyjs::enable(.x))
    }) # enable buttons on reset

    output$complete_message <- shiny::renderUI({
      if (!is.null(r$model)){
        htmltools::tagList(
          htmltools::tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
          ),
          htmltools::div(
            class = "reduced-embeddings",
            span(class = "check-emoji", "✅"),
            span(class = "reducing-text", "Modelling Complete!")
          ))
      } 
    })


  })
}






